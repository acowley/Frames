{-# LANGUAGE BangPatterns,
             ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             ScopedTypeVariables,
             TypeOperators,
             UndecidableInstances #-}
-- | Co-records: a flexible approach to sum types.
module Frames.CoRec where
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor (Compose(..), (:.), Identity(..))
import Data.Vinyl.TypeLevel (RIndex)
import Frames.RecF (reifyDict)
import Frames.TypeLevel (LAll, HasInstances, AllHave)
import GHC.Prim (Constraint)

-- | Generalize algebraic sum types.
data CoRec :: (* -> *) -> [*] -> * where
  Col :: RElem a ts (RIndex a ts) => !(f a) -> CoRec f ts

-- | A Field of a 'Record' is a 'CoRec Identity'.
type Field = CoRec Identity

-- | Helper to build a 'Show'-able 'CoRec'
col :: (Show a, a ∈ ts) => a -> CoRec (Dict Show) ts
col = Col . Dict

instance Show (CoRec (Dict Show) ts) where
  show (Col (Dict x)) = "Col "++show x

-- | A function type constructor that takes its arguments in the
-- reverse order.
newtype Op b a = Op { runOp :: a -> b }

instance forall ts. (LAll Show ts, RecApplicative ts)
  => Show (CoRec Identity ts) where
  show (Col (Identity x)) = "(Col "++show' x++")"
    where shower :: Rec (Op String) ts
          shower = reifyDict (Proxy::Proxy Show) (Op show)
          show' = runOp (rget Proxy shower)

-- | Remove a 'Dict' wrapper from a value.
dictId :: Dict c a -> Identity a
dictId (Dict x) = Identity x

-- | Helper to build a @Dict Show@
showDict :: Show a => a -> Dict Show a
showDict = Dict
  
-- | We can inject a a 'CoRec' into a 'Rec' where every field of the
-- 'Rec' is 'Nothing' except for the one whose type corresponds to the
-- type of the given 'CoRec' variant.
corecToRec :: RecApplicative ts => CoRec f ts -> Rec (Maybe :. f) ts
corecToRec (Col x) = rput (Compose $ Just x) (rpure (Compose Nothing))

-- | Shorthand for applying 'corecToRec' with common functors.
corecToRec' :: RecApplicative ts => CoRec Identity ts -> Rec Maybe ts
corecToRec' = rmap (fmap getIdentity . getCompose) . corecToRec

-- | Fold a field selection function over a 'Rec'.
class FoldRec ss ts where
  foldRec :: (CoRec f ss -> CoRec f ss -> CoRec f ss)
          -> CoRec f ss
          -> Rec f ts
          -> CoRec f ss

instance FoldRec ss '[] where foldRec _ z _ = z

instance (t ∈ ss, FoldRec ss ts) => FoldRec ss (t ': ts) where
  foldRec f z (x :& xs) = foldRec f (f z (Col x)) xs

-- | Apply a natural transformation to a variant.
corecMap :: (forall x. f x -> g x) -> CoRec f ts -> CoRec g ts
corecMap nt (Col x) = Col (nt x)

-- | This can be used to pull effects out of a 'CoRec'.
corecTraverse :: Functor h
              => (forall x. f x -> h (g x)) -> CoRec f ts -> h (CoRec g ts)
corecTraverse f (Col x) = fmap Col (f x)

-- | Fold a field selection function over a non-empty 'Rec'.
foldRec1 :: FoldRec (t ': ts) ts
         => (CoRec f (t ': ts) -> CoRec f (t ': ts) -> CoRec f (t ': ts))
         -> Rec f (t ': ts)
         -> CoRec f (t ': ts)
foldRec1 f (x :& xs) = foldRec f (Col x) xs

-- | Similar to 'Data.Monoid.First': find the first field that is not
-- 'Nothing'.
firstField :: FoldRec ts ts
           => Rec (Maybe :. f) ts -> Maybe (CoRec f ts)
firstField RNil = Nothing
firstField v@(x :& _) = corecTraverse getCompose $ foldRec aux (Col x) v
  where aux :: CoRec (Maybe :. f) (t ': ts)
            -> CoRec (Maybe :. f) (t ': ts)
            -> CoRec (Maybe :. f) (t ': ts)
        aux c@(Col (Compose (Just _))) _ =  c
        aux _ c = c

-- | Similar to 'Data.Monoid.Last': find the last field that is not
-- 'Nothing'.
lastField :: FoldRec ts ts
          => Rec (Maybe :. f) ts -> Maybe (CoRec f ts)
lastField RNil = Nothing
lastField v@(x :& _) = corecTraverse getCompose $ foldRec aux (Col x) v
  where aux :: CoRec (Maybe :. f) (t ': ts)
            -> CoRec (Maybe :. f) (t ': ts)
            -> CoRec (Maybe :. f) (t ': ts)
        aux _ c@(Col (Compose (Just _))) = c
        aux c _ = c

-- | Apply a type class method on a 'CoRec'. The first argument is a
-- 'Proxy' value for a /list/ of 'Constraint' constructors. For
-- example, @onCoRec [pr|Num,Ord|] (> 20) r@. If only one constraint
-- is needed, use the @pr1@ quasiquoter.
onCoRec :: forall (cs :: [* -> Constraint]) f ts b.
           (AllHave cs ts, Functor f, RecApplicative ts)
        => Proxy cs
        -> (forall a. HasInstances a cs => a -> b)
        -> CoRec f ts -> f b
onCoRec p f (Col x) = fmap meth x
  where meth = runOp $
               rget Proxy (reifyDicts p (Op f) :: Rec (Op b) ts)

-- | Apply a type class method on a 'Field'. The first argument is a
-- 'Proxy' value for a /list/ of 'Constraint' constructors. For
-- example, @onCoRec [pr|Num,Ord|] (> 20) r@. If only one constraint
-- is needed, use the @pr1@ quasiquoter.
onField :: forall cs ts b.
           (AllHave cs ts, RecApplicative ts)
        => Proxy cs
        -> (forall a. HasInstances a cs => a -> b)
        -> Field ts -> b
onField p f x = getIdentity (onCoRec p f x)

-- | Build a record whose elements are derived solely from a
-- list of constraint constructors satisfied by each.
reifyDicts :: forall cs f proxy ts. (AllHave cs ts, RecApplicative ts)
           => proxy cs -> (forall a. HasInstances a cs => f a) -> Rec f ts
reifyDicts _ f = go (rpure Nothing)
  where go :: AllHave cs ts' => Rec Maybe ts' -> Rec f ts'
        go RNil = RNil
        go (_ :& xs) = f :& go xs
