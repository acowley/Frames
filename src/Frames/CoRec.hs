{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             KindSignatures, MultiParamTypeClasses, RankNTypes,
             TypeOperators, UndecidableInstances #-}
-- | Co-records: a flexible approach to sum types.
module Frames.CoRec where
import Data.Vinyl
import Data.Vinyl.Functor (Compose(..), (:.), Identity(..))
import Data.Vinyl.TypeLevel (RIndex)

-- | Generalize algebraic sum type.
data CoRec :: (* -> *) -> [*] -> * where
  Col :: RElem a ts (RIndex a ts) => f a -> CoRec f ts

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
firstField :: forall ts f. FoldRec ts ts
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
lastField :: forall ts f. FoldRec ts ts
          => Rec (Maybe :. f) ts -> Maybe (CoRec f ts)
lastField RNil = Nothing
lastField v@(x :& _) = corecTraverse getCompose $ foldRec aux (Col x) v
  where aux :: CoRec (Maybe :. f) (t ': ts)
            -> CoRec (Maybe :. f) (t ': ts)
            -> CoRec (Maybe :. f) (t ': ts)
        aux _ c@(Col (Compose (Just _))) = c
        aux c _ = c
