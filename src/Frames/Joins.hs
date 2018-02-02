{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, MultiParamTypeClasses, PolyKinds,
             ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances, TemplateHaskell, QuasiQuotes,
             Rank2Types #-}
module Frames.Joins where
import Data.Discrimination
import Data.Functor.Contravariant
import Data.Foldable as F
import Frames
import Frames.Melt (Elem(..), Or(..), Not(..), ElemOf(..)
                   , RDeleteAll(..), Disjoint(..))
import Frames.InCore (RecVec(..))
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor
import GHC.Exts (Constraint)
import Data.Functor.Contravariant.Generic 
import GHC.Generics (Generic(..))
import Data.Functor.Contravariant.Divisible
import Data.Word

mergeRec :: forall proxy fs rs rs2  rs2'.
  (fs    ⊆ rs2
  , rs2' ⊆ rs2 
  , rs2' ~ RDeleteAll fs rs2
  ) =>
  proxy fs ->
  Record rs ->
  Record rs2 ->
  Record (rs ++ rs2')
mergeRec _ rec1 rec2 =
  rec1 <+> rec2'
  where
    rec2' = rcast rec2 :: Record rs2'


instance (AllCols Grouping rs
         , Grouping (Record rs)
         , Grouping (s :-> r)
         , Grouping r
         ) =>
         Grouping (Record ((s :-> r) : rs)) where
  grouping = divide (\(a :& as) -> (a, as)) grouping grouping

instance Grouping (Record '[]) where
 grouping = contramap (\_ -> (1 :: Word64)) grouping

instance (Grouping a) => Grouping (s :-> a) where
  grouping = contramap getCol grouping

instance (Grouping a) => Grouping (Identity a) where
  grouping = contramap getIdentity grouping
--instance (Grouping a, Data.Vinyl.Functor) => Grouping (f a)
--instance (Generic a) => Generic (s :-> a)

--instance Record (Grouping rs) =>
--  Grouping (Record rs)

inner_join :: forall proxy fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>  
    proxy fs ->
    Frame (Record rs) ->
    Frame (Record rs2) ->
    [(Record (rs ++ rs2'))]
    
inner_join cols a b =
                --toFrame $
    foldl (++) []
    (inner grouping mergeFun proj1 proj2 (toList a) (toList b))
    where
      mergeFun l r = mergeRec cols l r
      proj1 x = rcast x :: Record fs
      proj2 y = rcast y :: Record fs
    
        


tableTypes "FL2" "/home/chammill/Documents/2018-01-05_ghcBug/FL6.csv"
tableTypes "FL3" "/home/chammill/Documents/2018-01-05_ghcBug/FL7.csv"

fl2 :: IO (Frame FL2)
fl2 = inCoreAoS (readTable "/home/chammill/Documents/2018-01-05_ghcBug/FL6.csv")

fl3 :: IO (Frame FL3)
fl3 = inCoreAoS (readTable "/home/chammill/Documents/2018-01-05_ghcBug/FL7.csv")

-- main :: IO ()
-- main =
--   do
--     f2 <- fl2
--     f3 <- fl3
--     print $ mergeRec [pr1|PolicyID|] (head $ toList f2) (head $ toList f3)
