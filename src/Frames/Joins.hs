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
import Frames.Melt (RDeleteAll)
import Frames.InCore (RecVec)
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor
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
  grouping = divide recUncons grouping grouping

instance Grouping (Record '[]) where
  grouping = conquer

instance (Grouping a) => Grouping (s :-> a) where
   grouping = contramap getCol grouping

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
    Frame (Record (rs ++ rs2'))
    
inner_join cols a b =
    toFrame $
    foldr (++) [] 
    (inner grouping mergeFun proj1 proj2 (toList a) (toList b))
    where
      mergeFun l r = mergeRec cols l r
      proj1 x = rcast x :: Record fs
      proj2 y = rcast y :: Record fs
    
        
