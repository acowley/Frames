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
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Functor.Contravariant.Divisible

dropCols :: 
  (rs' ~ RDeleteAll fs rs
  , rs' ⊆ rs) =>
  proxy fs -> Record rs -> Record rs'
dropCols _ rs = rcast rs

mergeRec :: 
  (fs ⊆ rs2
  , rs2' ⊆ rs2
  , rs2' ~ RDeleteAll fs rs2
  , rs ⊆ (rs ++ rs2')) =>
  proxy fs ->
  Record rs ->
  Record rs2 ->
  Record (rs ++ rs2')
mergeRec cols rec1 rec2 =
  rec1 <+> rec2'
  where
    rec2' = dropCols cols rec2 

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

-- | Perform an inner join operation on two frames
-- matching on 
inner_join :: forall proxy fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
    proxy fs -- ^ A quasiquoter with shared columns to join on
             -- usually generated with pr or pr1.
  -> Frame (Record rs)  -- ^ The left frame 
  -> Frame (Record rs2) -- ^ The right frame
  -> Frame (Record (rs ++ rs2')) -- ^ The joined frame
    
inner_join cols a b =
    toFrame $
    foldr (++) [] 
    (inner grouping mergeFun proj1 proj2 (toList a) (toList b))
    where
      mergeFun l r = mergeRec cols l r
      proj1 x = rcast x :: Record fs
      proj2 y = rcast y :: Record fs


justsFromRec :: Record fs -> Rec Maybe fs
justsFromRec rs = rmap (\x -> Just (getIdentity x)) rs

nothingsFromRec :: Record fs -> Rec Maybe fs
nothingsFromRec rs = rmap (\_ -> Nothing) rs

-- | Perform an outer join operation on two frames
-- matching on a quasiquoter of column names
outer_join :: forall proxy fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
    proxy fs -- ^ A quasiquoter with shared columns to join on,
             --  usually generated with pr or pr1 
    -> Frame (Record rs)  -- ^ The left frame 
    -> Frame (Record rs2) -- ^ The right frame
    -> [(Rec Maybe (rs ++ rs2'))] -- ^ A list of the merged records, now in the Maybe functor
    
outer_join cols a b =
  let
    as = toList a
    bs = toList b
  in
    let
      proj1 x = rcast x :: Record fs
      proj2 y = rcast y :: Record fs
      mergeFun l r = justsFromRec $ mergeRec cols l r
      mergeLeftEmpty l = (justsFromRec l) <+> (nothingsFromRec (dropCols cols (head bs)))
      mergeRightEmpty r = (nothingsFromRec (head as)) <+> (justsFromRec (dropCols cols r))
    in  
      foldr' (++) [] 
      (outer grouping mergeFun mergeLeftEmpty mergeRightEmpty
       proj1 proj2 (toList a) (toList b))

    
-- | Perform an outer join operation on two frames
-- matching on a quasiquoter of column names
right_join :: forall proxy fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
    proxy fs -- ^ A quasiquoter with shared columns to join on,
             --   usually generated with pr or pr1
  -> Frame (Record rs)  -- ^ The left frame 
  -> Frame (Record rs2) -- ^ The right frame
  -> [(Rec Maybe (rs ++ rs2'))] -- ^ A list of the merged records, now in the Maybe functor
    
right_join cols a b =
  let
    as = toList a
  in
    let
      proj1 x = rcast x :: Record fs
      proj2 y = rcast y :: Record fs
      mergeFun l r = justsFromRec $ mergeRec cols l r
      mergeRightEmpty r = (nothingsFromRec (head as)) <+> (justsFromRec (dropCols cols r))
    in  
      foldr' (++) [] 
      (rightOuter grouping mergeFun mergeRightEmpty
       proj1 proj2 (toList a) (toList b))

-- | Perform an outer join operation on two frames
-- matching on a quasiquoter of column names
left_join :: forall proxy fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
    proxy fs -- ^ A quasiquoter with shared columns to join on,
             -- usually generated with pr or pr1 
  -> Frame (Record rs)  -- ^ The left frame 
  -> Frame (Record rs2) -- ^ The right frame
  -> [(Rec Maybe (rs ++ rs2'))] -- ^ A list of the merged records, now in the Maybe functor
    
left_join cols a b =
  let
    bs = toList b
  in
    let
      proj1 x = rcast x :: Record fs
      proj2 y = rcast y :: Record fs
      mergeFun l r = justsFromRec $ mergeRec cols l r
      mergeLeftEmpty l = (justsFromRec l) <+> (nothingsFromRec (dropCols cols (head bs)))
    in  
      foldr' (++) [] 
      (leftOuter grouping mergeFun mergeLeftEmpty 
       proj1 proj2 (toList a) (toList b))
