{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, MultiParamTypeClasses, PolyKinds,
             ScopedTypeVariables, TypeFamilies, TypeOperators, 
             UndecidableInstances, TemplateHaskell, QuasiQuotes,
             Rank2Types, TypeApplications, AllowAmbiguousTypes #-}
module Frames.Joins (innerJoin
                    , outerJoin
                    , leftJoin
                    , rightJoin)
  
where
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
import qualified Data.Text as T


mergeRec :: forall fs rs rs2 rs2'.
  (fs ⊆ rs2
  , rs2' ⊆ rs2
  , rs2' ~ RDeleteAll fs rs2
  , rs ⊆ (rs ++ rs2')) =>
  Record rs ->
  Record rs2 ->
  Record (rs ++ rs2')
mergeRec rec1 rec2 =
  rec1 <+> rec2'
  where
    rec2' = rcast @rs2' rec2 

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

instance Grouping Text where
  grouping = contramap T.unpack grouping

-- | Perform an inner join operation on two frames
innerJoin :: forall fs rs rs2 rs2'.
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
  Frame (Record rs)  -- ^ The left frame 
  -> Frame (Record rs2) -- ^ The right frame
  -> Frame (Record (rs ++ rs2')) -- ^ The joined frame
    
innerJoin a b =
    toFrame $
    concat
    (inner grouping mergeFun proj1 proj2 (toList a) (toList b))
    where
      mergeFun = mergeRec @fs
      proj1 = rcast @fs 
      proj2 = rcast @fs


justsFromRec :: Record fs -> Rec Maybe fs
justsFromRec = rmap (Just . getIdentity)

mkNothingsRec :: forall fs.
  (RecApplicative fs) =>
  Rec Maybe fs
mkNothingsRec = rpure @fs Nothing

-- | Perform an outer join operation on two frames
outerJoin :: forall fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , RecApplicative rs2'
    , RecApplicative rs
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
  Frame (Record rs)  -- ^ The left frame 
  -> Frame (Record rs2) -- ^ The right frame
  -> [Rec Maybe (rs ++ rs2')] -- ^ A list of the merged records, now in the Maybe functor
    
outerJoin a b =
  concat
  (outer grouping mergeFun mergeLeftEmpty mergeRightEmpty
    proj1 proj2 (toList a) (toList b))
  where
    proj1 = rcast @fs
    proj2 = rcast @fs
    mergeFun l r = justsFromRec $ mergeRec @fs l r 
    mergeLeftEmpty l = justsFromRec l <+> mkNothingsRec @rs2'
    mergeRightEmpty r = mkNothingsRec @rs <+> justsFromRec (rcast @rs2' r)
     
      
    
-- | Perform an outer join operation on two frames
rightJoin :: forall fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , RecApplicative rs
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
  Frame (Record rs)  -- ^ The left frame 
  -> Frame (Record rs2) -- ^ The right frame
  -> [Rec Maybe (rs ++ rs2')] -- ^ A list of the merged records, now in the Maybe functor
    
rightJoin a b =
  concat
  (rightOuter grouping mergeFun mergeRightEmpty
    proj1 proj2 (toList a) (toList b))
  where
    proj1 = rcast @fs
    proj2 = rcast @fs
    mergeFun l r = justsFromRec $ mergeRec @fs l r
    mergeRightEmpty r = mkNothingsRec @rs <+> justsFromRec (rcast @rs2' r)  

-- | Perform an outer join operation on two frames
leftJoin :: forall fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2 
    , rs2' ~ RDeleteAll fs rs2
    , RecApplicative rs2'
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
  Frame (Record rs)  -- ^ The left frame 
  -> Frame (Record rs2) -- ^ The right frame
  -> [Rec Maybe (rs ++ rs2')] -- ^ A list of the merged records, now in the Maybe functor
    
leftJoin a b =
  concat
  (leftOuter grouping mergeFun mergeLeftEmpty 
    proj1 proj2 (toList a) (toList b))
  where
    proj1 = rcast @fs
    proj2 = rcast @fs
    mergeFun l r = justsFromRec $ mergeRec @fs l r
    mergeLeftEmpty l = justsFromRec l <+> mkNothingsRec @rs2'
    
