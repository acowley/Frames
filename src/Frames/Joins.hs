{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, MultiParamTypeClasses, PolyKinds,
             ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances, TemplateHaskell, QuasiQuotes,
             Rank2Types, TypeApplications, AllowAmbiguousTypes,
             DeriveAnyClass #-}

-- | Functions for performing SQL style table joins on
-- @Frame@ objects. Uses Data.Discrimination under the hood
-- for O(n) joins. These have behaviour equivalent to
-- @INNER JOIN@, @FULL JOIN@, @LEFT JOIN@, and @RIGHT JOIN@ from
-- SQL.
module Frames.Joins (innerJoin
                    , outerJoin
                    , leftJoin
                    , rightJoin)

where
import Data.Discrimination (outer, leftOuter, Grouping(..), inner, rightOuter)
import Data.Foldable as F
import Frames.Frame
import Frames.Rec
import Frames.InCore (toFrame)
import Frames.Melt (RDeleteAll)
import Frames.InCore (RecVec)
import Data.Vinyl.TypeLevel
import Data.Vinyl
import Data.Vinyl.Functor
import Frames.ShowCSV
import Frames.Col ((:->))
-- ((:->))

data MergeStatus = MergeFromLeft | MergeFromRight | MergeBoth deriving (Show, ShowCSV)

type MergeStatusField = "mergeStatus" :-> MergeStatus

-- type AbsTime2 = "absTime2" :-> Text  -- :: (Symbol, *)

mergeRec :: forall fs rs rs2 rs2'.
  (fs ⊆ rs2
  , rs2' ⊆ rs2
  , rs2' ~ RDeleteAll fs rs2
  , rs ⊆ (rs ++ rs2')) =>
  Record rs ->
  Record rs2 ->
  Record (rs ++ rs2')
{-# INLINE mergeRec #-}
mergeRec rec1 rec2 =
  rec1 <+> rec2'
  where
    rec2' = rcast @rs2' rec2


-- | Perform an inner join operation on two frames.
--
-- Requires the language extension @TypeApplications@ for specifying the columns to
-- join on.
--
-- Joins can be done on on one or more columns provided the matched
-- columns have a @Grouping@ instance, most simple types do.
--
-- Presently join columns must be present and named identically in both left
-- and right frames.
--
-- Basic usage: @innerJoin \@'[JoinCol1, ..., JoinColN] leftFrame rightFrame@
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
      {-# INLINE mergeFun #-}
      mergeFun = mergeRec @fs
      {-# INLINE proj1 #-}
      proj1 = rcast @fs
      {-# INLINE proj2 #-}
      proj2 = rcast @fs


justsFromRec :: RMap fs => Record fs -> Rec (Maybe :. ElField) fs
{-# INLINE justsFromRec #-}
justsFromRec = rmap (Compose . Just)

mkNothingsRec :: forall fs.
  (RecApplicative fs) =>
  Rec (Maybe :. ElField) fs
{-# INLINE mkNothingsRec #-}
mkNothingsRec = rpure @fs (Compose Nothing)

-- | Perform an outer join (@FULL JOIN@) operation on two frames.
--
-- Requires the use the  language extension @TypeApplications@ for specifying the
-- columns to join on.
--
-- Joins can be done on on one or more columns provided the
-- columns have a @Grouping@ instance, most simple types do.
--
-- Presently join columns must be present and named identically in both left
-- and right frames.
--
-- Returns a list of Records in the Maybe interpretation functor.
-- If a key in the left table is missing from the right table, non-key
-- columns from the right table are filled with @Nothing@.
-- If a key in the right table is missing from the left table, non-key
-- columns from the right table are filled with @Nothing@.
--
-- Basic usage: @outerJoin \@'[JoinCol1, ..., JoinColN] leftFrame rightFrame@
outerJoin :: forall fs rs rs' rs2  rs2' ors.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs' ⊆ rs
    , rs' ~ RDeleteAll fs rs
    , rs2' ⊆ rs2
    , rs2' ~ RDeleteAll fs rs2
    , ors ~ (rs ++ rs2')
    , ors :~: (rs' ++ rs2)
    , RecApplicative rs2'
    , RecApplicative rs
    , RecApplicative rs'
    , Grouping (Record fs)
    , RMap rs
    , RMap rs2
    , RMap ors
    , RecVec rs
    , RecVec rs2'
    , RecVec ors
    ) =>
  Frame (Record rs)  -- ^ The left frame
  -> Frame (Record rs2) -- ^ The right frame
  -> [Rec (Maybe :. ElField) ors] -- ^ A list of the merged records, now in the Maybe functor

outerJoin a b =
  concat
  (outer grouping mergeFun mergeLeftEmpty mergeRightEmpty
    proj1 proj2 (toList a) (toList b))
  where
    {-# INLINE proj1 #-}
    proj1 = rcast @fs
    {-# INLINE proj2 #-}
    proj2 = rcast @fs
    {-# INLINE mergeFun #-}
    mergeFun l r = justsFromRec $ mergeRec @fs l r
    {-# INLINE mergeLeftEmpty #-}
    mergeLeftEmpty l = justsFromRec l <+> mkNothingsRec @rs2'
    {-# INLINE mergeRightEmpty #-}
    mergeRightEmpty r = rcast @ors (mkNothingsRec @rs' <+> justsFromRec r)

-- https://hackage.haskell.org/package/discrimination-0.4.1/docs/Data-Discrimination.html#v:outer
-- len(total[total.merge_status == "both"]), len(total[total.merge_status != "both"]),
-- like outerJoin but with status as in pandas
-- outer
  -- :: Discriminating f
  -- => f d           -- ^ the discriminator to use
  -- -> (a -> b -> c) -- ^ how to join two rows
  -- -> (a -> c)      -- ^ row present on the left, missing on the right
  -- -> (b -> c)      -- ^ row present on the right, missing on the left
  -- -> (a -> d)      -- ^ selector for the left table
  -- -> (b -> d)      -- ^ selector for the right table
  -- -> [a]           -- ^ left table
  -- -> [b]           -- ^ right table
  -- -> [[c]]
  -- (<+>) :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
  -- https://hackage.haskell.org/package/vinyl-0.13.3/docs/Data-Vinyl-Core.html#v:-60--43--62-
outerJoinStatus :: forall fs rs rs' rs2  rs2' ors.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    -- , fs   ⊆ '[MergeStatus]
    , rs ⊆ (rs ++ rs2')
    , rs' ⊆ rs
    , rs' ~ RDeleteAll fs rs
    , rs2' ⊆ rs2
    , rs2' ~ RDeleteAll fs rs2
    , ors ~ (rs ++ rs2' ++ '[MergeStatusField])
    -- , ors :~: (rs' ++ rs2)
    , ors :~: (rs' ++ rs2 ++ '[MergeStatusField])
    , RecApplicative rs2'
    , RecApplicative rs
    , RecApplicative rs'
    , Grouping (Record fs)
    , RMap rs
    , RMap rs2
    , RMap ors
    -- RecVec => Tooling to allocate, grow, write to, freeze, and index into records of vectors.
    , RecVec rs
    , RecVec rs2'
    , RecVec ors
    ) =>
  Frame (Record rs)  -- ^ The left frame
  -> Frame (Record rs2) -- ^ The right frame
  -- TODO here we should give the name of the status column
  -- -> FieldRec
  -> [Rec (Maybe :. ElField) ors] -- ^ A list of the merged records, now in the Maybe functor
outerJoinStatus a b =
  concat
  -- mergeFun => how to join two rows
  (outer grouping mergeFun mergeLeftEmpty mergeRightEmpty
    proj1 proj2 (toList a) (toList b))
  where
    {-# INLINE proj1 #-}
    proj1 = rcast @fs
    {-# INLINE proj2 #-}
    proj2 = rcast @fs
    {-# INLINE mergeFun #-}
    --  <+> MergeBoth
    mergeFun l r = justsFromRec $ mergeRecStatus @fs l r  <+> (MergeFromRight &: RNil)
    {-# INLINE mergeLeftEmpty #-}
    mergeLeftEmpty l = justsFromRec l <+> mkNothingsRec @rs2' <+> justsFromRec (MergeFromRight &: RNil)
    {-# INLINE mergeRightEmpty #-}
    -- <+> MergeFromLeft
    mergeRightEmpty r = rcast @ors (mkNothingsRec @rs' <+> justsFromRec (r <+> MergeFromLeft &: RNil))

mergeRecStatus :: forall fs rs rs2 rs2'.
  (fs ⊆ rs2
  , rs2' ⊆ rs2
  , rs2' ~ RDeleteAll fs rs2
  , rs ⊆ (rs ++ rs2')) =>
  Record rs ->
  Record rs2 ->
  Record (rs ++ rs2')
{-# INLINE mergeRecStatus #-}
mergeRecStatus rec1 rec2 =
  rec1 <+> rec2'
  where
    rec2' = rcast @rs2' rec2

-- | Perform an right join operation on two frames.
--
-- Requires the language extension @TypeApplications@ for specifying the
-- columns to join on.
--
-- Joins can be done on on one or more columns provided the
-- columns have a @Grouping@ instance, most simple types do.
--
-- Presently join columns must be present and named identically in both left
-- and right frames.
--
-- Returns a list of Records in the Maybe interpretation functor.
-- If a key in the right table is missing from the left table, non-key
-- columns from the right table are filled with @Nothing@.
--
-- Basic usage: @rightJoin \@'[JoinCol1, ..., JoinColN] leftFrame rightFrame@
rightJoin :: forall fs rs rs' rs2  rs2' ors.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs' ⊆ rs
    , rs' ~ RDeleteAll fs rs
    , rs2' ⊆ rs2
    , rs2' ~ RDeleteAll fs rs2
    , ors ~ (rs ++ rs2')
    , ors :~: (rs' ++ rs2)
    , RecApplicative rs2'
    , RecApplicative rs
    , RecApplicative rs'
    , Grouping (Record fs)
    , RMap rs2
    , RMap ors
    , RecVec rs
    , RecVec rs2'
    , RecVec ors
    ) =>
  Frame (Record rs)  -- ^ The left frame
  -> Frame (Record rs2) -- ^ The right frame
  -> [Rec (Maybe :. ElField) ors] -- ^ A list of the merged records, now in the Maybe functor

rightJoin a b =
  concat  $
  rightOuter grouping mergeFun mergeRightEmpty
  proj1 proj2 (toList a) (toList b)
  where
    {-# INLINE proj1 #-}
    proj1 = rcast @fs
    {-# INLINE proj2 #-}
    proj2 = rcast @fs
    {-# INLINE mergeFun #-}
    mergeFun l r = justsFromRec $ mergeRec @fs l r
    {-# INLINE mergeRightEmpty #-}
    mergeRightEmpty r = rcast @ors (mkNothingsRec @rs' <+> justsFromRec r)

-- | Perform an left join operation on two frames.
--
-- Requires the language extension @TypeApplications@ for specifying the
-- columns to join on.
--
-- Joins can be done on on one or more columns provided the
-- columns have a @Grouping@ instance, most simple types do.
--
-- Presently join columns must be present and named identically in both left
-- and right frames.
--
-- Returns a list of Records in the Maybe interpretation functor.
-- If a key in the left table is missing from the right table, non-key
-- columns from the right table are filled with @Nothing@.
--
-- Basic usage: @leftJoin \@'[JoinCol1, ..., JoinColN] leftFrame rightFrame@
leftJoin :: forall fs rs rs2  rs2'.
  (fs    ⊆ rs
    , fs   ⊆ rs2
    , rs ⊆ (rs ++ rs2')
    , rs2' ⊆ rs2
    , rs2' ~ RDeleteAll fs rs2
    , RMap rs
    , RMap (rs ++ rs2')
    , RecApplicative rs2'
    , Grouping (Record fs)
    , RecVec rs
    , RecVec rs2'
    , RecVec (rs ++ rs2')
    ) =>
  Frame (Record rs)  -- ^ The left frame
  -> Frame (Record rs2) -- ^ The right frame
  -> [Rec (Maybe :. ElField) (rs ++ rs2')] -- ^ A list of the merged records, now in the Maybe functor

leftJoin a b =
  concat
  (leftOuter grouping mergeFun mergeLeftEmpty
    proj1 proj2 (toList a) (toList b))
  where
    proj1 = rcast @fs
    proj2 = rcast @fs
    mergeFun l r = justsFromRec $ mergeRec @fs l r
    mergeLeftEmpty l = justsFromRec l <+> mkNothingsRec @rs2'
