{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, MultiParamTypeClasses, PolyKinds,
             ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances, TemplateHaskell, QuasiQuotes,
             Rank2Types, TypeApplications, AllowAmbiguousTypes #-}

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
import Data.Discrimination
import Data.Foldable as F
import Frames.Frame
import Frames.Rec
import Frames.InCore (toFrame)
import Frames.Melt (RDeleteAll)
import Frames.InCore (RecVec)
import Data.Vinyl.TypeLevel
import Data.Vinyl
import Data.Vinyl.Functor

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


justsFromRec :: Record fs -> Rec (Maybe :. ElField) fs
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
