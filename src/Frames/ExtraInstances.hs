{-# LANGUAGE ConstraintKinds, CPP, DataKinds, FlexibleContexts,
             FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             PolyKinds, ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances, TemplateHaskell,
             QuasiQuotes, Rank2Types, TypeApplications,
             AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Frames.ExtraInstances where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Discrimination.Grouping
import Data.Hashable
import Control.DeepSeq
import Frames.Col
import Frames.Rec
import Frames.Frame
import Frames.RecF (AllCols)
import Data.Vinyl.Functor as VF
import Data.Vinyl
import Data.Text (Text)

-- Grouping instances
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
  grouping = contramap hash grouping


-- NFData* instances
#if MIN_VERSION_deepseq(1,4,3)
instance (NFData a) =>
         NFData (VF.Identity a) where
  rnf = rnf1

instance NFData1 VF.Identity where
  liftRnf r = r . getIdentity

instance (AllCols NFData rs
         , NFData1 f
         , Functor f
         , NFData (Rec f rs)
         , NFData (s :-> r)
         , NFData r) =>
         NFData (Rec f ((s :-> r) : rs)) where
  rnf (r :& rs) = rnf1 r `seq` rnf rs

instance (NFData1 f) => NFData (Rec f '[]) where
  rnf = rwhnf
#endif

instance (NFData a) =>
         NFData (Frame a) where
  rnf = foldr (\x acc -> rnf x `seq` acc) ()

instance (NFData a) => NFData (s :-> a) where
  rnf = rnf . getCol
