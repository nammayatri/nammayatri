{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TripTerms where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TripTermsT f = TripTermsT
  { createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    descriptions :: (B.C f Data.Text.Text),
    id :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))
  }
  deriving (Generic, B.Beamable)

instance B.Table TripTermsT where
  data PrimaryKey TripTermsT f = TripTermsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = TripTermsId . id

type TripTerms = TripTermsT Identity

$(enableKVPG (''TripTermsT) [('id)] [])

$(mkTableInstances (''TripTermsT) "trip_terms")
