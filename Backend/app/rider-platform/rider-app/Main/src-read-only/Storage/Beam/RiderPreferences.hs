{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderPreferences where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.RiderPreferences
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RiderPreferencesT f = RiderPreferencesT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    preferenceData :: (B.C f Data.Aeson.Value),
    preferenceType :: (B.C f Domain.Types.Extra.RiderPreferences.PreferenceType),
    riderId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderPreferencesT where
  data PrimaryKey RiderPreferencesT f = RiderPreferencesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RiderPreferencesId . id

type RiderPreferences = RiderPreferencesT Identity

$(enableKVPG (''RiderPreferencesT) [('id)] [[('riderId)]])

$(mkTableInstances (''RiderPreferencesT) "rider_preferences")
