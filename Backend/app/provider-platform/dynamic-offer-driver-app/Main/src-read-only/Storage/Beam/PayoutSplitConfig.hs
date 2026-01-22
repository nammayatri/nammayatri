{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutSplitConfig where

import qualified Data.Aeson
import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data PayoutSplitConfigT f = PayoutSplitConfigT
  { area :: (B.C f Lib.Types.SpecialLocation.Area),
    bankDetails :: (B.C f Data.Aeson.Value),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    vehicleVariant :: (B.C f Domain.Types.VehicleVariant.VehicleVariant),
    vendorId :: (B.C f Data.Text.Text),
    vendorSplitAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutSplitConfigT where
  data PrimaryKey PayoutSplitConfigT f
    = PayoutSplitConfigId (B.C f Lib.Types.SpecialLocation.Area) (B.C f Data.Text.Text) (B.C f Domain.Types.VehicleVariant.VehicleVariant) (B.C f Data.Text.Text)
    deriving (Generic, B.Beamable)
  primaryKey = PayoutSplitConfigId <$> area <*> merchantOperatingCityId <*> vehicleVariant <*> vendorId

type PayoutSplitConfig = PayoutSplitConfigT Identity

$(enableKVPG (''PayoutSplitConfigT) [('area), ('merchantOperatingCityId), ('vehicleVariant), ('vendorId)] [])

$(mkTableInstances (''PayoutSplitConfigT) "payout_split_config")
