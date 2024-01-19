{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantOperatingCity where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data MerchantOperatingCityT f = MerchantOperatingCityT
  { city :: B.C f Kernel.Types.Beckn.Context.City,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantShortId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantOperatingCityT where
  data PrimaryKey MerchantOperatingCityT f = MerchantOperatingCityId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = MerchantOperatingCityId . id

type MerchantOperatingCity = MerchantOperatingCityT Identity

$(enableKVPG ''MerchantOperatingCityT ['id] [])

$(mkTableInstances ''MerchantOperatingCityT "merchant_operating_city")
