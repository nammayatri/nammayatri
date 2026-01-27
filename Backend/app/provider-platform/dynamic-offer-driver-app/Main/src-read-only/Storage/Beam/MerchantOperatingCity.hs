{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantOperatingCity where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data MerchantOperatingCityT f = MerchantOperatingCityT
  { city :: B.C f Kernel.Types.Beckn.Context.City,
    country :: B.C f Kernel.Types.Beckn.Context.Country,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    id :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language,
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantShortId :: B.C f Kernel.Prelude.Text,
    state :: B.C f Kernel.Types.Beckn.Context.IndianState,
    stdCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    supportNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantOperatingCityT where
  data PrimaryKey MerchantOperatingCityT f = MerchantOperatingCityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantOperatingCityId . id

type MerchantOperatingCity = MerchantOperatingCityT Identity

$(enableKVPG ''MerchantOperatingCityT ['id] [])

$(mkTableInstances ''MerchantOperatingCityT "merchant_operating_city")

$(Domain.Types.UtilsTH.mkCacParseInstance ''MerchantOperatingCityT)
