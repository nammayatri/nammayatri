{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Version where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Extra.Rollout
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Version = Version
  { id :: Kernel.Types.Id.Id Domain.Types.Version.Version,
    inputDataType :: Domain.Types.Extra.Rollout.RawDataType,
    isReadyToApply :: Kernel.Prelude.Bool,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    versionTag :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
