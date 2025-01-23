{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Rollout (module Domain.Types.Rollout, module ReExport) where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import Domain.Types.Extra.Rollout as ReExport
import qualified Domain.Types.Extra.Rollout
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Rollout = Rollout
  { id :: Kernel.Types.Id.Id Domain.Types.Rollout.Rollout,
    inputDataType :: Domain.Types.Extra.Rollout.RawDataType,
    percentage :: Kernel.Prelude.Int,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    versionTag :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
