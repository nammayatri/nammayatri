{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Stage where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Extra.Rollout
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Stage = Stage
  { id :: Kernel.Types.Id.Id Domain.Types.Stage.Stage,
    inputDataType :: Domain.Types.Extra.Rollout.RawDataType,
    name :: Domain.Types.Stage.StageName,
    order :: Kernel.Prelude.Int,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data StageName = PREPROCESSING | VALIDATION | UPLOAD deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''StageName)
