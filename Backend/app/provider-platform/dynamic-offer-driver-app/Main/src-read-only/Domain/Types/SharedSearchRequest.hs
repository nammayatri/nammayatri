{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedSearchRequest where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SharedSearchRequest = SharedSearchRequest
  { createdAt :: Kernel.Prelude.UTCTime,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocationIds :: [Kernel.Types.Id.Id Domain.Types.Location.Location],
    id :: Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    status :: Domain.Types.SharedSearchRequest.SearchRequestStatus,
    toLocationIds :: [Kernel.Types.Id.Id Domain.Types.Location.Location],
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    transactionId :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory
  }
  deriving (Generic, Show)

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SearchRequestStatus))

$(mkHttpInstancesForEnum (''SearchRequestStatus))
