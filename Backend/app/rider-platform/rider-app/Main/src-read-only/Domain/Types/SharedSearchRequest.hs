{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedSearchRequest where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SharedSearchRequest = SharedSearchRequest
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest,
    maxDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    searchRequestIds :: [Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest],
    status :: Domain.Types.SharedSearchRequest.SharedSearchRequestStatus,
    totalCustomerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory,
    waypoints :: Data.Aeson.Value
  }
  deriving (Generic, (Show))

data SharedSearchRequestStatus = POOLING | MATCHED | EXPIRED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SharedSearchRequestStatus))

$(mkHttpInstancesForEnum (''SharedSearchRequestStatus))
