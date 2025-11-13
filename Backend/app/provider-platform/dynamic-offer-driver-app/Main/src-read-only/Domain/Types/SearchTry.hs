{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchTry where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified SharedLogic.Type
import qualified Tools.Beam.UtilsTH

data SearchTry = SearchTry
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    billingCategory :: SharedLogic.Type.BillingCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    customerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimateId :: Kernel.Prelude.Text,
    estimateIds :: [Kernel.Prelude.Text],
    id :: Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry,
    isAdvancedBookingEnabled :: Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    messageId :: Kernel.Prelude.Text,
    petCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    preferSafetyPlus :: Kernel.Prelude.Bool,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    searchRepeatCounter :: Kernel.Prelude.Int,
    searchRepeatType :: Domain.Types.SearchTry.SearchRepeatType,
    serviceTierArray :: [Kernel.Prelude.Text],
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.SearchTry.SearchTryStatus,
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SearchRepeatType = INITIAL | RETRIED | REALLOCATION | CANCELLED_AND_RETRIED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SearchTryStatus = ACTIVE | CANCELLED | COMPLETED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''SearchTryStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''SearchTryStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''SearchRepeatType)
