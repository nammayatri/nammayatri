{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.NyRegularInstanceLog where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.NyRegularSubscription
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data NyRegularInstanceLog = NyRegularInstanceLog
  { automationStatus :: Domain.Types.NyRegularInstanceLog.NyRegularInstanceAutomationStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    instanceTransactionId :: Kernel.Prelude.Text,
    nyRegularSubscriptionId :: Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription,
    scheduledPickupTime :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

data NyRegularInstanceAutomationStatus
  = PENDING
  | SEARCH_SENT
  | AUTO_SELECTED
  | BOOKING_INITIATED
  | CONFIRMED
  | FAILED_NO_OFFER
  | FAILED_BPP_ERROR
  | FAILED_CONFIRMATION
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''NyRegularInstanceAutomationStatus)
