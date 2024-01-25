{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.EventTracker where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data EventTracker = EventTracker
  { createdAt :: Kernel.Prelude.UTCTime,
    entity :: Kernel.Prelude.Text,
    entityFieldName :: Kernel.Prelude.Text,
    entityPrimaryId :: Kernel.Prelude.Text,
    eventName :: Domain.Types.EventTracker.EventName,
    fromState :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.EventTracker.EventTracker,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionServiceName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toState :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EventName = DRIVER_FEE_AUTO_PAY_TO_MANUAL | AUTO_PAY_STATUS_TOGGLE | SERVICE_USAGE_CHARGE_TOGGLE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EventName)
