{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverPlan (module Domain.Types.DriverPlan, module ReExport) where

import Data.Aeson
import qualified Domain.Types.DriverInformation
import Domain.Types.Extra.DriverPlan as ReExport
import qualified Domain.Types.Extra.DriverPlan
import qualified Domain.Types.Mandate
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverPlan = DriverPlan
  { autoPayStatus :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverAutoPayStatus,
    coinCovertedToCashLeft :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    enableServiceUsageCharge :: Kernel.Prelude.Bool,
    lastPaymentLinkSentAtIstDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    mandateId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Mandate.Mandate),
    mandateSetupDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOpCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planId :: Kernel.Types.Id.Id Domain.Types.Plan.Plan,
    planType :: Domain.Types.Plan.PaymentMode,
    serviceName :: Domain.Types.Plan.ServiceNames,
    subscriptionServiceRelatedData :: Domain.Types.Extra.DriverPlan.SubscriptionServiceRelatedData,
    totalCoinsConvertedCash :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, Ord)

{-
	DSL Source Link: file://./../../../spec/Storage/DriverPlan.yaml
-}
