{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Notification (module Domain.Types.Notification, module ReExport) where

import Data.Aeson
import qualified Domain.Types.DriverFee
import Domain.Types.Extra.Notification as ReExport
import qualified Domain.Types.Extra.Notification
import qualified Domain.Types.Mandate
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Notification = Notification
  { createdAt :: Kernel.Prelude.UTCTime,
    dateCreated :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Text,
    driverFeeId :: Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee,
    id :: Kernel.Types.Id.Id Domain.Types.Notification.Notification,
    juspayProvidedId :: Kernel.Prelude.Text,
    lastStatusCheckedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastUpdated :: Kernel.Prelude.UTCTime,
    mandateId :: Kernel.Types.Id.Id Domain.Types.Mandate.Mandate,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notificationType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shortId :: Kernel.Prelude.Text,
    sourceAmount :: Kernel.Types.Common.HighPrecMoney,
    status :: Domain.Types.Extra.Notification.NotificationStatus,
    txnDate :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)
