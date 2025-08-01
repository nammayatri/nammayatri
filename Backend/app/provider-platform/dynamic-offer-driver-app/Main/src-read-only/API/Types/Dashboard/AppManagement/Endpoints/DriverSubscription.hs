{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.DriverSubscription where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.VendorFee
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data DriverFeeInfoToUpdate = DriverFeeInfoToUpdate
  { driverFeeId :: Kernel.Prelude.Text,
    mkManualDue :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    mkAutoPayDue :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    mkCleared :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    sgstWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    cgstWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InvoiceInfoToUpdate = InvoiceInfoToUpdate {invoiceId :: Kernel.Prelude.Text, driverFeeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text, invoiceStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SendSmsReq = SendSmsReq
  { channel :: Domain.Types.MerchantMessage.MediaChannel,
    messageKey :: Kernel.Prelude.Maybe Domain.Types.MerchantMessage.MessageKey,
    overlayKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    messageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SendSmsReq where
  hideSecrets = Kernel.Prelude.identity

data SubscriptionDriverFeesAndInvoicesToUpdate = SubscriptionDriverFeesAndInvoicesToUpdate
  { driverFees :: Kernel.Prelude.Maybe [DriverFeeInfoToUpdate],
    invoices :: Kernel.Prelude.Maybe [InvoiceInfoToUpdate],
    vendorFees :: Kernel.Prelude.Maybe [Domain.Types.VendorFee.VendorFee],
    mkDuesToAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    mkDuesToAmountWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    subscribed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SubscriptionDriverFeesAndInvoicesToUpdate where
  hideSecrets = Kernel.Prelude.identity

type API = ("plan" :> (PostDriverSubscriptionSendSmsHelper :<|> PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo))

type PostDriverSubscriptionSendSms = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "sendSms" :> ReqBody '[JSON] SendSmsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverSubscriptionSendSmsHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> Capture "volunteerId" Kernel.Prelude.Text :> "sendSms"
      :> ReqBody
           '[JSON]
           SendSmsReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "update" :> "driverFeeAndInvoiceInfo"
      :> Capture
           "serviceName"
           Dashboard.Common.ServiceNames
      :> ReqBody '[JSON] SubscriptionDriverFeesAndInvoicesToUpdate
      :> Post
           '[JSON]
           SubscriptionDriverFeesAndInvoicesToUpdate
  )

data DriverSubscriptionAPIs = DriverSubscriptionAPIs
  { postDriverSubscriptionSendSms :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> SendSmsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.ServiceNames -> SubscriptionDriverFeesAndInvoicesToUpdate -> EulerHS.Types.EulerClient SubscriptionDriverFeesAndInvoicesToUpdate
  }

mkDriverSubscriptionAPIs :: (Client EulerHS.Types.EulerClient API -> DriverSubscriptionAPIs)
mkDriverSubscriptionAPIs driverSubscriptionClient = (DriverSubscriptionAPIs {..})
  where
    postDriverSubscriptionSendSms :<|> postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo = driverSubscriptionClient

data DriverSubscriptionUserActionType
  = POST_DRIVER_SUBSCRIPTION_SEND_SMS
  | POST_DRIVER_SUBSCRIPTION_UPDATE_DRIVER_FEE_AND_INVOICE_INFO
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DriverSubscriptionUserActionType])
