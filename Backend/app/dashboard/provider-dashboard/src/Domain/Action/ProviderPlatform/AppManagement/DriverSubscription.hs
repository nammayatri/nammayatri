module Domain.Action.ProviderPlatform.AppManagement.DriverSubscription
  ( postDriverSubscriptionSendSms,
    postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo,
  )
where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.DriverSubscription
import qualified Dashboard.Common
import "dashboard-helper-api" Dashboard.Common (HideSecrets (hideSecrets))
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

data VolunteerTransactionStorageReq = VolunteerTransactionStorageReq
  { volunteerId :: Text,
    driverId :: Text,
    messageKey :: Text,
    channel :: Text,
    overlayKey :: Text,
    messageId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, Kernel.Prelude.ToSchema)

instance HideSecrets VolunteerTransactionStorageReq where
  hideSecrets = Kernel.Prelude.identity

postDriverSubscriptionSendSms ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Driver ->
  API.Types.Dashboard.AppManagement.DriverSubscription.SendSmsReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postDriverSubscriptionSendSms merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      ( Kernel.Prelude.Just $
          VolunteerTransactionStorageReq
            apiTokenInfo.personId.getId
            driverId.getId
            (show req.messageKey)
            (show req.channel)
            (show $ fromMaybe "" req.overlayKey)
            (show $ fromMaybe "" req.messageId)
      )
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.driverSubscriptionDSL.postDriverSubscriptionSendSms)
      driverId
      apiTokenInfo.personId.getId
      req

postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Driver ->
  Dashboard.Common.ServiceNames ->
  API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate ->
  Environment.Flow API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate
postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo merchantShortId opCity apiTokenInfo driverId serviceName req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.driverSubscriptionDSL.postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo)
      driverId
      serviceName
      req
