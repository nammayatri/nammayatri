{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.DynamicOfferDriver.Management.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.Common.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import Servant
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

type API = ("driver" :> (GetDriverPanAadharSelfieDetails :<|> PostDriverSyncDocAadharPan))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverPanAadharSelfieDetails merchantId city :<|> postDriverSyncDocAadharPan merchantId city

type GetDriverPanAadharSelfieDetails = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('DRIVERS) ('PAN_AADHAAR_SELFIE_DETAILS) :> API.Types.ProviderPlatform.Management.Driver.GetDriverPanAadharSelfieDetails)

type PostDriverSyncDocAadharPan = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('DRIVERS) ('SYNC_DOC_AADHAR_PAN) :> API.Types.ProviderPlatform.Management.Driver.PostDriverSyncDocAadharPan)

getDriverPanAadharSelfieDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp)
getDriverPanAadharSelfieDetails merchantShortId opCity apiTokenInfo a1 a2 =
  withFlowHandlerAPI' $
    ( do
        checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
        ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.driverDSL.getDriverPanAadharSelfieDetails) a1 a2
    )

postDriverSyncDocAadharPan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSyncDocAadharPan merchantShortId opCity apiTokenInfo req =
  withFlowHandlerAPI' $
    ( do
        checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
        transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.DriverAPI Dashboard.Common.Driver.PostDriverSyncDocAadharPanEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
        SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.driverDSL.postDriverSyncDocAadharPan) req)
    )
