{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Offer
  ( postOfferCreate,
    postOfferUpdate,
    getOfferList,
    postOfferToggle,
    postOfferValidateEligibility,
    getOfferEligibilitySchema,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Offer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.Offer
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postOfferCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Offer.CreateOfferReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOfferCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.offerDSL.postOfferCreate) req)

postOfferUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> API.Types.RiderPlatform.Management.Offer.UpdateOfferReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOfferUpdate merchantShortId opCity apiTokenInfo offerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.offerDSL.postOfferUpdate) offerId req)

getOfferList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [API.Types.RiderPlatform.Management.Offer.OfferResp])
getOfferList merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.offerDSL.getOfferList)

postOfferToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOfferToggle merchantShortId opCity apiTokenInfo offerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.offerDSL.postOfferToggle) offerId)

postOfferValidateEligibility :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityReq -> Environment.Flow API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityResp)
postOfferValidateEligibility merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.offerDSL.postOfferValidateEligibility) req)

getOfferEligibilitySchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow API.Types.RiderPlatform.Management.Offer.OfferEligibilitySchemaResp)
getOfferEligibilitySchema merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.offerDSL.getOfferEligibilitySchema)
