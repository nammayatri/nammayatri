{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.RiderPlatform.Management.Offer 
( API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified API.Types.RiderPlatform.Management.Offer
import qualified API.Types.RiderPlatform.Management
import qualified Domain.Action.RiderPlatform.Management.Offer
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.APISuccess
import qualified Lib.Payment.Domain.Types.Offer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("offer" :> (PostOfferCreate :<|> PostOfferUpdate :<|> GetOfferList :<|> PostOfferToggle :<|> PostOfferValidateEligibility :<|> GetOfferEligibilitySchema))
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postOfferCreate merchantId city :<|> postOfferUpdate merchantId city :<|> getOfferList merchantId city :<|> postOfferToggle merchantId city :<|> postOfferValidateEligibility merchantId city :<|> getOfferEligibilitySchema merchantId city
type PostOfferCreate = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                ('DSL)
                                (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.OFFER) / ('API.Types.RiderPlatform.Management.Offer.POST_OFFER_CREATE)) :> API.Types.RiderPlatform.Management.Offer.PostOfferCreate)
type PostOfferUpdate = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                ('DSL)
                                (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.OFFER) / ('API.Types.RiderPlatform.Management.Offer.POST_OFFER_UPDATE)) :> API.Types.RiderPlatform.Management.Offer.PostOfferUpdate)
type GetOfferList = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                             ('DSL)
                             (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.OFFER) / ('API.Types.RiderPlatform.Management.Offer.GET_OFFER_LIST)) :> API.Types.RiderPlatform.Management.Offer.GetOfferList)
type PostOfferToggle = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                ('DSL)
                                (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.OFFER) / ('API.Types.RiderPlatform.Management.Offer.POST_OFFER_TOGGLE)) :> API.Types.RiderPlatform.Management.Offer.PostOfferToggle)
type PostOfferValidateEligibility = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                             ('DSL)
                                             (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.OFFER) / ('API.Types.RiderPlatform.Management.Offer.POST_OFFER_VALIDATE_ELIGIBILITY)) :> API.Types.RiderPlatform.Management.Offer.PostOfferValidateEligibility)
type GetOfferEligibilitySchema = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                          ('DSL)
                                          (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.OFFER) / ('API.Types.RiderPlatform.Management.Offer.GET_OFFER_ELIGIBILITY_SCHEMA)) :> API.Types.RiderPlatform.Management.Offer.GetOfferEligibilitySchema)
postOfferCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Offer.CreateOfferReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Offer.postOfferCreate merchantShortId opCity apiTokenInfo req
postOfferUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> API.Types.RiderPlatform.Management.Offer.UpdateOfferReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferUpdate merchantShortId opCity apiTokenInfo offerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Offer.postOfferUpdate merchantShortId opCity apiTokenInfo offerId req
getOfferList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Offer.OfferResp])
getOfferList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Offer.getOfferList merchantShortId opCity apiTokenInfo
postOfferToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferToggle merchantShortId opCity apiTokenInfo offerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Offer.postOfferToggle merchantShortId opCity apiTokenInfo offerId
postOfferValidateEligibility :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityResp)
postOfferValidateEligibility merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Offer.postOfferValidateEligibility merchantShortId opCity apiTokenInfo req
getOfferEligibilitySchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.RiderPlatform.Management.Offer.OfferEligibilitySchemaResp)
getOfferEligibilitySchema merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Offer.getOfferEligibilitySchema merchantShortId opCity apiTokenInfo



