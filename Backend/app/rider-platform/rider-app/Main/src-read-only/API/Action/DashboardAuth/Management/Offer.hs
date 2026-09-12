{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Offer
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Offer
import qualified Domain.Action.Dashboard.Offer
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.Offer
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("offer" :> (PostOfferCreate :<|> PostOfferUpdate :<|> GetOfferList :<|> PostOfferToggle :<|> PostOfferValidateEligibility :<|> GetOfferEligibilitySchema))

type PostOfferCreate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/OFFER/POST_OFFER_CREATE" :> API.Types.RiderPlatform.Management.Offer.PostOfferCreate)

type PostOfferUpdate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/OFFER/POST_OFFER_UPDATE" :> API.Types.RiderPlatform.Management.Offer.PostOfferUpdate)

type GetOfferList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/OFFER/GET_OFFER_LIST" :> API.Types.RiderPlatform.Management.Offer.GetOfferList)

type PostOfferToggle = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/OFFER/POST_OFFER_TOGGLE" :> API.Types.RiderPlatform.Management.Offer.PostOfferToggle)

type PostOfferValidateEligibility =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/OFFER/POST_OFFER_VALIDATE_ELIGIBILITY"
      :> API.Types.RiderPlatform.Management.Offer.PostOfferValidateEligibility
  )

type GetOfferEligibilitySchema =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/OFFER/GET_OFFER_ELIGIBILITY_SCHEMA"
      :> API.Types.RiderPlatform.Management.Offer.GetOfferEligibilitySchema
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postOfferCreate merchantId city :<|> postOfferUpdate merchantId city :<|> getOfferList merchantId city :<|> postOfferToggle merchantId city :<|> postOfferValidateEligibility merchantId city :<|> getOfferEligibilitySchema merchantId city

postOfferCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Offer.CreateOfferReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferCreate a4 a3 a1

postOfferUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> API.Types.RiderPlatform.Management.Offer.UpdateOfferReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferUpdate a5 a4 a2 a1

getOfferList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Offer.OfferResp])
getOfferList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.getOfferList a3 a2

postOfferToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferToggle a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferToggle a4 a3 a1

postOfferValidateEligibility :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityResp)
postOfferValidateEligibility a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferValidateEligibility a4 a3 a1

getOfferEligibilitySchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.RiderPlatform.Management.Offer.OfferEligibilitySchemaResp)
getOfferEligibilitySchema a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.getOfferEligibilitySchema a3 a2
