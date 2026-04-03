{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.Offer 
( API.Types.RiderPlatform.Management.Offer.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Offer
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.RiderPlatform.Management.Offer
import qualified Kernel.Types.APISuccess
import qualified Lib.Payment.Domain.Types.Offer



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Offer.API)
handler merchantId city = postOfferCreate merchantId city :<|> postOfferUpdate merchantId city :<|> getOfferList merchantId city :<|> postOfferToggle merchantId city :<|> postOfferValidateEligibility merchantId city :<|> getOfferEligibilitySchema merchantId city
postOfferCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Offer.CreateOfferReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferCreate a3 a2 a1
postOfferUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> API.Types.RiderPlatform.Management.Offer.UpdateOfferReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferUpdate a4 a3 a2 a1
getOfferList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Offer.OfferResp])
getOfferList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.getOfferList a2 a1
postOfferToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOfferToggle a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferToggle a3 a2 a1
postOfferValidateEligibility :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Offer.ValidateOfferEligibilityResp)
postOfferValidateEligibility a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.postOfferValidateEligibility a3 a2 a1
getOfferEligibilitySchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.RiderPlatform.Management.Offer.OfferEligibilitySchemaResp)
getOfferEligibilitySchema a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Offer.getOfferEligibilitySchema a2 a1



