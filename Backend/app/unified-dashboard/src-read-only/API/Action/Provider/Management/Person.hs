{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Provider.Management.Person
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management.Person
import qualified Domain.Action.Provider.Management.Person
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (PostPersonCreate :<|> PostUserLoginSendOtp)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPersonCreate merchantId city :<|> postUserLoginSendOtp merchantId city

type PostPersonCreate = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'POST_MANAGEMENT_PERSON_CREATE :> API.Types.UnifiedDashboard.Management.Person.PostPersonCreate)

type PostUserLoginSendOtp = API.Types.UnifiedDashboard.Management.Person.PostUserLoginSendOtp

postPersonCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.Person.CreatePersonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Provider.Management.Person.postPersonCreate merchantShortId opCity apiTokenInfo req

postUserLoginSendOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UnifiedDashboard.Management.Person.SendOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postUserLoginSendOtp merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.Provider.Management.Person.postUserLoginSendOtp merchantShortId opCity req
