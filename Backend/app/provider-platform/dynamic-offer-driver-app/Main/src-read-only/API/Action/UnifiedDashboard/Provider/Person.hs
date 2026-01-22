{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UnifiedDashboard.Provider.Person
  ( API.Types.Provider.Person.API,
    handler,
  )
where

import qualified API.Types.Provider.Person
import qualified Domain.Action.UnifiedDashboard.Provider.Person
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Provider.Person.API)
handler merchantId city = postPersonCreate merchantId city :<|> postUserLoginSendOtp merchantId city

postPersonCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Provider.Person.CreatePersonReq -> Environment.FlowHandler API.Types.Provider.Person.CreatePersonResp)
postPersonCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.UnifiedDashboard.Provider.Person.postPersonCreate a3 a2 a1

postUserLoginSendOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Provider.Person.SendOtpReq -> Environment.FlowHandler API.Types.Provider.Person.SendOtpResp)
postUserLoginSendOtp a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.UnifiedDashboard.Provider.Person.postUserLoginSendOtp a3 a2 a1
