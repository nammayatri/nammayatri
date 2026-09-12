{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Passetto
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Passetto
import qualified Domain.Action.Dashboard.AppManagement.Passetto
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("passetto" :> (PostPassettoEncrypt :<|> PostPassettoDecrypt))

type PostPassettoEncrypt = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/PASSETTO/POST_PASSETTO_ENCRYPT" :> API.Types.Dashboard.AppManagement.Passetto.PostPassettoEncrypt)

type PostPassettoDecrypt = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/PASSETTO/POST_PASSETTO_DECRYPT" :> API.Types.Dashboard.AppManagement.Passetto.PostPassettoDecrypt)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPassettoEncrypt merchantId city :<|> postPassettoDecrypt merchantId city

postPassettoEncrypt :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptResp)
postPassettoEncrypt a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Passetto.postPassettoEncrypt a4 a3 a1

postPassettoDecrypt :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptResp)
postPassettoDecrypt a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Passetto.postPassettoDecrypt a4 a3 a1
