{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Passetto
  ( API.Types.Dashboard.AppManagement.Passetto.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Passetto.API)
handler merchantId city = postPassettoEncrypt merchantId city :<|> postPassettoDecrypt merchantId city

postPassettoEncrypt :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptResp)
postPassettoEncrypt a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Passetto.postPassettoEncrypt a3 a2 a1

postPassettoDecrypt :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptResp)
postPassettoDecrypt a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Passetto.postPassettoDecrypt a3 a2 a1
