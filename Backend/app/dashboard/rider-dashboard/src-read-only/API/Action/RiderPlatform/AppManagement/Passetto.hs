{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.Passetto
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Passetto
import qualified Domain.Action.RiderPlatform.AppManagement.Passetto
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("passetto" :> (PostPassettoEncrypt :<|> PostPassettoDecrypt))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPassettoEncrypt merchantId city :<|> postPassettoDecrypt merchantId city

type PostPassettoEncrypt =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASSETTO / 'API.Types.Dashboard.AppManagement.Passetto.POST_PASSETTO_ENCRYPT)
      :> API.Types.Dashboard.AppManagement.Passetto.PostPassettoEncrypt
  )

type PostPassettoDecrypt =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASSETTO / 'API.Types.Dashboard.AppManagement.Passetto.POST_PASSETTO_DECRYPT)
      :> API.Types.Dashboard.AppManagement.Passetto.PostPassettoDecrypt
  )

postPassettoEncrypt :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptResp)
postPassettoEncrypt merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Passetto.postPassettoEncrypt merchantShortId opCity apiTokenInfo req

postPassettoDecrypt :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptResp)
postPassettoDecrypt merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Passetto.postPassettoDecrypt merchantShortId opCity apiTokenInfo req
