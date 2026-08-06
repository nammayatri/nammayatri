{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Management.Registration
  ( API,
    handler,
  )
where

import qualified "this" API.Types.Management
import qualified "this" API.Types.Management.Registration
import qualified Domain.Action.Management.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (PostUserLogin :<|> PostUserLoginVerifyOtp)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postUserLogin merchantId city :<|> postUserLoginVerifyOtp merchantId city

type PostUserLogin = API.Types.Management.Registration.PostUserLogin

type PostUserLoginVerifyOtp = API.Types.Management.Registration.PostUserLoginVerifyOtp

postUserLogin :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Management.Registration.LoginReq -> Environment.FlowHandler API.Types.Management.Registration.LoginRes)
postUserLogin merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.Management.Registration.postUserLogin merchantShortId opCity req

postUserLoginVerifyOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Management.Registration.VerifyOtpReq -> Environment.FlowHandler API.Types.Management.Registration.LoginRes)
postUserLoginVerifyOtp merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.Management.Registration.postUserLoginVerifyOtp merchantShortId opCity req
