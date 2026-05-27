module API.Internal.SendEmailOTP
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.SendEmailOTP as Domain
import qualified Email.OTP.API as Shared
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common

type API = Shared.API

handler :: FlowServer API
handler = sendEmailOTP

sendEmailOTP :: Text -> Context.City -> Maybe Text -> Domain.SendEmailOTPReq -> FlowHandler Domain.SendEmailOTPRes
sendEmailOTP merchantShortId city apiKey = withFlowHandlerAPI . Domain.sendEmailOTP apiKey merchantShortId city
