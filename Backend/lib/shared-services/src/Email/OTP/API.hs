module Email.OTP.API where

import EulerHS.Prelude
import Email.OTP.Types
import qualified Kernel.Types.Beckn.Context as Context
import Servant

type API =
  ( "sendEmailOTP"
      :> Capture "merchantShortId" Text
      :> Capture "city" Context.City
      :> Header "api-key" Text
      :> ReqBody '[JSON] SendEmailOTPReq
      :> Post '[JSON] SendEmailOTPRes
  )
