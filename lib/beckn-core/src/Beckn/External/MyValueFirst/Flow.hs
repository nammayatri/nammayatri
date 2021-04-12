module Beckn.External.MyValueFirst.Flow where

import qualified Beckn.External.MyValueFirst.API as API
import Beckn.External.MyValueFirst.Types
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Client

submitSms :: HasLogContext r => BaseUrl -> SubmitSms -> FlowR r (Either Text ())
submitSms url params = do
  res <- L.callAPI url $ API.submitSms params
  whenRight res $ \_ ->
    logInfo "SMS" $ "Submitted sms successfully to " <> show (_to params)
  return $ first show res

type OtpTemplate = Text

constructOtpSms :: Text -> Text -> OtpTemplate -> Text
constructOtpSms otp hash =
  let otpTemp = "{#otp#}"
      hashTemp = "{#hash#}"
   in T.replace otpTemp otp . T.replace hashTemp hash

type OrgName = Text

type InviteTemplate = Text

constructInviteSms :: OrgName -> InviteTemplate -> Text
constructInviteSms = T.replace "{#org#}"
