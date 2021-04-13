module Beckn.External.MyValueFirst.Flow where

import qualified Beckn.External.MyValueFirst.API as API
import Beckn.External.MyValueFirst.Types (SmsSender (..), SubmitSms (..))
import Beckn.Sms.Config (SmsConfig (..))
import Beckn.Types.Common
import qualified Beckn.Types.Error.API as Error
import Beckn.Utils.Common
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Client

submitSms :: BaseUrl -> SubmitSms -> FlowR r (Either Text ())
submitSms url params = do
  res <- L.callAPI url $ API.submitSms params
  whenRight res $ \_ ->
    logTagInfo "SMS" $ "Submitted sms successfully to " <> show (_to params)
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

sendOTP :: HasLogContext r => SmsConfig -> Text -> SmsSender -> Text -> Text -> FlowR r ()
sendOTP smsCfg otpSmsTemplate sender phoneNumber otpCode = do
  let smsCred = smsCfg ^. #credConfig
  let url = smsCfg ^. #url
  let otpHash = smsCred ^. #otpHash
  res <-
    submitSms
      url
      SubmitSms
        { _username = smsCred ^. #username,
          _password = smsCred ^. #password,
          _from = sender,
          _to = phoneNumber,
          _text = constructOtpSms otpCode otpHash otpSmsTemplate
        }
  whenLeft res $ \err -> throwErrorWithInfo Error.UnableToSendSMS err
