module Beckn.External.MyValueFirst.Flow where

import qualified Beckn.External.MyValueFirst.API as API
import Beckn.External.MyValueFirst.Types (SubmitSms (..), SubmitSmsRes (..))
import Beckn.Sms.Config (SmsConfig (..))
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Data.Text as T
import EulerHS.Prelude

submitSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  SubmitSms ->
  m SubmitSmsRes
submitSms url params = do
  callAPI url (API.submitSms params) "submitSms"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_SEND_SMS") url)

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

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  Text ->
  m SubmitSmsRes
sendOTP smsCfg otpSmsTemplate phoneNumber otpCode = do
  let smsCred = smsCfg.credConfig
  let url = smsCfg.url
  let otpHash = smsCred.otpHash
  submitSms
    url
    SubmitSms
      { username = smsCred.username,
        password = smsCred.password,
        from = smsCfg.sender,
        to = phoneNumber,
        text = constructOtpSms otpCode otpHash otpSmsTemplate
      }

checkRegistrationSmsResult :: (Log m, MonadThrow m) => SubmitSmsRes -> m ()
checkRegistrationSmsResult =
  \case
    Sent -> pure ()
    BadNumber -> throwError SMSInvalidNumber
    InvalidReceiver -> throwError SMSInvalidNumber
    err -> throwError $ SMSError err
