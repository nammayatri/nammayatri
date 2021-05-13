module Beckn.External.MyValueFirst.Flow where

import qualified Beckn.External.MyValueFirst.API as API
import Beckn.External.MyValueFirst.Types (SubmitSms (..))
import Beckn.Sms.Config (SmsConfig (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import qualified Data.Text as T
import EulerHS.Prelude
import Servant.Client
import Beckn.Utils.Monitoring.Prometheus.Metrics (HasCoreMetrics)

submitSms :: HasCoreMetrics (FlowR r) => BaseUrl -> SubmitSms -> FlowR r ()
submitSms url params = do
  res <- callAPI url (API.submitSms params) "submitSms"
  whenRight res $ \_ ->
    logTagInfo "SMS" $ "Submitted sms successfully to " <> show (_to params)
  res & fromEitherM (ExternalAPICallErrorWithCode "UNABLE_TO_SEND_SMS" url)

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

sendOTP :: HasCoreMetrics (FlowR r) => SmsConfig -> Text -> Text -> Text -> FlowR r ()
sendOTP smsCfg otpSmsTemplate phoneNumber otpCode = do
  let smsCred = smsCfg ^. #credConfig
  let url = smsCfg ^. #url
  let otpHash = smsCred ^. #otpHash
  submitSms
    url
    SubmitSms
      { _username = smsCred ^. #username,
        _password = smsCred ^. #password,
        _from = smsCfg ^. #sender,
        _to = phoneNumber,
        _text = constructOtpSms otpCode otpHash otpSmsTemplate
      }
