{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.MyValueFirst.Flow where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import SMS.Config (SmsConfig)
import qualified SMS.MyValueFirst.API as API
import SMS.MyValueFirst.Types (SubmitSms (..), SubmitSmsRes (..), submitSmsResToText)

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

type OrgName = Text

type InviteTemplate = Text

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  m SubmitSmsRes
sendOTPApi url username password otpSmsTemplate phoneNumber sender = do
  submitSms
    url
    SubmitSms
      { username = username,
        password = password,
        from = sender,
        to = phoneNumber,
        text = otpSmsTemplate
      }

sendSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  m SubmitSmsRes
sendSms smsCfg smsTemplate phoneNumber = do
  let smsCred = smsCfg.credConfig
      url = smsCfg.url
  submitSms
    url
    SubmitSms
      { username = smsCred.username,
        password = smsCred.password,
        from = smsCfg.sender,
        to = phoneNumber,
        text = smsTemplate
      }

checkSmsResult :: (Log m, MonadThrow m) => SubmitSmsRes -> m ()
checkSmsResult =
  \case
    Sent -> pure ()
    BadNumber -> throwError SMSInvalidNumber
    InvalidReceiver -> throwError SMSInvalidNumber
    err -> throwError $ SMSError $ submitSmsResToText err
