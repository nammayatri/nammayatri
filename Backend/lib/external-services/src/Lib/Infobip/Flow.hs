{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Infobip.Flow where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.Infobip.API.SendSms as APISend
import qualified Lib.Infobip.API.WebengageWebhook as APIStatus
import Lib.Infobip.Types

sendSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  InfoBIPConfig ->
  Text ->
  Text ->
  Text ->
  Text ->
  m SMSRes
sendSms smsCfg smsTemplate phoneNumber entityId templetId = do
  let url = smsCfg.url
  let userName = smsCfg.username
  let password = smsCfg.password
  let from = smsCfg.sender
  let webhookurl = smsCfg.webhookurl
  submitSms url userName password from phoneNumber smsTemplate entityId templetId webhookurl

submitSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  m SMSRes
submitSms url userName password from phoneNumber smsTemplate entityId templetId webhookurl = do
  callAPI url (APISend.sendSms userName password from phoneNumber smsTemplate entityId templetId webhookurl) "sendSms"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_SEND_SMS") url)

callWebengageWebhook ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  WebengageConfig ->
  WebengageRes ->
  m APISuccess
callWebengageWebhook webCfg req = do
  let url = webCfg.url
  callAPI url (APIStatus.sendStatus req) "success"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_WEBHOOK") url)
