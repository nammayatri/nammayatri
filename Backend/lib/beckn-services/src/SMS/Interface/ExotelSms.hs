{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.Interface.ExotelSms
  ( module Reexport,
    sendOTP,
  )
where

import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import SMS.ExotelSms.Config
import qualified SMS.ExotelSms.Flow as Ex
import SMS.ExotelSms.Types
import SMS.Interface.Types as IT
import SMS.Types as Reexport
import Servant

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  ExotelSmsCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP exoCfg SendSMSReq {..} = do
  let sid = exoCfg.sid
  apiKey <- decrypt exoCfg.apiKey
  apiToken <- decrypt exoCfg.apiToken
  let exoUrl = exoCfg.url
      exoOtpSmsTemplate = smsBody
      exoPhoneNumber = phoneNumber
      senderName = sender
      authData =
        BasicAuthData
          (DT.encodeUtf8 apiKey)
          (DT.encodeUtf8 apiToken)
  res <- Ex.sendOTPApi exoUrl authData sid exoOtpSmsTemplate exoPhoneNumber senderName
  return $ returnSmsResultExo res.exoSMSMessage.exoStatus

returnSmsResultExo :: ExotelSmsStatus -> IT.SendSMSRes
returnSmsResultExo txt =
  case txt of
    SENT -> Success
    FAILED -> Fail
    FAILED_DND -> Fail
    QUEUED -> Pending
    SENDING -> Pending
    _ -> UnknownError
