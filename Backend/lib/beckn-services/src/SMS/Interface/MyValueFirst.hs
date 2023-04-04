{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.Interface.MyValueFirst
  ( module Reexport,
    sendOTP,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import SMS.Interface.Types as IT
import SMS.MyValueFirst.Config
import qualified SMS.MyValueFirst.Flow as MVF
import SMS.MyValueFirst.Types as MT
import SMS.Types as Reexport

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  MyValueFirstCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP smsCfg SendSMSReq {..} = do
  let urlAddress = smsCfg.url
      otpSmsTemp = smsBody
      phone = phoneNumber
      senderName = sender
  username <- decrypt smsCfg.username
  password <- decrypt smsCfg.password
  res <- MVF.sendOTPApi urlAddress username password otpSmsTemp phone senderName

  return $ returnSmsResultMVF res

returnSmsResultMVF :: MT.SubmitSmsRes -> IT.SendSMSRes
returnSmsResultMVF txt =
  case txt of
    Sent -> Success
    BadNumber -> Fail
    _ -> IT.UnknownError
