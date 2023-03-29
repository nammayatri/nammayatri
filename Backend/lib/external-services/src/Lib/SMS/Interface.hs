{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.SMS.Interface
  ( module Reexport,
    sendSMS,
    checkSmsResult,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Encryption
import Lib.Error
import Lib.SMS.ExotelSms.Config as Reexport
import qualified Lib.SMS.Interface.ExotelSms as ExotelSms
import qualified Lib.SMS.Interface.MyValueFirst as MyValueFirst
import Lib.SMS.Interface.Types as Reexport
import Lib.SMS.MyValueFirst.Config as Reexport
import Lib.SMS.Types as Reexport

sendSMS :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => SmsHandler m -> SendSMSReq -> m SendSMSRes
sendSMS SmsHandler {..} req = do
  prividersPriorityList <- getProvidersPriorityList
  when (null prividersPriorityList) $ throwError $ InternalError "No sms serive provider configured"
  sendSmsWithFallback prividersPriorityList
  where
    sendSmsWithFallback [] = throwError $ InternalError "Not able to send sms with all the configured providers"
    sendSmsWithFallback (preferredProvider : restProviders) = do
      smsConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ sendSMS' smsConfig req
      case result of
        Left _ -> sendSmsWithFallback restProviders
        Right res -> case res of
          UnknownError -> sendSmsWithFallback restProviders
          Fail -> sendSmsWithFallback restProviders
          _ -> pure res

sendSMS' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  SmsServiceConfig ->
  SendSMSReq ->
  m SendSMSRes
sendSMS' serviceConfig req = case serviceConfig of
  ExotelSmsConfig cfg -> ExotelSms.sendOTP cfg req
  MyValueFirstConfig cfg -> MyValueFirst.sendOTP cfg req

checkSmsResult ::
  (Log m, MonadThrow m) => SendSMSRes -> m ()
checkSmsResult txt =
  case txt of
    Success -> pure ()
    Fail -> throwError SMSInvalidNumber
    Pending -> pure ()
    _ -> throwError SMSInvalidNumber
