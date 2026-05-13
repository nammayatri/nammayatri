{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.Domain where

import Data.Map.Strict (foldlWithKey', insert)
import qualified Data.Text as T
import EulerHS.Prelude
import SMS.Types

sendSMS ::
  (Monad m) =>
  (Text -> m (Text, Text, Text, Maybe Text)) -> -- lookupTemplate: key -> (messageTemplate, sender, templateId, messageType)
  m Text -> -- generateOtp
  Maybe Text -> -- useFakeOtp
  (Text -> Text -> Text -> Text -> Maybe Text -> m ()) -> -- sendSms: msg -> phoneNumber -> sender -> templateId -> messageType -> m ()
  SendSMSReq ->
  m SendSMSRes
sendSMS lookupTemplate generateOtp useFakeOtp sendSms req = do
  let isOtpMessage = fromMaybe False req.isOtp
  if isOtpMessage
    then do
      otpCode <- generateOtp
      let otpToUse = fromMaybe otpCode useFakeOtp
          updatedTemplateVars = insert "otp" otpToUse req.templateVars
      unless (isJust useFakeOtp) $ do
        (msgTemplate, sender, templateId, msgType) <- lookupTemplate req.messageKey
        let msg = foldlWithKey' (\acc k v -> T.replace ("{#" <> k <> "#}") v acc) msgTemplate updatedTemplateVars
        sendSms msg req.phoneNumber sender templateId msgType
      pure $ SendSMSRes {otp = Just otpToUse}
    else do
      (msgTemplate, sender, templateId, msgType) <- lookupTemplate req.messageKey
      let msg = foldlWithKey' (\acc k v -> T.replace ("{#" <> k <> "#}") v acc) msgTemplate req.templateVars
      sendSms msg req.phoneNumber sender templateId msgType
      pure $ SendSMSRes {otp = Nothing}
