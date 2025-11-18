{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Email.AWS.Flow where

import qualified Amazonka as AWS
import Amazonka.SES
import qualified Data.Text as T
import qualified Email.Types as Email
import Kernel.Prelude

sendEmail :: Email.EmailOTPConfig -> [Text] -> Text -> IO ()
sendEmail config to otpCode = do
  env <- AWS.newEnv AWS.discover
  let from = config.fromEmail
      subject = config.subject
      textBody = T.replace "<otp>" otpCode config.bodyTemplate
      destination = Destination' Nothing Nothing (Just to)
      message = newMessage (newContent subject) (Body' Nothing (Just $ newContent textBody))
      sendReq = newSendEmail from destination message
  void $ AWS.runResourceT $ AWS.send env sendReq

sendMagicLinkEmail :: Email.EmailMagicLinkConfig -> [Text] -> Text -> IO ()
sendMagicLinkEmail config to token = do
  env <- AWS.newEnv AWS.discover
  let from = config.fromEmail
      subject = config.subject
      verificationUrl = T.replace "<token>" token config.verificationUrlTemplate
      textBody = T.replace "<link>" verificationUrl config.bodyTemplate
      destination = Destination' Nothing Nothing (Just to)
      message = newMessage (newContent subject) (Body' Nothing (Just $ newContent textBody))
      sendReq = newSendEmail from destination message
  void $ AWS.runResourceT $ AWS.send env sendReq

sendBusinessVerificationEmail :: Email.EmailBusinessVerificationConfig -> [Text] -> Text -> Text -> IO ()
sendBusinessVerificationEmail config to otpCode token = do
  env <- AWS.newEnv AWS.discover
  let from = config.fromEmail
      subject = config.subject
      verificationUrl = T.replace "<token>" token config.verificationUrlTemplate
      textBody = T.replace "<otp>" otpCode $ T.replace "<link>" verificationUrl config.bodyTemplate
      destination = Destination' Nothing Nothing (Just to)
      message = newMessage (newContent subject) (Body' Nothing (Just $ newContent textBody))
      sendReq = newSendEmail from destination message
  void $ AWS.runResourceT $ AWS.send env sendReq
