{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Email.Flow
  ( sendEmail,
    sendMagicLinkEmail,
    sendBusinessVerificationEmail,
    sendEmailWithAttachment,
    module Email.Types,
  )
where

import qualified Data.Text as T
import qualified Email.AWS.Flow as AWS
import qualified Email.GCP.Flow as GCP
import Email.Types
import Kernel.Prelude
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.App (lookupCloudType)

getSendGridUrl :: EmailServiceConfig -> String
getSendGridUrl config = maybe "" T.unpack config.sendGridUrl

sendEmail :: EmailServiceConfig -> EmailOTPConfig -> [Text] -> Text -> IO ()
sendEmail serviceConfig emailConfig to otpCode = do
  putStrLn $ ("DEBUG: Email.Flow: sendEmail called. To: " :: Text) <> show to
  handleEmailRouting serviceConfig "sendEmail" $ \cloudType ->
    case cloudType of
      AWS -> AWS.sendEmail emailConfig to otpCode
      GCP -> GCP.sendEmail (getSendGridUrl serviceConfig) emailConfig to otpCode
      UNAVAILABLE -> do
           putStrLn ("ERROR: Email.Flow: CloudType UNAVAILABLE" :: Text)
           error "CloudType UNAVAILABLE: Cannot route email"

sendMagicLinkEmail :: EmailServiceConfig -> EmailMagicLinkConfig -> [Text] -> Text -> IO ()
sendMagicLinkEmail serviceConfig emailConfig to token = do
  putStrLn $ ("DEBUG: Email.Flow: sendMagicLinkEmail called. To: " :: Text) <> show to
  handleEmailRouting serviceConfig "sendMagicLinkEmail" $ \cloudType ->
    case cloudType of
      AWS -> AWS.sendMagicLinkEmail emailConfig to token
      GCP -> GCP.sendMagicLinkEmail (getSendGridUrl serviceConfig) emailConfig to token
      UNAVAILABLE -> do
           putStrLn ("ERROR: Email.Flow: CloudType UNAVAILABLE" :: Text)
           error "CloudType UNAVAILABLE: Cannot route magic link email"

sendBusinessVerificationEmail :: EmailServiceConfig -> EmailBusinessVerificationConfig -> [Text] -> Text -> Text -> IO ()
sendBusinessVerificationEmail serviceConfig emailConfig to otpCode token = do
  putStrLn $ ("DEBUG: Email.Flow: sendBusinessVerificationEmail called. To: " :: Text) <> show to
  handleEmailRouting serviceConfig "sendBusinessVerificationEmail" $ \cloudType ->
    case cloudType of
      AWS -> AWS.sendBusinessVerificationEmail emailConfig to otpCode token
      GCP -> GCP.sendBusinessVerificationEmail (getSendGridUrl serviceConfig) emailConfig to otpCode token
      UNAVAILABLE -> do
           putStrLn ("ERROR: Email.Flow: CloudType UNAVAILABLE" :: Text)
           error "CloudType UNAVAILABLE: Cannot route business verification email"

sendEmailWithAttachment ::
  EmailServiceConfig ->
  Text -> -- From email
  [Text] -> -- To emails
  Text -> -- Subject
  Text -> -- Body text
  FilePath -> -- Attachment file path
  Text -> -- Attachment filename
  IO ()
sendEmailWithAttachment serviceConfig from to subject bodyText filePath fileName = do
  putStrLn $ ("DEBUG: Email.Flow: sendEmailWithAttachment called. To: " :: Text) <> show to
  handleEmailRouting serviceConfig "sendEmailWithAttachment" $ \cloudType ->
    case cloudType of
      AWS -> AWS.sendEmailWithAttachment from to subject bodyText filePath fileName
      GCP -> GCP.sendEmailWithAttachment (getSendGridUrl serviceConfig) from to subject bodyText filePath fileName
      UNAVAILABLE -> do
           putStrLn ("ERROR: Email.Flow: CloudType UNAVAILABLE" :: Text)
           error "CloudType UNAVAILABLE: Cannot route email with attachment"

handleEmailRouting :: EmailServiceConfig -> Text -> (CloudType -> IO ()) -> IO ()
handleEmailRouting config caller action = do
  if config.isForcedAWS
    then do
      putStrLn $ ("DEBUG: Email.Flow: " <> caller <> ": isForcedAWS is TRUE. Forcing routing to AWS SES." :: Text)
      action AWS
    else do
      putStrLn $ ("DEBUG: Email.Flow: " <> caller <> ": isForcedAWS is FALSE. Looking up CloudType..." :: Text)
      cloudType <- lookupCloudType
      putStrLn $ ("DEBUG: Email.Flow: " <> caller <> ": Detected CloudType: " :: Text) <> show cloudType
      action cloudType
