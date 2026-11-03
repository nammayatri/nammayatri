{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Email.Types
  ( EmailOTPConfig (..),
    EmailMagicLinkConfig (..),
    EmailBusinessVerificationConfig (..),
    EmailServiceConfig (..),
    EmailAttachment (..),
    EmailBodyFormat (..),
    EmailProvider (..),
    EmailSendOptions (..),
    EmailSendResult (..),
    noEmailSendOptions,
  )
where

import Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (..))

data EmailServiceConfig = EmailServiceConfig
  { sendGridUrl :: Maybe Text,
    isForcedAWS :: Bool,
    maxAttachmentBytes :: Int
  }
  deriving (Generic, FromDhall)

data EmailAttachment = EmailAttachment
  { content :: ByteString,
    filename :: Text,
    contentType :: Text
  }

data EmailBodyFormat = Text | HtmlText
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema)

data EmailProvider = SES | SENDGRID
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

-- | Delivery-tracking options for a send. On SES, 'configurationSet' routes the message's delivery and
-- bounce events to that configuration set's event destinations, and 'tags' come back on every event.
-- On SendGrid, 'tags' are sent as custom_args, which SendGrid echoes on its event webhook.
data EmailSendOptions = EmailSendOptions
  { configurationSet :: Maybe Text,
    tags :: [(Text, Text)]
  }
  deriving (Show, Eq)

noEmailSendOptions :: EmailSendOptions
noEmailSendOptions = EmailSendOptions {configurationSet = Nothing, tags = []}

-- | The provider that accepted the message and the id it gave it (SES MessageId / SendGrid X-Message-Id).
data EmailSendResult = EmailSendResult
  { provider :: EmailProvider,
    messageId :: Maybe Text
  }
  deriving (Show, Eq)

data EmailOTPConfig = EmailOTPConfig
  { fromEmail :: Text,
    subject :: Text,
    bodyTemplate :: Text
  }
  deriving (Read, Generic, Show, FromJSON, ToJSON, ToSchema, Ord, Eq)

data EmailMagicLinkConfig = EmailMagicLinkConfig
  { fromEmail :: Text,
    subject :: Text,
    bodyTemplate :: Text,
    verificationUrlTemplate :: Text -- Should contain <token> placeholder
  }
  deriving (Read, Generic, Show, FromJSON, ToJSON, ToSchema, Ord, Eq)

data EmailBusinessVerificationConfig = EmailBusinessVerificationConfig
  { fromEmail :: Text,
    subject :: Text,
    bodyTemplate :: Text, -- Should contain both <otp> and <link> placeholders
    verificationUrlTemplate :: Text -- Should contain <token> placeholder
  }
  deriving (Read, Generic, Show, FromJSON, ToJSON, ToSchema, Ord, Eq)

fromFieldEmailConfig ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion EmailOTPConfig
fromFieldEmailConfig f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance FromField EmailOTPConfig where
  fromField = fromFieldEmailConfig

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be EmailOTPConfig where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be EmailOTPConfig

instance FromBackendRow Postgres EmailOTPConfig

instance {-# OVERLAPPING #-} ToSQLObject EmailOTPConfig where
  convertToSQLObject = SQLObjectValue . show . A.encode

fromFieldEmailMagicLinkConfig ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion EmailMagicLinkConfig
fromFieldEmailMagicLinkConfig f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance FromField EmailMagicLinkConfig where
  fromField = fromFieldEmailMagicLinkConfig

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be EmailMagicLinkConfig where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be EmailMagicLinkConfig

instance FromBackendRow Postgres EmailMagicLinkConfig

instance {-# OVERLAPPING #-} ToSQLObject EmailMagicLinkConfig where
  convertToSQLObject = SQLObjectValue . show . A.encode

fromFieldEmailBusinessVerificationConfig ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion EmailBusinessVerificationConfig
fromFieldEmailBusinessVerificationConfig f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance FromField EmailBusinessVerificationConfig where
  fromField = fromFieldEmailBusinessVerificationConfig

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be EmailBusinessVerificationConfig where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be EmailBusinessVerificationConfig

instance FromBackendRow Postgres EmailBusinessVerificationConfig

instance {-# OVERLAPPING #-} ToSQLObject EmailBusinessVerificationConfig where
  convertToSQLObject = SQLObjectValue . show . A.encode
