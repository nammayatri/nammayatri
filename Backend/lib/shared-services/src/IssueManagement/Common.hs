{-# LANGUAGE ApplicativeDo #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Common where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import EulerHS.Prelude hiding (id, state)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Servant hiding (Summary)

data Identifier = CUSTOMER | DRIVER
  deriving (Generic, Show, Read)

data Ride

data Person

data Merchant

data RideT

data Driver

data User

data IssueStatus
  = OPEN
  | PENDING
  | AWAIT
  | RESOLVED
  | REOPENED
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, FromJSON, ToJSON, ToParamSchema)

$(mkBeamInstancesForEnum ''IssueStatus)

instance FromHttpApiData IssueStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData IssueStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader :: IssueStatus -> ByteString
  toHeader = BSL.toStrict . encode

data ChatType = IssueMessage | IssueOption | MediaFile | IssueDescription
  deriving (Generic, FromJSON, ToSchema, ToJSON, Show, Read, Eq, Ord)

data Sender = USER | BOT
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

data MessageType = Text | Audio | Image
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

data Chat = Chat
  { chatType :: ChatType,
    chatId :: Text,
    timestamp :: UTCTime
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be Chat where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromField Chat where
  fromField = fromFieldEnum

instance FromField [Chat] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Chat] where
  sqlValueSyntax batchList =
    let x = (show <$> batchList :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Chat]

instance FromBackendRow Postgres [Chat]
