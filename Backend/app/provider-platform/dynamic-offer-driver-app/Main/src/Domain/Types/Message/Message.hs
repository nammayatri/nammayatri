{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Message.Message where

import Data.Map as HM
import Data.OpenApi hiding (description, title)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Domain.Types.MediaFile as MF
import Domain.Types.Merchant (Merchant)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data MessageType = Action Text | Read deriving (Generic, ToJSON, FromJSON, ToSchema, Read, Show)

instance FromField MessageType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MessageType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be MessageType

instance FromBackendRow Postgres MessageType

deriving stock instance Ord MessageType

deriving stock instance Eq MessageType

instance IsString MessageType where
  fromString = show

data Message = Message
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    shortDescription :: Text,
    label :: Maybe Text,
    likeCount :: Int,
    viewCount :: Int,
    mediaFiles :: [Id MF.MediaFile],
    messageTranslations :: [MessageTranslation],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }

data MessageTranslation = MessageTranslation
  { language :: Language,
    title :: Text,
    description :: Text,
    shortDescription :: Text,
    label :: Maybe Text,
    createdAt :: UTCTime
  }

data RawMessage = RawMessage
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    shortDescription :: Text,
    label :: Maybe Text,
    likeCount :: Int,
    viewCount :: Int,
    mediaFiles :: [Id MF.MediaFile],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON)

data MessageDict = MessageDict
  { defaultMessage :: RawMessage,
    translations :: HM.Map Text RawMessage
  }
  deriving (Generic, ToJSON, FromJSON)
