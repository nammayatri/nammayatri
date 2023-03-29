{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Message.Message where

import Data.Map as HM
import Data.OpenApi hiding (description, title)
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Message.MediaFile as MF
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Types (Language)

data MessageType = Action Text | Read deriving (Generic, ToJSON, FromJSON, ToSchema, Read, Show)

data Message = Message
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    label :: Maybe Text,
    mediaFiles :: [Id MF.MediaFile],
    messageTranslations :: [MessageTranslation],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }

data MessageTranslation = MessageTranslation
  { language :: Language,
    title :: Text,
    description :: Text,
    label :: Maybe Text,
    createdAt :: UTCTime
  }

data RawMessage = RawMessage
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    label :: Maybe Text,
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
