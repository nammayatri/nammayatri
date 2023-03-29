{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Infobip.Types where

import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

newtype SMSRes = SMSRes
  { messages :: [Message]
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data Message = Message
  { to :: Text,
    status :: SMSStatus,
    messageId :: Text,
    smsCount :: Integer
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data SMSStatus = SMSStatus
  { description :: Text,
    groupId :: Int,
    groupName :: Text,
    id :: Int,
    name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data WebengageRes = WebengageRes
  { version :: Text,
    messageId :: Text,
    toNumber :: Text,
    status :: Text,
    statusCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data InfoBIPConfig = InfoBIPConfig
  { username :: Text,
    password :: Text,
    url :: BaseUrl,
    sender :: Text,
    token :: Text,
    webhookurl :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

newtype WebengageConfig = WebengageConfig
  { url :: BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)
