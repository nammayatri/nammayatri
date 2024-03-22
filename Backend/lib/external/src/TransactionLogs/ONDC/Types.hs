{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TransactionLogs.ONDC.Types where

import qualified Data.Aeson as A
import Data.Aeson.Types
import EulerHS.Prelude hiding (state)
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.JSON

data ONDCConfig = ONDCConfig
  { apiToken :: Text,
    url :: BaseUrl
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, FromDhall)

data ONDCRequest = ONDCRequest
  { _type :: Text,
    _data :: ReqLog
  }
  deriving (Generic, Eq, Show)

instance FromJSON ONDCRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ONDCRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ReqLog = ReqLog
  { context :: A.Value, -- Context type defined in NY
    message :: A.Value -- message body of type of req (defined in NY)
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data ONDCResponse = ONDCResponsen
  { _type :: Text,
    _data :: RespLog
  }
  deriving (Generic, Eq, Show)

instance FromJSON ONDCResponse where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ONDCResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data RespLog = RespLog
  { context :: A.Value, -- Context type defined in NY
    message :: A.Value -- Ack type defined in NY
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)
