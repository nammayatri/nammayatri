{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSelect.Agent
  ( module Beckn.Types.Core.Taxi.OnSelect.Agent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Beckn.Types.Core.Taxi.Common.Tags as Reexport
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import EulerHS.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Agent = Agent
  { name :: Maybe Text,
    rateable :: Maybe Bool,
    tags :: TagGroups
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Agent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data AgentTags = AgentTags
  { agent_info_rating :: Maybe Text,
    agent_info_duration_to_pickup_in_s :: Maybe Text
  }
  deriving (Generic, Show)

-- instance ToSchema Agent where
--   declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

-- newtype AgentInfo = AgentInfo
--     {
--         rating
--     }

instance ToJSON AgentTags where
  toJSON = genericToJSON agentTagsJSONOptions

instance FromJSON AgentTags where
  parseJSON = genericParseJSON agentTagsJSONOptions

instance ToSchema AgentTags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions agentTagsJSONOptions

agentTagsJSONOptions :: Options
agentTagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "agent_info_rating" -> "agent_info/rating"
        "agent_info_duration_to_pickup_in_s" -> "agent_info/duration_to_pickup_in_s"
        a -> a
    }

-- instance ToJSON AgentTags where
--   toJSON = genericToJSON itemTagsJSONOptions

-- instance FromJSON AgentTags where
--   parseJSON = genericParseJSON itemTagsJSONOptions

-- instance ToSchema AgentTags where
--   declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions itemTagsJSONOptions
