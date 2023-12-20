{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Beckn.Types.Core.Taxi.Common.Agent where

import qualified Beckn.Types.Core.Taxi.Common.Customer as Customer
import Beckn.Types.Core.Taxi.Common.Descriptor
import Beckn.Types.Core.Taxi.Common.Tags
import Data.Aeson
import Data.OpenApi hiding (Example, contact, example, name, tags)
import Kernel.Prelude
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data AgentV2 = AgentV2
  { person :: Maybe Customer.OrderPersonV2,
    contact :: Maybe Customer.Contact,
    organization :: Maybe Organization,
    rating :: Maybe Text
    -- name :: Text, -- moved to person in 2.x
    -- rateable :: Bool, -- removed in 2.x
    -- phone :: Maybe Text, -- moved to contact in 2.x
    -- image :: Maybe Text, -- moved to person in 2.x
    -- tags :: Maybe TagGroups -- moved to person in 2.x
  }
  deriving (Generic, Show)

instance ToSchema AgentV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON AgentV2 where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON AgentV2 where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data Organization = Organization
  { descriptor :: Maybe Descriptor,
    address :: Maybe String,
    state :: Maybe Place,
    city :: Maybe Place,
    contact :: Maybe Customer.Contact
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

data Place = Place
  { name :: Maybe String,
    code :: Maybe String
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Agent = Agent
  { name :: Text,
    rateable :: Bool,
    phone :: Maybe Text,
    image :: Maybe Text,
    tags :: Maybe TagGroups
  }
  deriving (Generic, Show)

instance ToSchema Agent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Agent where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON Agent where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
