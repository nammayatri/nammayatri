{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Beckn.Types.Core.Taxi.Common.FulfillmentInfo
  ( module Beckn.Types.Core.Taxi.Common.FulfillmentInfo,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Agent as Reexport
import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import qualified Beckn.Types.Core.Taxi.Common.Tags as T
import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Reexport
import Data.Aeson (omitNothingFields)
import Data.Aeson.Types (Options)
import Data.OpenApi hiding (tags)
import Kernel.Prelude
import Kernel.Utils.JSON (removeNullFields, stripPrefixUnderscoreIfAny)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data StartInfo = StartInfo
  { authorization :: Maybe Authorization,
    location :: Location,
    time :: Maybe TimeTimestamp
  }
  deriving (Generic, Show)

instance ToJSON StartInfo where
  toJSON = genericToJSON removeNullFields

instance FromJSON StartInfo where
  parseJSON = genericParseJSON removeNullFields

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data Authorization = Authorization
  { _type :: Text,
    token :: Text
  }
  deriving (Eq, Generic, Show)

instance ToSchema Authorization where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Authorization where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Authorization where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data FulfillmentInfo = FulfillmentInfo
  { id :: Text, -- bppRideId
    start :: StartInfo,
    end :: Maybe EndInfo,
    agent :: Maybe Agent,
    _type :: Text,
    vehicle :: Maybe Vehicle,
    tags :: Maybe T.TagGroups
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToJSON FulfillmentInfo where
  toJSON = genericToJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreAndRemoveNullFields

data EndInfo = EndInfo
  { location :: Location,
    time :: Maybe TimeTimestamp
  }
  deriving (Generic, Show)

instance ToJSON EndInfo where
  toJSON = genericToJSON removeNullFields

instance FromJSON EndInfo where
  parseJSON = genericParseJSON removeNullFields

instance ToSchema EndInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data Vehicle = Vehicle
  { model :: Text,
    variant :: Text,
    color :: Text,
    registration :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Vehicle where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Location = Location
  { gps :: Gps
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

stripPrefixUnderscoreAndRemoveNullFields :: Options
stripPrefixUnderscoreAndRemoveNullFields =
  stripPrefixUnderscoreIfAny
    { omitNothingFields = True
    }
