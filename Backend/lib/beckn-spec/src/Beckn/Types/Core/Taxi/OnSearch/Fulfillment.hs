{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSearch.Fulfillment
  ( module Beckn.Types.Core.Taxi.OnSearch.Fulfillment,
    module Reexport,
    FulfillmentType (..),
  )
where

import Beckn.Types.Core.Taxi.Common.Agent as Reexport
import Beckn.Types.Core.Taxi.Common.Customer as Reexport
import Beckn.Types.Core.Taxi.Common.FulfillmentInfo as Reexport (FulfillmentType (..), stripPrefixUnderscoreAndRemoveNullFields)
import Beckn.Types.Core.Taxi.Common.State as Reexport
import Beckn.Types.Core.Taxi.Common.Stops as Reexport
-- import Data.Aeson (Options (..))
-- import Kernel.Utils.JSON
import Beckn.Types.Core.Taxi.Common.Tags as Reexport
import Beckn.Types.Core.Taxi.Common.Vehicle as Reexport
import Beckn.Types.Core.Taxi.OnSearch.StartInfo as Reexport
import Beckn.Types.Core.Taxi.OnSearch.StopInfo as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data FulfillmentInfoV2 = FulfillmentInfoV2
  { id :: Text,
    _type :: FulfillmentType,
    stops :: [Stops],
    vehicle :: Vehicle
    -- tags :: Maybe [TagGroupV2]
    -- rateable :: Maybe Bool,
    -- rating :: Maybe Text,
    -- _state :: Maybe State,
    -- tracking :: Maybe Text,
    -- customer :: Maybe Customer,
    -- agent :: Maybe Agent,
    -- contact :: Maybe Contact,
    -- start :: StartInfo,
    -- end :: StopInfo,
    -- path :: Maybe Text,
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentInfoV2 where
  parseJSON = genericParseJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToJSON FulfillmentInfoV2 where
  toJSON = genericToJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToSchema FulfillmentInfoV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data FulfillmentInfo = FulfillmentInfo
  { id :: Text,
    start :: StartInfo,
    _type :: FulfillmentType,
    end :: StopInfo,
    vehicle :: Vehicle
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToJSON FulfillmentInfo where
  toJSON = genericToJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
