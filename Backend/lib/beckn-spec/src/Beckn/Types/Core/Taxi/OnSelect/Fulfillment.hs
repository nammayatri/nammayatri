{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSelect.Fulfillment
  ( module Beckn.Types.Core.Taxi.OnSelect.Fulfillment,
    module Reexport,
    FulfillmentType (..),
  )
where

import Beckn.Types.Core.Taxi.Common.FulfillmentInfo (FulfillmentType (..))
import Beckn.Types.Core.Taxi.Common.VehicleVariant as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Agent
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data FulfillmentInfo = FulfillmentInfo
  { id :: Text,
    start :: StartInfo,
    end :: StopInfo,
    vehicle :: FulfillmentVehicle,
    _type :: FulfillmentType,
    agent :: Agent
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype FulfillmentVehicle = FulfillmentVehicle
  { category :: VehicleVariant
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentVehicle where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
