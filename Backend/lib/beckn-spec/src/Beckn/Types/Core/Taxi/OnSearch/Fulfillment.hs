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
  )
where

import Beckn.Types.Core.Taxi.Common.VehicleVariant as Reexport
import Beckn.Types.Core.Taxi.OnSearch.StartInfo
import Beckn.Types.Core.Taxi.OnSearch.StopInfo
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data FulfillmentInfo = FulfillmentInfo
  { id :: Text,
    start :: StartInfo,
    end :: Maybe StopInfo,
    vehicle :: FulfillmentVehicle
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
