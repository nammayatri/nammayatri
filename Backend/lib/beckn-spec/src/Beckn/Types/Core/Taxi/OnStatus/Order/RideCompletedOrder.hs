{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder
  ( module Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Agent as Reexport
import Beckn.Types.Core.Taxi.Common.Authorization as Reexport
import Beckn.Types.Core.Taxi.Common.RideCompletedPayment as Reexport
import Beckn.Types.Core.Taxi.Common.RideCompletedQuote as Reexport
import Beckn.Types.Core.Taxi.Common.RideStartedStartInfo as Reexport
import Beckn.Types.Core.Taxi.Common.Vehicle as Reexport
import Beckn.Types.Core.Taxi.OnStatus.Order.OrderState (RideCompletedOrderCode (RIDE_COMPLETED))
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, title, value)
import EulerHS.Prelude hiding (id, state, (.=))
import Kernel.Prelude
import Kernel.Utils.Schema

data RideCompletedOrder = RideCompletedOrder
  { id :: Text,
    state :: RideCompletedOrderCode,
    quote :: RideCompletedQuote,
    fulfillment :: FulfillmentInfo,
    payment :: Maybe RideCompletedPayment,
    arrival_time :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

orderState :: RideCompletedOrderCode
orderState = RIDE_COMPLETED

data FulfillmentInfo = FulfillmentInfo
  { id :: Text, -- bppRideId
    start :: RideStartedStartInfo,
    end :: EndInfo,
    agent :: Agent,
    vehicle :: Vehicle,
    chargeable_distance :: DecimalValue,
    traveled_distance :: DecimalValue
  }
  deriving (Generic, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions fulfillmentInfoJSONOptions

instance FromJSON FulfillmentInfo where
  parseJSON = genericParseJSON fulfillmentInfoJSONOptions

instance ToJSON FulfillmentInfo where
  toJSON = genericToJSON fulfillmentInfoJSONOptions

fulfillmentInfoJSONOptions :: A.Options
fulfillmentInfoJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "chargeable_distance" -> "./komn/chargeable_distance"
        a -> a
    }

newtype EndInfo = EndInfo
  { time :: TimeTimestamp
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema EndInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
