{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Stops
  ( module Beckn.Types.Core.Taxi.Common.Stops,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport (Location)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data StopType = START | END | INTERMEDIATE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, Enum, Bounded)

data Stops = Stops
  { location :: Location,
    stopType :: StopType
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Stops where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
