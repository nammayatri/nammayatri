{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Spec.OnSearch.Departure where

import Data.OpenApi
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Departure = Departure
  { id :: Text,
    route_id :: Text,
    start_time :: TimeStamp,
    end_time :: TimeStamp
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema Departure where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype TimeStamp = TimeStamp
  { timestamp :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema TimeStamp where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
