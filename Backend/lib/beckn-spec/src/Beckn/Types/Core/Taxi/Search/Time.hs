{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Search.Time where

import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Example
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Time where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Time where
  example =
    Time
      { timestamp = example
      }
