{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.StartInfo where

import Beckn.Types.Core.Taxi.Common.Authorization
import Beckn.Types.Core.Taxi.Common.Location (Location)
import Beckn.Types.Core.Taxi.OnSearch (TimeTimestamp)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.JSON (removeNullFields)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data StartInfo = StartInfo
  { location :: Location,
    authorization :: Maybe Authorization,
    time :: TimeTimestamp
  }
  deriving (Generic, Show)

instance ToJSON StartInfo where
  toJSON = genericToJSON removeNullFields

instance FromJSON StartInfo where
  parseJSON = genericParseJSON removeNullFields

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
