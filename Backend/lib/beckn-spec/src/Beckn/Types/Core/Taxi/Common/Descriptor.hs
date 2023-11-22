{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Descriptor where

import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (exp, id)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Descriptor = Descriptor
  { short_desc :: Maybe Text,
    code :: Maybe Text,
    name :: Maybe Text,
    images :: Maybe Text
  }
  deriving (Generic, Show)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Descriptor where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON Descriptor where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
