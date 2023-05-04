{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.Tags where

import Data.Aeson
import Data.OpenApi hiding (Example, example, name, tags)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Tags = Tags
  { force :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Tags where
  toJSON = genericToJSON tagsJSONOptions

instance FromJSON Tags where
  parseJSON = genericParseJSON tagsJSONOptions

instance ToSchema Tags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions tagsJSONOptions

tagsJSONOptions :: Options
tagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "force" -> "./komn/force"
        a -> a
    }
