{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Tags
  ( module Beckn.Types.Core.Taxi.Common.Tags,
  )
where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, (.=))
import Kernel.Utils.JSON

data TagGroup = TagGroup
  { display :: Bool,
    code :: Text,
    name :: Text,
    list :: [Tag]
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON TagGroup where
  parseJSON = genericParseJSON removeNullFields

instance ToJSON TagGroup where
  toJSON = genericToJSON removeNullFields

data Tag = Tag
  { display :: Maybe Bool,
    code :: Maybe Text,
    name :: Maybe Text,
    value :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Tag where
  parseJSON = genericParseJSON removeNullFields

instance ToJSON Tag where
  toJSON = genericToJSON removeNullFields
