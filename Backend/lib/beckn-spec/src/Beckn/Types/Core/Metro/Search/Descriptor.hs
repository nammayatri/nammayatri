 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.Search.Descriptor (Descriptor (..), emptyDescriptor) where

import Beckn.Types.Core.Metro.Search.Image (Image)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Types.App (BaseUrl)
import Kernel.Utils.JSON

data Descriptor = Descriptor
  { name :: Maybe Text,
    code :: Maybe Text,
    symbol :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: Maybe [Image],
    audio :: Maybe BaseUrl,
    _3d_render :: Maybe BaseUrl
  }
  deriving (Generic, Show, Eq, ToSchema)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Descriptor where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

emptyDescriptor :: Descriptor
emptyDescriptor =
  Descriptor
    { name = Nothing,
      code = Nothing,
      symbol = Nothing,
      short_desc = Nothing,
      long_desc = Nothing,
      images = Nothing,
      audio = Nothing,
      _3d_render = Nothing
    }
