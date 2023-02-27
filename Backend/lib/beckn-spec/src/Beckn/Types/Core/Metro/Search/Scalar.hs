{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.Search.Scalar (Scalar (..), ScalarRange (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (max, min)
import Kernel.Utils.JSON

data Scalar = Scalar
  { _type :: Maybe ScalarType,
    value :: Int, -- FIXME: probably not integer
    estimated_value :: Maybe Int,
    computed_value :: Maybe Int,
    range :: Maybe ScalarRange,
    unit :: Text
  }
  deriving (Generic, Show, ToSchema)

data ScalarType = CONSTANT | VARIABLE
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data ScalarRange = ScalarRange
  { min :: Int,
    max :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Scalar where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
