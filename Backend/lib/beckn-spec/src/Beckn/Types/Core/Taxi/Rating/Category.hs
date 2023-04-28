{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Rating.Category where

import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString.Lazy as BSL
import Data.OpenApi
import Data.Text as T
import Data.Text.Encoding as DT
import EulerHS.Prelude hiding (id)
import Servant

data CategoryName = RIDE
  deriving (Generic, Show, Read, ToSchema, ToParamSchema)

instance ToJSON CategoryName where
  toJSON RIDE = A.String "RIDE"

instance FromJSON CategoryName where
  parseJSON (A.String "RIDE") = pure RIDE
  parseJSON invalid =
    prependFailure
      "parsing CategoryName failed, "
      (typeMismatch "Object" invalid)

instance FromHttpApiData CategoryName where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData CategoryName where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

newtype RatingCategories = RatingCategories {rating_categories :: [CategoryName]}
  deriving (Generic, FromJSON, ToJSON, Show, Read, ToSchema)
