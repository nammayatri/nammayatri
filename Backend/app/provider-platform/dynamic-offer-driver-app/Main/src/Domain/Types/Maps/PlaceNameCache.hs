{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Domain.Types.Maps.PlaceNameCache where

import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data PlaceNameCache = PlaceNameCache
  { id :: Id PlaceNameCache,
    formattedAddress :: Maybe Text,
    plusCode :: Maybe Text,
    lat :: Double,
    lon :: Double,
    placeId :: Maybe Text,
    addressComponents :: [AddressResp],
    geoHash :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddressResp = AddressResp
  { longName :: Text,
    shortName :: Text,
    types :: [Text]
  }
  deriving stock (Generic, Show, Read, Ord, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromField [AddressResp] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance FromField AddressResp where
  fromField = fromFieldEnum

instance (HasSqlValueSyntax be (V.Vector String)) => HasSqlValueSyntax be [AddressResp] where
  sqlValueSyntax addressRespList =
    let x = show <$> addressRespList
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [AddressResp]

instance FromBackendRow Postgres [AddressResp]
