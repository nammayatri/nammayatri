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

import Database.Beam.MySQL ()
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForList)
import Kernel.Prelude
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

$(mkBeamInstancesForList ''AddressResp)
