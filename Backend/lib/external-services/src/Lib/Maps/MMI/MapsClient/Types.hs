{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Maps.MMI.MapsClient.Types where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import EulerHS.Prelude ((...))
import Kernel.Prelude
import Kernel.Types.CommonImport
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON (constructorsWithSnakeCase, stripPrefixUnderscoreIfAny)
import Kernel.Utils.TH
import qualified Lib.Maps.Google.PolyLinePoints as PP
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData

data AuthRequest = AuthRequest
  { grant_type :: Text,
    client_id :: Text,
    client_secret :: Text
  }
  deriving (Generic, Eq, Show)

instance ToForm AuthRequest where
  toForm AuthRequest {..} =
    [ ("grant_type", toQueryParam grant_type),
      ("client_id", toQueryParam client_id),
      ("client_secret", toQueryParam client_secret)
    ]

data AuthResp = AuthResp
  { accessToken :: Text,
    tokenType :: Text,
    expiresIn :: Int,
    scope :: Text,
    projectCode :: Text,
    clientId :: Text
  }
  deriving (Generic)

instance FromJSON AuthResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON AuthResp where
  toJSON = genericToJSON constructorsWithSnakeCase

newtype MMIAuthToken = MMIAuthToken
  { getMMIAuthToken :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''MMIAuthToken

data AutoSuggestResp = AutoSuggestResp
  { suggestedLocations :: [SuggestedLocations],
    userAddedLocations :: [UserAddedLocation],
    suggestedSearches :: [SuggestedSearches]
  }
  deriving (Generic, FromJSON, ToJSON)

data SuggestedLocations = SuggestedLocations
  { eLoc :: Text, -- place id in MMI
    placeName :: Text,
    placeAddress :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data UserAddedLocation = UserAddedLocation
  { eLoc :: Text,
    orderIndex :: Int,
    placeAddress :: Text,
    placeName :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data SuggestedSearches = SuggestedSearches
  { keyword :: Text,
    identifier :: Text,
    location :: Text,
    hyperlink :: Text,
    orderIndex :: Int,
    eLoc :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype LatLongList = LatLongList {getLatLongList :: [LatLong]}

instance ToHttpApiData LatLongList where
  toUrlPiece (LatLongList lst) = T.intercalate ";" $ map convertPoint lst

convertPoint :: LatLong -> Text
convertPoint (LatLong lat lon) = show lon <> "," <> show lat

data DistanceMatrixResp = DistanceMatrixResp
  { version :: Text,
    results :: DistanceMatrixResult,
    responseCode :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

data DistanceMatrixResult = DistanceMatrixResult
  { code :: Text,
    distances :: [[Double]],
    durations :: [[Double]]
  }
  deriving (Generic, FromJSON, ToJSON)

data DistanceMatrixElement = DistanceMatrixElement
  { distance :: Maybe TextValue,
    duration :: Maybe TextValue,
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

data TextValue = TextValue
  { text :: Text,
    value :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RouteResponse = RouteResponse
  { code :: Text,
    routes :: [Routes],
    waypoints :: [WayPoints]
  }
  deriving (Generic, FromJSON, ToJSON)

data Routes = Routes
  { legs :: [Legs],
    weight_name :: Text,
    geometry :: PP.PolyLinePoints,
    weight :: Double,
    distance :: Double,
    duration :: Double
  }
  deriving (Generic, FromJSON, ToJSON)

data Legs = Legs
  { steps :: [Steps],
    weight :: Double,
    distance :: Double,
    summary :: Text,
    duration :: Double
  }
  deriving (Generic, FromJSON, ToJSON)

data Steps = Steps
  { intersections :: [Intersections],
    driving_side :: Text,
    mode :: Text,
    maneuver :: Maneuver,
    distance :: Double,
    duration :: Double,
    geometry :: PP.PolyLinePoints,
    name :: Text
  }
  deriving (Generic)

instance FromJSON Steps where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Steps where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Maneuver = Maneuver
  { location :: LngLat,
    bearing_before :: Int,
    bearing_after :: Int,
    modifier :: Text,
    _type :: Text
  }
  deriving (Generic)

instance FromJSON Maneuver where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Maneuver where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data WayPoints = WayPoints
  { hint :: Text,
    location :: LngLat,
    name :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data Intersections = Intersections
  { location :: LngLat,
    bearings :: [Int],
    entry :: [Bool]
  }
  deriving (Generic)

instance FromJSON Intersections where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Intersections where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype LngLat = LngLat {getLatLong :: LatLong}
  deriving stock (Generic, Show, Eq)
  deriving (PrettyShow) via (Showable LngLat)

instance FromJSON LngLat where
  parseJSON = withArray "array [lon, lat]" $ \arr_ -> case toList arr_ of
    [lon, lat] -> LngLat ... LatLong <$> parseJSON lat <*> parseJSON lon
    _ -> fail "expected array [lon, lat]"

instance ToJSON LngLat where
  toJSON (LngLat LatLong {..}) = Array $ V.fromList $ map toJSON [lon, lat]
