{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Client-facing projection of a persisted 'Domain.Types.RouteDetails.RouteDetails' row (one
-- transit sub-leg). Shared by the multimodal journey response ('Domain.Action.UI.Quote') and the
-- FRFS quote response so both surface route details in the exact same shape. Lives in a leaf
-- @Domain.Types@ module (only @Text@/@LatLong@ in the type) so the generated FRFS API types can
-- import it without an import cycle through the journey modules.
module Domain.Types.RouteDetailsAPI where

import Control.Applicative ((<|>))
import Data.Aeson
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import Domain.Types.RouteDetails (RouteDetails)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude

data RouteDetail = RouteDetail
  { routeCode :: Maybe Text,
    fromStationCode :: Maybe Text,
    toStationCode :: Maybe Text,
    alternateShortNames :: [Text],
    alternateRouteIds :: Maybe [Text],
    color :: Maybe Text,
    colorCode :: Maybe Text,
    fromStationLatLong :: LatLong,
    toStationLatLong :: LatLong,
    fromStationPlatformCode :: Maybe Text,
    toStationPlatformCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkRouteDetail :: RouteDetails -> RouteDetail
mkRouteDetail routeDetail =
  RouteDetail
    { routeCode = gtfsIdtoDomainCode <$> routeDetail.routeGtfsId,
      fromStationCode = (gtfsIdtoDomainCode <$> routeDetail.fromStopCode) <|> (gtfsIdtoDomainCode <$> routeDetail.fromStopGtfsId),
      toStationCode = (gtfsIdtoDomainCode <$> routeDetail.toStopCode) <|> (gtfsIdtoDomainCode <$> routeDetail.toStopGtfsId),
      color = routeDetail.routeShortName,
      colorCode = routeDetail.routeShortName,
      alternateShortNames = routeDetail.alternateShortNames,
      alternateRouteIds = routeDetail.alternateRouteIds,
      fromStationLatLong =
        LatLong
          { lat = routeDetail.startLocationLat,
            lon = routeDetail.startLocationLon
          },
      toStationLatLong =
        LatLong
          { lat = routeDetail.endLocationLat,
            lon = routeDetail.endLocationLon
          },
      fromStationPlatformCode = routeDetail.fromStopPlatformCode,
      toStationPlatformCode = routeDetail.toStopPlatformCode
    }
