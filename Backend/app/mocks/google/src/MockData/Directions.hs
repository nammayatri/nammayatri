{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module MockData.Directions (mkDirectionsResp, mkAdvancedDirectionsResp) where

import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import Kernel.External.Maps.Google.PolyLinePoints (encode)
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)

-- route constructed from two points - origin and destination, its enough for tests
mkDirectionsResp :: GoogleMaps.LocationS -> GoogleMaps.LocationS -> GoogleMaps.DirectionsResp
mkDirectionsResp origin destination =
  GoogleMaps.DirectionsResp
    { status = "OK",
      routes = [mkMockRoutesResp origin destination]
    }

mkMockRoutesResp :: GoogleMaps.LocationS -> GoogleMaps.LocationS -> GoogleMaps.Route
mkMockRoutesResp origin destination =
  GoogleMaps.Route
    { bounds = mkBounds origin destination,
      legs = [mkLeg origin destination]
    }

mkBounds :: GoogleMaps.LocationS -> GoogleMaps.LocationS -> GoogleMaps.Bounds
mkBounds origin destination =
  GoogleMaps.Bounds
    { northeast = if origin.lat > destination.lat then origin else destination,
      southwest = if origin.lat < destination.lat then origin else destination
    }

mkLeg :: GoogleMaps.LocationS -> GoogleMaps.LocationS -> GoogleMaps.Leg
mkLeg origin destination = do
  let distanceInMeters = highPrecMetersToMeters $ distanceBetweenInMeters (mkLatLong origin) (mkLatLong destination)
  let speed = 10 -- meters per second
  let distance =
        GoogleMaps.TextValue
          { text = show distanceInMeters <> " m",
            value = distanceInMeters.getMeters
          }
  let duration =
        GoogleMaps.TextValue
          { text = show (distanceInMeters.getMeters `div` speed `div` 60) <> " min",
            value = distanceInMeters.getMeters `div` speed
          }
  GoogleMaps.Leg
    { distance,
      duration,
      end_location = destination,
      start_location = origin,
      steps = [mkStep origin destination distance duration]
    }

mkLatLong :: GoogleMaps.LocationS -> Maps.LatLong
mkLatLong location =
  Maps.LatLong
    { lat = location.lat,
      lon = location.lng
    }

mkStep :: GoogleMaps.LocationS -> GoogleMaps.LocationS -> GoogleMaps.TextValue -> GoogleMaps.TextValue -> GoogleMaps.Step
mkStep origin destination distance duration =
  GoogleMaps.Step
    { distance,
      duration,
      end_location = destination,
      polyline = GoogleMaps.EncodedPointObject {points = encode [mkLatLong origin, mkLatLong destination]},
      start_location = origin,
      travel_mode = GoogleMaps.DRIVING
    }

-- Advanced Directions (v2 computeRoutes) mock response
mkAdvancedDirectionsResp :: GoogleMaps.LatLngV2 -> GoogleMaps.LatLngV2 -> GoogleMaps.AdvancedDirectionsResp
mkAdvancedDirectionsResp origin destination =
  GoogleMaps.AdvancedDirectionsResp
    { routes = [mkRouteV2 origin destination]
    }

mkRouteV2 :: GoogleMaps.LatLngV2 -> GoogleMaps.LatLngV2 -> GoogleMaps.RouteV2
mkRouteV2 origin destination =
  let dist = highPrecMetersToMeters $ distanceBetweenInMeters (mkLatLongV2 origin) (mkLatLongV2 destination)
      speed = 10 -- meters per second
      durationSecs = dist.getMeters `div` speed
   in GoogleMaps.RouteV2
        { legs = [mkLegV2 origin destination dist durationSecs],
          viewport = mkViewPort origin destination,
          distanceMeters = dist.getMeters,
          duration = show durationSecs <> "s",
          staticDuration = Just $ show durationSecs <> "s"
        }

mkViewPort :: GoogleMaps.LatLngV2 -> GoogleMaps.LatLngV2 -> GoogleMaps.ViewPort
mkViewPort origin destination =
  GoogleMaps.ViewPort
    { low =
        GoogleMaps.LatLngV2
          { latitude = min origin.latitude destination.latitude,
            longitude = min origin.longitude destination.longitude
          },
      high =
        GoogleMaps.LatLngV2
          { latitude = max origin.latitude destination.latitude,
            longitude = max origin.longitude destination.longitude
          }
    }

mkLegV2 :: GoogleMaps.LatLngV2 -> GoogleMaps.LatLngV2 -> Meters -> Int -> GoogleMaps.LegV2
mkLegV2 origin destination dist durationSecs =
  GoogleMaps.LegV2
    { distanceMeters = dist.getMeters,
      duration = show durationSecs <> "s",
      startLocation = GoogleMaps.LocationV2 {latLng = origin},
      endLocation = GoogleMaps.LocationV2 {latLng = destination},
      steps = [mkStepV2 origin destination dist durationSecs]
    }

mkStepV2 :: GoogleMaps.LatLngV2 -> GoogleMaps.LatLngV2 -> Meters -> Int -> GoogleMaps.StepV2
mkStepV2 origin destination dist durationSecs =
  GoogleMaps.StepV2
    { distanceMeters = dist.getMeters,
      staticDuration = show durationSecs <> "s",
      startLocation = GoogleMaps.LocationV2 {latLng = origin},
      endLocation = GoogleMaps.LocationV2 {latLng = destination},
      polyline = GoogleMaps.Polyline {encodedPolyline = encode [mkLatLongV2 origin, mkLatLongV2 destination]},
      travelMode = GoogleMaps.DRIVE,
      transitDetails = Nothing
    }

mkLatLongV2 :: GoogleMaps.LatLngV2 -> Maps.LatLong
mkLatLongV2 loc =
  Maps.LatLong
    { lat = loc.latitude,
      lon = loc.longitude
    }
