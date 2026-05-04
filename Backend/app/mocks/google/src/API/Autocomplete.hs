{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Autocomplete
  ( handler,
  )
where

import Control.Applicative ((<|>))
import Environment
import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified MockData.Common as Data
import Tools.Error
import qualified Tools.OSRM as OSRM
import qualified Tools.PlaceCache as PlaceCache

handler ::
  Text ->
  Language ->
  GoogleMaps.AutoCompleteReqV2 ->
  FlowHandler GoogleMaps.AutoCompleteRespV2
handler key _languageCode req = withFlowHandlerAPI' $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  -- Resolve the centre point we cluster suggestions around. Caller may
  -- send any of: explicit `origin`, a `locationBias.circle.center`, or a
  -- `locationRestriction.circle.center`. If none, fall back to the
  -- single-suggestion echo behaviour (no real distance computable).
  let mbCentre = chooseCentre req
  case mbCentre of
    Nothing -> pure $ singleEcho req.input
    Just centre -> do
      let suggestions = synthesiseSuggestions centre req.input
      -- Try OSRM table once: row 0 = distances FROM the centre TO each
      -- synthetic point. Falls back to straight-line haversine if OSRM
      -- is unreachable.
      mbTable <-
        liftIO $
          OSRM.getTable $
            (centre.latitude, centre.longitude) :
            map (\(_, la, lo) -> (la, lo)) suggestions
      let osrmRow = case mbTable of
            Just t -> case OSRM.distancesMatrix t of
              (row : _) -> drop 1 row
              [] -> []
            Nothing -> []
      -- Per-cell: prefer OSRM's road distance; else fall back to
      -- haversine. Guarantees every prediction has a non-zero distance
      -- (haversine of an offset point against the centre is always > 0).
      let pickDist sugg mbOsrm =
            case mbOsrm of
              Just d | d > 0 -> d
              _ -> haversineMeters centre sugg
      let dists =
            zipWith
              pickDist
              suggestions
              (osrmRow <> repeat Nothing)
      let preds = zipWith (mkPrediction centre) suggestions (map Just dists)
      -- Cache (placeId → latlon) so PlaceName can resolve back later.
      liftIO $ forM_ preds cachePlace
      pure $ GoogleMaps.AutoCompleteRespV2 (map mkSuggestion preds)

-- ─── synthesis ───────────────────────────────────────────────────────

-- (label, lat, lon) — the offsets give a small ring of points around
-- the centre at varying distances/bearings so the rider-app shows a
-- variety of "X away" hints, not all the same number.
synthesiseSuggestions :: GoogleMaps.LatLngV2 -> Text -> [(Text, Double, Double)]
synthesiseSuggestions centre input =
  let mk dLat dLon suffix = (input <> suffix, centre.latitude + dLat, centre.longitude + dLon)
   in -- Every offset is non-zero so haversine and OSRM both produce a
      -- non-zero distance. Bearings spread roughly NE/SE/W/NW so the
      -- rider-app's "X km away" hints look varied.
      [ mk    0.0010    0.0010  ""
      , mk    0.0030    0.0040  " · Centre"
      , mk    0.0070   (-0.0050) " · Junction"
      , mk  (-0.0090)   0.0080  " · Plaza"
      , mk    0.0150   (-0.0100) " · Market"
      ]

-- ─── output construction ─────────────────────────────────────────────

data Pred = Pred
  { label :: Text,
    lat :: Double,
    lon :: Double,
    placeId :: Text,
    distanceMeters :: Maybe Int
  }

mkPrediction ::
  GoogleMaps.LatLngV2 ->
  (Text, Double, Double) ->
  Maybe Double ->
  Pred
mkPrediction _ (label, lat, lon) mbDist =
  Pred
    { label,
      lat,
      lon,
      placeId = PlaceCache.encodeMockPlaceId lat lon,
      distanceMeters = round <$> mbDist
    }

mkSuggestion :: Pred -> GoogleMaps.Suggestion
mkSuggestion p =
  GoogleMaps.Suggestion
    { placePrediction =
        GoogleMaps.PlacePrediction
          { text = GoogleMaps.PlaceText {text = p.label},
            placeId = Just p.placeId,
            types = Just ["geocode"],
            distanceMeters = p.distanceMeters
          }
    }

cachePlace :: Pred -> IO ()
cachePlace p = PlaceCache.insertPlace p.placeId (p.lat, p.lon)

-- ─── helpers ─────────────────────────────────────────────────────────

singleEcho :: Text -> GoogleMaps.AutoCompleteRespV2
singleEcho input =
  GoogleMaps.AutoCompleteRespV2
    [ GoogleMaps.Suggestion
        { placePrediction =
            GoogleMaps.PlacePrediction
              { text = GoogleMaps.PlaceText {text = input},
                placeId = Just ("mock-place-" <> input),
                types = Just ["geocode"],
                distanceMeters = Just 0
              }
        }
    ]

chooseCentre :: GoogleMaps.AutoCompleteReqV2 -> Maybe GoogleMaps.LatLngV2
chooseCentre req =
  req.origin
    <|> ((.center) . (.circle) <$> req.locationBias)
    <|> ((.center) . (.circle) <$> req.locationRestriction)

-- Straight-line fallback distance in metres if OSRM is down. Equirectangular
-- projection — accurate enough for the small offsets we use here.
haversineMeters :: GoogleMaps.LatLngV2 -> (Text, Double, Double) -> Double
haversineMeters c (_, la, lo) =
  let toRad d = d * pi / 180
      dLat = toRad (la - c.latitude)
      dLon = toRad (lo - c.longitude)
      a =
        sin (dLat / 2) ** 2
          + cos (toRad c.latitude) * cos (toRad la) * sin (dLon / 2) ** 2
      c2 = 2 * atan2 (sqrt a) (sqrt (1 - a))
   in 6371000 * c2

