{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Search where

import qualified Data.List as DL
import EulerHS.Prelude
import Kernel.Types.Common
import qualified Tools.Maps as Maps

findMaxWeightedInfoIdx :: Float -> Float -> [(Maybe Float, Maybe Float)] -> Int
findMaxWeightedInfoIdx distanceWeight durationWeight infoArr =
  fst $ DL.maximumBy (comparing snd) indexedScores
  where
    indexedScores = zip [0 ..] (map (\(dis, dur) -> calculateWeightedScore distanceWeight durationWeight (dis, dur)) infoArr)

minDistance :: [Maps.RouteInfo] -> Meters
minDistance = DL.minimum . mapMaybe (.distance)

minDuration :: [Maps.RouteInfo] -> Seconds
minDuration = DL.minimum . mapMaybe (.duration)

normalizeArr :: Maybe Meters -> Maybe Seconds -> [Maps.RouteInfo] -> [(Maybe Float, Maybe Float)]
normalizeArr minD minDur infoArr =
  [normalizeInfo minD minDur info | info <- infoArr]

normalizeInfo :: Maybe Meters -> Maybe Seconds -> Maps.RouteInfo -> (Maybe Float, Maybe Float)
normalizeInfo minD minDur info =
  (normalizeDisValue info.distance minD, normalizeDurValue info.duration minDur)

normalizeDisValue :: Maybe Meters -> Maybe Meters -> Maybe Float
normalizeDisValue (Just x) (Just minVal)
  | x /= 0 = Just (fromIntegral minVal / fromIntegral x)
normalizeDisValue _ _ = Nothing

normalizeDurValue :: Maybe Seconds -> Maybe Seconds -> Maybe Float
normalizeDurValue (Just x) (Just minVal)
  | x /= 0 = Just (fromIntegral minVal / fromIntegral x)
normalizeDurValue _ _ = Nothing

calculateWeightedScore :: Float -> Float -> (Maybe Float, Maybe Float) -> Float
calculateWeightedScore distanceWeight durationWeight (dis, dur) =
  case (dis, dur) of
    (Just normDist, Just normDur) -> normDist * distanceWeight + normDur * durationWeight
    _ -> 0
