{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.DistanceMatrix
  ( handler,
  )
where

import qualified Domain.Types.MockPlace as DPlace
import Environment
import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import Kernel.Prelude
import Kernel.Utils.Common
import qualified MockData.Common as Data
import qualified MockData.DistanceMatrix as Data
import Tools.Error

handler ::
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Text ->
  Maybe GoogleMaps.Mode ->
  Maybe Text ->
  FlowHandler GoogleMaps.DistanceMatrixResp
handler origins destinations key mode _ = withFlowHandlerAPI' $ do
  unless (key == Data.mockKey) $ throwError AccessDenied
  unless (isNothing mode || mode == Just GoogleMaps.DRIVING) $
    throwError $ NotImplemented $ "distanceMatrix is not implemented: mode: " <> show mode
  let originsLength = length origins
  let destinationsLength = length destinations
  when (originsLength > 25) $ throwError $ InvalidRequest "Maximum of 25 origins per request."
  when (destinationsLength > 25) $ throwError $ InvalidRequest "Maximum of 25 destinations per request."
  when (originsLength * destinationsLength > 100) $ throwError $ InvalidRequest "Maximum 100 elements per request."

  if allEqual $ origins <> destinations
    then pure $ mkDefaultMatrix origins destinations
    else case buildMatrixMaybe origins destinations of
      Just resp -> pure resp
      Nothing -> pure $ mkDefaultMatrix origins destinations

allEqual :: [GoogleMaps.Place] -> Bool
allEqual [] = True
allEqual (place : places) = all (DPlace.comparePlaces place) places

mkDefaultMatrix ::
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  GoogleMaps.DistanceMatrixResp
mkDefaultMatrix origins destinations = do
  let mockOrigins = Data.mkDefaultPlace <$> origins
      mockDestinations = Data.mkDefaultPlace <$> destinations
      elementsList = replicate (length mockDestinations) Data.defaultElement
      row = DistanceMatrixRow {elements = elementsList}
      rows = replicate (length mockOrigins) row
  DistanceMatrixResp
    { destination_addresses = mockDestinations <&> (.placeName),
      origin_addresses = mockOrigins <&> (.placeName),
      rows = rows,
      status = "OK"
    }

-- | Try to build matrix from known places, return Nothing if any place is unknown
buildMatrixMaybe ::
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Maybe GoogleMaps.DistanceMatrixResp
buildMatrixMaybe origins destinations = do
  mockOrigins <- forM origins $ \origin -> DPlace.lookupPlace origin Data.availablePlaces
  mockDestinations <- forM destinations $ \destination -> DPlace.lookupPlace destination Data.availablePlaces
  rows <-
    forM mockOrigins $ \mockOrigin -> do
      elementsList <- forM mockDestinations $ \mockDestination -> do
        let mbElement = DPlace.lookupDistanceMatrixElement (mockOrigin.placeId, mockDestination.placeId) Data.distanceMatrixElements
        case mbElement of
          Nothing ->
            if mockOrigin.placeId == mockDestination.placeId
              then pure Data.defaultElement
              else Nothing
          Just element -> pure element
      pure $ DistanceMatrixRow {elements = elementsList}
  pure $
    DistanceMatrixResp
      { destination_addresses = mockDestinations <&> (.placeName),
        origin_addresses = mockOrigins <&> (.placeName),
        rows = rows,
        status = "OK"
      }
