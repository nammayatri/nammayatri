module API.DistanceMatrix
  ( handler,
  )
where

import Beckn.External.Maps.Google.MapsClient.Types as GoogleMaps
import Beckn.Prelude
import Beckn.Utils.Common
import qualified Domain.Types.MockPlace as DPlace
import Environment
import qualified MockData.Common as Data
import qualified MockData.DistanceMatrix as Data
import Tools.Error

handler ::
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Text ->
  Maybe GoogleMaps.Mode ->
  FlowHandler GoogleMaps.DistanceMatrixResp
handler origins destinations key mode = withFlowHandlerAPI $ do
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
    else buildMatrix origins destinations

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

buildMatrix ::
  MonadFlow m =>
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  m GoogleMaps.DistanceMatrixResp
buildMatrix origins destinations = do
  mockOrigins <- forM origins $ \origin -> do
    DPlace.lookupPlace origin Data.availablePlaces & fromMaybeM (NotImplemented $ "distanceMatrix is not implemented: origin: " <> show origin)
  mockDestinations <- forM destinations $ \destination -> do
    DPlace.lookupPlace destination Data.availablePlaces & fromMaybeM (NotImplemented $ "distanceMatrix is not implemented: destination: " <> show destination)
  rows <-
    forM mockOrigins $ \mockOrigin -> do
      elementsList <- forM mockDestinations $ \mockDestination -> do
        let mbElement = DPlace.lookupDistanceMatrixElement (mockOrigin.placeId, mockDestination.placeId) Data.distanceMatrixElements
        case mbElement of
          Nothing ->
            if mockOrigin.placeId == mockDestination.placeId
              then pure Data.defaultElement
              else throwError (NotImplemented $ "distanceMatrix is not implemented: origin: " <> show mockOrigin.place <> "; destination: " <> show mockDestination.place)
          Just element -> pure element
      pure $ DistanceMatrixRow {elements = elementsList}
  pure $
    DistanceMatrixResp
      { destination_addresses = mockDestinations <&> (.placeName),
        origin_addresses = mockOrigins <&> (.placeName),
        rows = rows,
        status = "OK"
      }
