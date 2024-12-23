module Lib.JourneyModule.Base where

import qualified Storage.Queries.Journey as QJourney
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import qualified Domain.Types.Trip as DTrip
import Kernel.Utils.Common
import qualified Kernel.External.MultiModal.Interface as ExternalInterface
import qualified Storage.Journey.Queries as JQ
import qualified Domain.Types.Journey as DJ
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Lib.JourneyModule.Types

init :: JourneyLeg leg m => JourneyInitData leg -> m Journey
init journeyReq = do
  journeyId <- generateGUID
  let journeyLegsCount = length journeyReq.legs
      modes = map (\x -> convertMultiModalModeToTripMode x.mode (distanceToMeters x.distance) journeyReq.maximumWalkDistance) journeyReq.legs
  let journey =
        DJourney.Journey
          { convenienceCost = 0,
            estimatedDistance = journeyReq.estimatedDistance,
            estimatedDuration = Just journeyReq.estiamtedDuration,
            estimatedFare = Nothing,                                 -- call fare api
            fare = Nothing,
            id = Id journeyId,
            legsDone = 0,
            totalLegs = journeyLegsCount,
            modes = modes,
            searchRequestId = journeyReq.parentSearchId,
            merchantId = Just journeyReq.merchantId,
            merchantOperatingCityId = Just journeyReq.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QJourney.create journey

  mapWithIndex
    ( \idx leg -> do
        journeyLegId <- generateGUID

        let journeyLeg <-
              DJourenyLeg.JourneyLeg
                { agency = leg.agency,
                  distance = leg.distance,
                  duration = leg.duration,
                  endLocation = leg.endLocation,
                  fromArrivalTime = leg.fromArrivalTime,
                  fromDepartureTime = leg.fromDepartureTime,
                  fromStopDetails = leg.fromStopDetails,
                  id = journeyLegId,
                  journeyId,
                  mode = leg.mode,
                  polylinePoints = leg.polyline.encodedPolyline,
                  routeDetails = leg.routeDetails,
                  sequenceNumber = idx,
                  startLocation = leg.startLocation.latLng,
                  toArrivalTime = leg.toArrivalTime,
                  toDepartureTime = leg.toDepartureTime,
                  toStopDetails = leg.toStopDetails.latLng,
                  merchantId = Just journeyReq.merchantId,
                  merchantOperatingCityId = Just journeyReq.merchantOperatingCityId,
                  createdAt = currentTime,
                  updatedAt = currentTime
                }
        QJourneyLeg.create journeyLeg
    )
    journeyReq.legs

    logDebug $ "journey for multi-modal: " <> show journey

    return journey

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f xs = go 0 xs
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> DTrip.TravelMode
convertMultiModalModeToTripMode input distance maximumWalkDistance = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Walk -> if (distance > maximumWalkDistance) then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi

getJourney :: Id Journey -> m Journey
getJourney id = JQ.findById id >>= fromMaybeM (JourneyNotFound id.getId)

getAllLegs :: Id Journey -> m LegInfo
getAllLegs journeyId = do
  taxiLegs <- QSearchRequest.findAllByJourneyId journeyId >>= mapM mkLegInfoFromSearchRequest
  publicTransportLegs <- QFRFSSearch.findAllByJourneyId journeyId >>= mapM mkLegInfoFromFrfsSearchRequest
  return $ sortBy (.order) (taxiLegs <> publicTransportLegs)

startJourney :: Id Journey -> ConfirmReq -> m () -- confirm request
startJourney journeyId = do
  allLegs <- getAllLegs journeyId
  mapM (JL.confirm . mkConfirmReq) allLegs

addLeg :: JourneyLeg leg m => leg -> m ()
addLeg leg = JL.search leg -- output could be the search request

getCurrentLeg :: JourneyLeg leg m => Journey -> m leg
getCurrentLeg
  -- get the current leg based on status
  -- loop through all legs and based on getState
  journeyLegs <- getAllLegs journeyId
  let currentLeg = find (\leg -> notElem leg.status [completedStatus]) journeyLegs
  return currentLeg

getRemainingLegs :: JourneyLeg leg m => Id Journey -> m [leg]
getRemainingLegs journeyId = do
  -- get the remaining legs
  journeyLegs <- getAllLegs journeyId
  let remainingLegs = filter (\leg -> notElem leg.status [completedStatus]) journeyLegs
  return remainingLegs
--   filter -> based on status

-- createLeg :: RequiedData (OTP response for each leg)
deleteLeg :: JourneyLeg leg m => leg -> m ()
  JL.cancel leg
  -- remove it from the journey mark it deleted/skipped

updateLeg :: JourneyLeg leg m => UpdateJourneyLeg -> leg -> m ()
  -- update the leg
  -- update it in the journey

skipJourney :: Journey -> [leg] -> m ()
-- skipJourney journey
    -- getRemainingLegs
    map update [leg]
    -- @@ call cancel for current leg

endJourney :: Journey -> m ()
endJourney
-- if last leg then update leg
-- loop through and delete/update legs and journey as required
-- call leg level cancel

replaceLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m () -- leg2 can be an array
replaceLeg journey oldLegs newLeg =
  forM_ (deleteLeg journey) oldLegs >> addLeg journey newLeg

extendLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m ()
  forM_ (deleteLeg journey) oldLegs >> updateLeg journey newLeg
