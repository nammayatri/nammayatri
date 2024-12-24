module Lib.JourneyModule.Base where

import qualified Storage.Queries.Journey as QJourney
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Kernel.External.MultiModal.Interface as ExternalInterface
import qualified Storage.Journey.Queries as JQ
import qualified Domain.Types.Journey as DJ
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Lib.JourneyModule.Types
import Lib.JourneyModule.Utils as Utils
import qualified Domain.Types.Trip as DTrip
import SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Kernel.Types.Common
import qualified Storage.Merchant.Queries as QMerchant

init :: JourneyLeg leg m => JourneyInitData leg -> m Journey
init journeyReq = do
  journeyId <- generateGUID
  let journeyLegsCount = length journeyReq.legs
      modes = map (\x -> Utils.convertMultiModalModeToTripMode x.mode (distanceToMeters x.distance) journeyReq.maximumWalkDistance) journeyReq.legs

  totalFares <-
    Utils.mapWithIndex
      ( \idx leg -> do
          journeyLegId <- generateGUID
          tripMode <- Utils.convertMultiModalModeToTripMode $ leg.mode (distanceToMeters leg.distance) journeyReq.maximumWalkDistance
          merchant <- QMerchant.findById journeyReq.merchantId >>= fromMaybeM (MerchantDoesNotExist $ "merchantId:- " <> merchantId.getId)
          merchantOpCity <- CQMOC.findById journeyReq.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOpCityId: " <> journeyReq.merchantOperatingCityId.getId)
          let getFareReq =
            { startLocation = leg.startLocation,
              endLocation = leg.endLocation,
              distance
            }
          getFareReq <-
            case tripMode of
              DTrip.Taxi -> 
                TaxiLegGetFareRequest $ TaxiGetFareData
                  { startLocation = leg.startLocation,
                    endLocation = leg.endLocation,
                    distance = leg.distance,
                    duration = leg.duration,
                    merchant,
                    merchantOpCity
                  }
              Dtrip.Bus -> 
                BusLegGetFareRequest $ BusGetFareData
                  { startLocation = leg.startLocation,
                    endLocation = leg.endLocation
                  }
              Dtrip.Metro -> 
                MetroLegGetFareRequest $ MetroGetFareData
                  { startLocation = leg.startLocation,
                    endLocation = leg.endLocation
                  }
              Dtrip.Walk -> 
                WalkLegGetFareRequest $ WalkGetFareData
                  { startLocation = leg.startLocation,
                    endLocation = leg.endLocation
                  }
          totalLegFare <- JourneyLegTypes.getFare getFareReq
          let journeyLeg <- mkJourneyLeg leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId
          QJourneyLeg.create journeyLeg

          return totalLegFare
      )
      journeyReq.legs

  journey <- mkJourney journeyReq.estimatedDistance journeyReq.estiamtedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId totalFares journeyReq.legs journeyReq.maximumWalkDistance
  QJourney.create journey

  logDebug $ "journey for multi-modal: " <> show journey
  return journey

getJourney :: Id Journey -> m Journey
getJourney id = JQ.findById id >>= fromMaybeM (JourneyNotFound id.getId)

-- Return only the OTP raw data without the search and bookings
getJourneyLegs :: Id Journey -> m [JourneyLeg]
getJourneyLegs journeyId = QJourneyLeg.findAllByJourneyId journeyId

-- Return complete data with search and bookings
getAllLegsInfo :: Id Journey -> m [LegInfo]
getAllLegsInfo journeyId = do
  allLegsRawData <- getJourneyLegs journeyId
  allLegsInfo <- allLegsRawData `forM` \leg -> do
    case leg.mode of
      Trip.Taxi -> JL.get $ TaxiGetStateRequest leg.legId
      Trip.Walk -> JL.get $ WalkGetStateRequest leg.legId
      Trip.Metro -> JL.get $ MetroGetStateRequest leg.legId
      _ -> throwError $ InvalidRequest ("Mode " <> show leg.mode <> " not supported!")
  return $ sortBy (.order) allLegsInfo

startJourney :: Id Journey -> ConfirmReq -> m () -- confirm request
startJourney journeyId = do
  allLegs <- getAllLegsInfo journeyId
  mapM (JL.confirm . mkConfirmReq) allLegs

addAllLegs :: Id Journey -> [JourneyLegsReq] -> m ()
addAllLegs journeyId legsReq = do
  journey <- getJourney journeyId
  journeyLegs <- getJourneyLegs journeyId
  journeyLegs `forM` \journeyLeg -> do
    case journeyLeg.mode of
      Taxi -> do
        currentLegReq <- find (\lg -> lg.legNumber == journeyLeg.sequenceNumber) legsReq & fromMaybeM (JourneyLegReqDataNotFound journeyLeg.sequenceNumber)
        addTaxiLeg journey journeyLeg currentLegReq
      _ -> return () -- handle metro and other cases

addTaxiLeg :: Journey -> JourneyLeg -> JourneyLegsReq -> m ()
addTaxiLeg journey journeyLeg currentLegReq = do
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId
  let startLocation = mkSearchReqLocation currentLegReq.originAddress journeyLeg.startLocation
  let endLocation = mkSearchReqLocation currentLegReq.destinationAddress journeyLeg.endLocation
  taxiSearchReq <- mkTaxiSearchReq parentSearchReq journeyLeg (TaxiSearchRequestData { origin = startLocation, stops = [endLocation] })
  JL.search taxiSearchReq

getCurrentLeg :: JourneyLeg leg m => Id Journey -> Maybe [leg] -> m leg
getCurrentLeg journeyId mbJourneyLegs = do
  journeyLegs <- maybe (getAllLegs journeyId) return mbJourneyLegs
  let currentLeg = find (\leg -> notElem leg.status [completedStatus]) journeyLegs
  return currentLeg

getRemainingLegs :: JourneyLeg leg m => Id Journey -> m [leg]
getRemainingLegs journeyId = do
  journeyLegs <- getAllLegs journeyId
  let remainingLegs = dropWhile (\leg -> notElem leg.status [completedStatus]) journeyLegs -- check if edge case is to be handled [completed , skipped, inplan]
  return remainingLegs

deleteLeg :: JourneyLeg leg m => leg -> m ()
deleteLeg leg = do
  let cancelReq = mkCancelReq leg
  JL.cancel cancelReq

updateLeg :: JourneyLeg leg m => leg -> leg -> m ()
updateLeg 
  let updateReq = mkUpdateReq leg
  JL.update leg

-- skipJourney :: Journey -> [leg] -> m ()
-- skipJourney journey
    -- getRemainingLegs
    -- map update [leg]
    -- @@ call cancel for current leg

-- endJourney :: Journey -> m ()
-- endJourney
-- if last leg then update leg
-- loop through and delete/update legs and journey as required
-- call leg level cancel

replaceLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m () -- leg2 can be an array
replaceLeg journey oldLegs newLeg =
  forM_ (deleteLeg journey) oldLegs >> addLeg journey newLeg

extendLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m ()
extendLeg journey oldLegs newLeg =
  forM_ (deleteLeg journey) oldLegs >> updateLeg journey newLeg

