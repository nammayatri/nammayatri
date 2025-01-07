module Lib.JourneyModule.Base where

import qualified API.Types.UI.MultimodalConfirm as MultimodalConfirm
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Trip as DTrip
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Bus ()
import qualified Lib.JourneyLeg.Interface as JLI
import Lib.JourneyLeg.Metro ()
import Lib.JourneyLeg.Taxi ()
import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyLeg.Walk ()
import qualified Lib.JourneyModule.Types as JL
import Lib.JourneyModule.Utils
import qualified Storage.Queries.Journey as JQ
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.SearchRequest as QSearchRequest

init ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    JL.JourneyLeg TaxiLegRequest m,
    JL.JourneyLeg BusLegRequest m,
    JL.JourneyLeg MetroLegRequest m,
    JL.JourneyLeg WalkLegRequest m
  ) =>
  JL.JourneyInitData ->
  m DJourney.Journey
init journeyReq = do
  journeyId <- Common.generateGUID
  totalFares <-
    mapWithIndex
      ( \idx leg -> do
          let travelMode = convertMultiModalModeToTripMode leg.mode (distanceToMeters leg.distance) journeyReq.maximumWalkDistance
          totalLegFare <- JLI.getFare journeyReq.merchantId journeyReq.merchantOperatingCityId leg travelMode
          journeyLeg <- JL.mkJourneyLeg idx leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance
          QJourneyLeg.create journeyLeg
          return totalLegFare
      )
      journeyReq.legs
  journey <- JL.mkJourney journeyReq.estimatedDistance journeyReq.estimatedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId totalFares journeyReq.legs journeyReq.maximumWalkDistance
  QJourney.create journey
  logDebug $ "journey for multi-modal: " <> show journey
  return journey

getJourney :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m DJourney.Journey
getJourney id = JQ.findByPrimaryKey id >>= fromMaybeM (InternalError "JourneyNotFound id.getId")

getJourneyLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m [DJourneyLeg.JourneyLeg]
getJourneyLegs = QJourneyLeg.findAllByJourneyId

getAllLegsInfo ::
  ( EsqDBFlow m r,
    Monad m,
    CacheFlow m r,
    JL.JourneyLeg TaxiLegRequest m,
    JL.JourneyLeg BusLegRequest m,
    JL.JourneyLeg MetroLegRequest m,
    JL.JourneyLeg WalkLegRequest m,
    EncFlow m r
  ) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getAllLegsInfo journeyId = do
  allLegsRawData <- getJourneyLegs journeyId
  allLegsInfo <-
    allLegsRawData `forM` \leg -> do
      case leg.legId of
        Just legIdText -> do
          let legId = Id legIdText
          case leg.mode of
            DTrip.Taxi -> JL.getInfo $ TaxiLegRequestGetInfo $ TaxiLegRequestGetInfoData {searchId = cast legId}
            DTrip.Walk -> JL.getInfo $ WalkLegRequestGetInfo $ WalkLegRequestGetInfoData {walkLegId = cast legId}
            DTrip.Metro -> JL.getInfo $ MetroLegRequestGetInfo $ MetroLegRequestGetInfoData {searchId = cast legId}
            _ -> throwError $ InvalidRequest ("Mode " <> show leg.mode <> " not supported!")
        Nothing -> throwError $ InvalidRequest ("LegId null for Mode: " <> show leg.mode)
  return $ sortBy (comparing (.order)) allLegsInfo

startJourney ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    JL.JourneyLeg TaxiLegRequest m,
    JL.JourneyLeg BusLegRequest m,
    JL.JourneyLeg MetroLegRequest m,
    JL.JourneyLeg WalkLegRequest m
  ) =>
  Id DJourney.Journey ->
  m () -- confirm request
startJourney _ = do
  -- startJourney journeyId = do
  --allLegs <- getAllLegsInfo journeyId
  --mapM_ (JL.confirm . mkConfirmReq) allLegs
  return ()

addAllLegs ::
  ( JL.SearchRequestFlow m r c,
    JL.JourneyLeg TaxiLegRequest m,
    JL.JourneyLeg BusLegRequest m,
    JL.JourneyLeg MetroLegRequest m,
    JL.JourneyLeg WalkLegRequest m
  ) =>
  Id DJourney.Journey ->
  [MultimodalConfirm.JourneyLegsReq] ->
  m ()
addAllLegs journeyId legsReq = do
  journey <- getJourney journeyId
  journeyLegs <- getJourneyLegs journeyId
  forM_ journeyLegs $ \journeyLeg -> do
    case journeyLeg.mode of
      DTrip.Taxi -> do
        currentLegReq <- find (\lg -> lg.legNumber == journeyLeg.sequenceNumber) legsReq & fromMaybeM (InternalError "JourneyLegReqDataNotFound journeyLeg.sequenceNumber")
        addTaxiLeg journey journeyLeg currentLegReq
      _ -> return () -- handle metro and other cases

addTaxiLeg ::
  ( JL.SearchRequestFlow m r c,
    JL.JourneyLeg TaxiLegRequest m,
    JL.JourneyLeg BusLegRequest m,
    JL.JourneyLeg MetroLegRequest m,
    JL.JourneyLeg WalkLegRequest m
  ) =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  MultimodalConfirm.JourneyLegsReq ->
  m ()
addTaxiLeg journey journeyLeg currentLegReq = do
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (InternalError "JourneyNotFound journey.searchRequestId")
  let startLocation = JL.mkSearchReqLocation currentLegReq.originAddress journeyLeg.startLocation
  let endLocation = JL.mkSearchReqLocation currentLegReq.destinationAddress journeyLeg.endLocation
  let taxiSearchReq = JLI.mkTaxiSearchReq parentSearchReq journeyLeg startLocation [endLocation]
  JL.search taxiSearchReq
  return ()

-- getCurrentLeg :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, Monad m, MonadTime m, JourneyLeg TaxiLegRequest Maybe, JourneyLeg BusLegRequest Maybe, JourneyLeg MetroLegRequest Maybe, JourneyLeg WalkLegRequest Maybe) => Id DJourney.Journey -> Maybe JL.LegInfo
-- getCurrentLeg journeyId = do
--   journeyLegs <- getAllLegsInfo journeyId
--   --let completedStatus = [] -- TODO: to be passed
--   let currentLeg = find (\leg -> leg.status `notElem` completedStatus) journeyLegs
--   currentLeg

getRemainingLegs ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    JL.JourneyLeg TaxiLegRequest m,
    JL.JourneyLeg BusLegRequest m,
    JL.JourneyLeg MetroLegRequest m,
    JL.JourneyLeg WalkLegRequest m
  ) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getRemainingLegs journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  let remainingLegs = dropWhile (\leg -> leg.status `notElem` JL.completedStatus) journeyLegs -- check if edge case is to be handled [completed , skipped, inplan]
  return remainingLegs

-- deleteLeg :: JourneyLeg leg m => leg -> m ()
-- deleteLeg leg = do
--   let cancelReq = mkCancelReq leg
--   JL.cancel cancelReq

-- updateLeg :: JourneyLeg leg m => leg -> leg -> m ()
-- updateLeg
--   let updateReq = mkUpdateReq leg
--   JL.update leg

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

-- replaceLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m () -- leg2 can be an array
-- replaceLeg journey oldLegs newLeg =
--   forM_ (deleteLeg journey) oldLegs >> addLeg journey newLeg

-- extendLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m ()
-- extendLeg journey oldLegs newLeg =
--   forM_ (deleteLeg journey) oldLegs >> updateLeg journey newLeg
