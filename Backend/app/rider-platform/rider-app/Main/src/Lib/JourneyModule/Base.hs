module Lib.JourneyModule.Base where

import qualified API.Types.UI.MultimodalConfirm as MultimodalConfirm
-- import Beckn.Types.Core.Taxi.API.Confirm
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (unpack)
--import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Trip as DTrip
--import qualified Kernel.External.MultiModal.Interface as ExternalInterface

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBEnv, EsqDBReplicaFlow)
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
--import Kernel.Utils.Logging
import Lib.JourneyLeg.Interface
-- import Lib.JourneyLeg.Types as JT

import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyModule.Types
import Lib.JourneyModule.Types as JL
import Lib.JourneyModule.Utils
-- import SharedLogic.CallBPPInternal as CallBPPInternal
-- import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.Journey as JQ
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
-- import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Metrics

--import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)

--(CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) =>
init ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    JourneyLeg TaxiLegRequest m,
    JourneyLeg BusLegRequest m,
    JourneyLeg MetroLegRequest m,
    JourneyLeg WalkLegRequest m
  ) =>
  JourneyInitData ->
  m DJourney.Journey
init journeyReq = do
  journeyId <- Common.generateGUID
  --let journeyLegsCount = length journeyReq.legs
  totalFares <-
    mapWithIndex
      ( \idx leg -> do
          let travelMode = (convertMultiModalModeToTripMode leg.mode (distanceToMeters leg.distance) journeyReq.maximumWalkDistance)
          totalLegFare <-
            case travelMode of
              DTrip.Taxi -> do
                getFareReq :: TaxiLegRequest <- mkTaxiGetFareReq journeyReq.merchantId journeyReq.merchantOperatingCityId leg
                JL.getFare getFareReq
              DTrip.Bus -> do
                getFareReq :: BusLegRequest <- mkBusGetFareReq leg
                JL.getFare getFareReq
              DTrip.Metro -> do
                getFareReq :: MetroLegRequest <- mkMetroGetFareReq leg
                JL.getFare getFareReq
              DTrip.Walk -> do
                getFareReq :: WalkLegRequest <- mkWalkGetFareReq leg
                JL.getFare getFareReq

          journeyLeg <- mkJourneyLeg idx leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance
          QJourneyLeg.create journeyLeg
          return totalLegFare
      )
      journeyReq.legs
  journey <- mkJourney journeyReq.estimatedDistance journeyReq.estimatedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId totalFares journeyReq.legs journeyReq.maximumWalkDistance
  QJourney.create journey
  logDebug $ "journey for multi-modal: " <> show journey
  return journey

-- (MonadReader r m, EsqDBFlow m r, CoreMetrics m, HasField "hedisMigrationStage" r Bool) =>
getJourney :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m DJourney.Journey
getJourney id = JQ.findByPrimaryKey id >>= fromMaybeM (InternalError "JourneyNotFound id.getId")

-- Return only the OTP raw data without the search and bookings
getJourneyLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m [DJourneyLeg.JourneyLeg]
getJourneyLegs journeyId = QJourneyLeg.findAllByJourneyId journeyId

-- Return complete data with search and bookings
getAllLegsInfo ::
  ( EsqDBFlow m r,
    Monad m,
    CacheFlow m r,
    JourneyLeg TaxiLegRequest m,
    JourneyLeg BusLegRequest m,
    JourneyLeg MetroLegRequest m,
    JourneyLeg WalkLegRequest m,
    EncFlow m r
  ) =>
  Id DJourney.Journey ->
  m [LegInfo]
getAllLegsInfo journeyId = do
  allLegsRawData <- getJourneyLegs journeyId
  allLegsInfo <-
    allLegsRawData `forM` \leg -> do
      case leg.legId of
        Just legIdText -> do
          case readMaybe (unpack legIdText) :: Maybe (Id DJourneyLeg.JourneyLeg) of
            Just legId -> do
              case leg.mode of
                DTrip.Taxi -> JL.getInfo $ TaxiLegRequestGetInfo $ TaxiLegRequestGetInfoData {searchId = cast legId}
                DTrip.Walk -> JL.getInfo $ WalkLegRequestGetInfo $ WalkLegRequestGetInfoData {walkLegId = cast legId}
                DTrip.Metro -> JL.getInfo $ MetroLegRequestGetInfo $ MetroLegRequestGetInfoData {searchId = cast legId}
                _ -> throwError $ InvalidRequest ("Mode " <> show leg.mode <> " not supported!")
            Nothing -> throwError $ InvalidRequest ("Mode not supported!")
        Nothing -> throwError $ InvalidRequest ("Mode not supported!")
  return $ sortBy (comparing order) allLegsInfo

startJourney ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    JourneyLeg TaxiLegRequest m,
    JourneyLeg BusLegRequest m,
    JourneyLeg MetroLegRequest m,
    JourneyLeg WalkLegRequest m
  ) =>
  Id DJourney.Journey ->
  m () -- confirm request
startJourney _ = do
  -- startJourney journeyId = do
  --allLegs <- getAllLegsInfo journeyId
  --mapM_ (JL.confirm . mkConfirmReq) allLegs
  return ()

addAllLegs ::
  ( EncFlow m r,
    EsqDBFlow m r,
    HasField "esqDBReplicaEnv" r EsqDBEnv,
    Monad m,
    CacheFlow m r,
    JourneyLeg TaxiLegRequest m,
    JourneyLeg BusLegRequest m,
    JourneyLeg MetroLegRequest m,
    JourneyLeg WalkLegRequest m
  ) =>
  Id DJourney.Journey ->
  [MultimodalConfirm.JourneyLegsReq] ->
  m ()
addAllLegs journeyId legsReq = do
  journey <- getJourney journeyId
  journeyLegs <- getJourneyLegs journeyId
  _ <-
    journeyLegs `forM` \journeyLeg -> do
      case journeyLeg.mode of
        DTrip.Taxi -> do
          currentLegReq <- find (\lg -> lg.legNumber == journeyLeg.sequenceNumber) legsReq & fromMaybeM (InternalError "JourneyLegReqDataNotFound journeyLeg.sequenceNumber")
          addTaxiLeg journey journeyLeg currentLegReq
        _ -> return () -- handle metro and other cases
  return ()

addTaxiLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    MonadClock m,
    MonadTime m,
    MonadFlow m,
    JourneyLeg TaxiLegRequest m,
    JourneyLeg BusLegRequest m,
    JourneyLeg MetroLegRequest m,
    JourneyLeg WalkLegRequest m,
    CoreMetrics m
  ) =>
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  MultimodalConfirm.JourneyLegsReq ->
  m ()
addTaxiLeg journey journeyLeg currentLegReq = do
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (InternalError "JourneyNotFound journey.searchRequestId")
  let startLocation = mkSearchReqLocation currentLegReq.originAddress journeyLeg.startLocation
  let endLocation = mkSearchReqLocation currentLegReq.destinationAddress journeyLeg.endLocation
  let taxiSearchReq = mkTaxiSearchReq parentSearchReq journeyLeg startLocation [endLocation]
  JL.search taxiSearchReq
  return ()

-- getCurrentLeg :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, Monad m, MonadTime m, JourneyLeg TaxiLegRequest Maybe, JourneyLeg BusLegRequest Maybe, JourneyLeg MetroLegRequest Maybe, JourneyLeg WalkLegRequest Maybe) => Id DJourney.Journey -> Maybe LegInfo
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
    JourneyLeg TaxiLegRequest m,
    JourneyLeg BusLegRequest m,
    JourneyLeg MetroLegRequest m,
    JourneyLeg WalkLegRequest m
  ) =>
  Id DJourney.Journey ->
  m [LegInfo]
getRemainingLegs journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  --let completedStatus = [] -- TODO: to be passed
  let remainingLegs = dropWhile (\leg -> leg.status `notElem` completedStatus) journeyLegs -- check if edge case is to be handled [completed , skipped, inplan]
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
