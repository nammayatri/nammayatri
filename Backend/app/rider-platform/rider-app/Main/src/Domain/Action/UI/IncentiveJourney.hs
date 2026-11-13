module Domain.Action.UI.IncentiveJourney
  ( getIncentiveJourneyList,
    getIncentiveJourneyHistory,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common.UI.IncentiveJourney as LibUI
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Domain.Action.UI.IncentiveJourney as LibUIAction
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.AutoApplyCohortMapping as CQAuto
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified SharedLogic.IncentiveJourney as SLJourney
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.SpecialZone ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error

mkHandle :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> ServiceHandle Flow
mkHandle merchantId merchantOpCityId =
  ServiceHandle
    { findMerchantByShortId = \_ -> throwError (InvalidRequest "findMerchantByShortId unused in UI"),
      getMerchantOpCityId = \_ _ -> throwError (InvalidRequest "getMerchantOpCityId unused in UI"),
      getJourneys = \mbLimit mbOffset mbJourneyId mbJourneyType ->
        case mbJourneyId of
          Just journeyId -> do
            mbJourney <- CQJourney.findById IJ.RiderActor journeyId
            pure $
              case mbJourney of
                Just j | maybe True (== j.journeyType) mbJourneyType -> [j]
                _ -> []
          Nothing ->
            case mbJourneyType of
              Just jt -> CQJourney.findByJourneyType IJ.RiderActor mbLimit mbOffset jt
              Nothing -> CQJourney.findAll IJ.RiderActor mbLimit mbOffset,
      getJourneysByIds = CQJourney.findByIds IJ.RiderActor,
      getOneJourney = CQJourney.findById IJ.RiderActor,
      getMilestonesByJourneyId = CQMilestone.findByJourneyId IJ.RiderActor,
      clearJourneyCache = CQJourney.clearCache IJ.RiderActor,
      clearMilestoneCacheByJourneyId = CQMilestone.clearCacheByJourneyId IJ.RiderActor,
      clearAssignmentCacheByPersonId = CQAssignment.clearCacheByPersonId IJ.RiderActor,
      clearAssignmentCacheByCohortMappingId = CQAssignment.clearCacheByCohortMappingId IJ.RiderActor,
      clearAutoApplyCacheByMerchantAndCity = CQAuto.clearCacheByMerchantAndCity IJ.RiderActor,
      getTimeDiffFromUtc = \opCityId -> do
        riderConfig <-
          getConfig (RiderConfigDimensions {merchantOperatingCityId = opCityId.getId}) Nothing
            >>= fromMaybeM (RiderConfigNotFound opCityId.getId)
        pure riderConfig.timeDiffFromUtc,
      findPersonById = \_ -> pure Nothing,
      findAssignmentsByUserId = \personId -> SLJourney.findAssignmentsForPerson (cast personId) merchantId merchantOpCityId Nothing,
      findStatsHistoryByPersonId = \personId dayStart dayEnd mbLimit mbOffset ->
        QStats.findHistoryByPersonIdAndCreatedAtRange personId dayStart dayEnd mbLimit mbOffset,
      findStatsByPersonIdAndPeriodKey = \personId periodKey ->
        QStats.findByPersonIdAndPeriodKey personId periodKey,
      findStatsByPersonIdCjmAndPeriodKey = CQStats.findByPersonIdAndCjmAndPeriodKey IJ.RiderActor,
      waiveDriverMilestone = Nothing,
      waiveRiderMilestone = Nothing,
      putBulkAssignCsv = Nothing,
      scheduleBulkUpload = Nothing,
      findSpecialLocationNameById =
        Just $ \locId -> do
          mbLoc <- QSpecialLocation.findById (Id locId)
          pure $ (.locationName) <$> mbLoc,
      loadJourneyMilestones = SLJourney.loadJourneyMilestones
    }

findPersonCityId :: Id DP.Person -> Flow (Id DMOC.MerchantOperatingCity)
findPersonCityId personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  pure person.merchantOperatingCityId

getIncentiveJourneyList ::
  (Id DP.Person, Id DM.Merchant) ->
  Maybe Int ->
  Maybe Int ->
  Flow LibUI.IncentiveJourneyListRes
getIncentiveJourneyList (personId, merchantId) mbLimit mbOffset = do
  merchantOpCityId <- findPersonCityId personId
  LibUIAction.getIncentiveJourneyList
    (mkHandle merchantId merchantOpCityId)
    (Just (cast personId), cast merchantId, cast merchantOpCityId)
    mbLimit
    mbOffset

getIncentiveJourneyHistory ::
  (Id DP.Person, Id DM.Merchant) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow LibUI.IncentiveJourneyHistoryRes
getIncentiveJourneyHistory (personId, merchantId) mbDate mbLimit mbOffset = do
  merchantOpCityId <- findPersonCityId personId
  LibUIAction.getIncentiveJourneyHistory
    (mkHandle merchantId merchantOpCityId)
    (Just (cast personId), cast merchantId, cast merchantOpCityId)
    mbDate
    mbLimit
    mbOffset
