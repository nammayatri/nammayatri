module Domain.Action.UI.IncentiveJourney
  ( getIncentiveJourneyList,
    getIncentiveJourneyHistory,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
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
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

mkHandle :: ServiceHandle Flow
mkHandle =
  ServiceHandle
    { findMerchantByShortId = \_ -> throwError (InvalidRequest "findMerchantByShortId unused in UI"),
      getMerchantOpCityId = \_ _ -> throwError (InvalidRequest "getMerchantOpCityId unused in UI"),
      getJourneys = \mbLimit mbOffset mbJourneyId mbJourneyType ->
        case mbJourneyId of
          Just journeyId -> do
            mbJourney <- CQJourney.findById IJ.DriverActor journeyId
            pure $
              case mbJourney of
                Just j | maybe True (== j.journeyType) mbJourneyType -> [j]
                _ -> []
          Nothing ->
            case mbJourneyType of
              Just jt -> CQJourney.findByJourneyType IJ.DriverActor mbLimit mbOffset jt
              Nothing -> CQJourney.findAll IJ.DriverActor mbLimit mbOffset,
      getJourneysByIds = CQJourney.findByIds IJ.DriverActor,
      getOneJourney = CQJourney.findById IJ.DriverActor,
      getMilestonesByJourneyId = CQMilestone.findByJourneyId IJ.DriverActor,
      clearJourneyCache = CQJourney.clearCache IJ.DriverActor,
      clearMilestoneCacheByJourneyId = CQMilestone.clearCacheByJourneyId IJ.DriverActor,
      clearAssignmentCacheByPersonId = CQAssignment.clearCacheByPersonId IJ.DriverActor,
      clearAssignmentCacheByCohortMappingId = CQAssignment.clearCacheByCohortMappingId IJ.DriverActor,
      clearAutoApplyCacheByMerchantAndCity = CQAuto.clearCacheByMerchantAndCity IJ.DriverActor,
      getTimeDiffFromUtc = \merchantOpCityId -> do
        transporterConfig <-
          getOneConfig
            (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
            Nothing
            >>= fromMaybeM (InvalidRequest "TransporterConfig not found")
        pure transporterConfig.timeDiffFromUtc,
      findPersonById = \_ -> pure Nothing,
      findAssignmentsByUserId = \personId -> SLJourney.findAssignmentsByUserId (cast personId),
      findStatsHistoryByPersonId = \personId dayStart dayEnd mbLimit mbOffset ->
        QStats.findHistoryByPersonIdAndCreatedAtRange personId dayStart dayEnd mbLimit mbOffset,
      findStatsByPersonIdAndPeriodKey = \personId periodKey ->
        QStats.findByPersonIdAndPeriodKey personId periodKey,
      findStatsByPersonIdJourneyIdAndPeriodKey = \personId journeyId periodKey ->
        CQStats.findByPersonIdAndJourneyIdAndPeriodKey IJ.DriverActor personId journeyId periodKey,
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

getIncentiveJourneyList ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe Int ->
  Maybe Int ->
  Flow LibUI.IncentiveJourneyListRes
getIncentiveJourneyList (mbPersonId, merchantId, merchantOpCityId) =
  LibUIAction.getIncentiveJourneyList
    mkHandle
    (cast <$> mbPersonId, cast merchantId, cast merchantOpCityId)

getIncentiveJourneyHistory ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow LibUI.IncentiveJourneyHistoryRes
getIncentiveJourneyHistory (mbPersonId, merchantId, merchantOpCityId) =
  LibUIAction.getIncentiveJourneyHistory
    mkHandle
    (cast <$> mbPersonId, cast merchantId, cast merchantOpCityId)
