module Domain.Action.UI.IncentiveJourney
  ( getIncentiveJourneyList,
    getIncentiveJourneyHistory,
  )
where

import Data.Maybe (listToMaybe)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common.UI.IncentiveJourney as LibUI
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Domain.Action.UI.IncentiveJourney as LibUIAction
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Yudhishthira.Types.ConfigPilot as LYT
import qualified SharedLogic.IncentiveJourney as SLJourney
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.SpecialZone ()
import Storage.Beam.Yudhishthira ()
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

mkHandle :: ServiceHandle Flow
mkHandle =
  ServiceHandle
    { findMerchantByShortId = \_ -> throwError (InvalidRequest "findMerchantByShortId unused in UI"),
      getMerchantOpCityId = \_ _ -> throwError (InvalidRequest "getMerchantOpCityId unused in UI"),
      getJourneys = \merchantOpCityId mbMerchantId mbJourneyId mbEnabled mbJourneyType ->
        let dims =
              IncentiveJourneyDimensions
                { merchantOperatingCityId = merchantOpCityId.getId,
                  merchantId = (.getId) <$> mbMerchantId,
                  journeyId = mbJourneyId,
                  enabled = mbEnabled,
                  journeyType = mbJourneyType
                }
            fetch = case (mbMerchantId, mbEnabled) of
              (Just mid, Just True) ->
                CQJourney.findEnabledByMerchantIdAndMerchantOperatingCityId IJ.DriverActor mid merchantOpCityId
              (_, Just True) ->
                CQJourney.findEnabledByMerchantOperatingCityId IJ.DriverActor merchantOpCityId
              _ ->
                CQJourney.findByMerchantOperatingCityId IJ.DriverActor merchantOpCityId
         in getConfig dims (Just fetch),
      getOneJourney = \merchantOpCityId journeyId ->
        listToMaybe
          <$> getConfig
            ( IncentiveJourneyDimensions
                { merchantOperatingCityId = merchantOpCityId.getId,
                  merchantId = Nothing,
                  journeyId = Just journeyId,
                  enabled = Nothing,
                  journeyType = Nothing
                }
            )
            (Just $ CQJourney.findById IJ.DriverActor journeyId >>= maybe (pure []) (pure . (: []))),
      getMilestonesByJourneyId = \merchantOpCityId journeyId ->
        getConfig
          ( IncentiveJourneyMilestoneDimensions
              { merchantOperatingCityId = merchantOpCityId.getId,
                journeyId = Just journeyId,
                milestoneId = Nothing
              }
          )
          (Just $ CQMilestone.findByJourneyId IJ.DriverActor journeyId),
      clearJourneyCache = CQJourney.clearCache IJ.DriverActor,
      clearMilestoneCacheByJourneyId = CQMilestone.clearCacheByJourneyId IJ.DriverActor,
      clearAssignmentCacheByPersonId = CQAssignment.clearCacheByPersonId IJ.DriverActor,
      clearAssignmentCacheByCohortMappingId = CQAssignment.clearCacheByCohortMappingId IJ.DriverActor,
      invalidateJourneyConfigInMem = invalidateConfigInMem LYT.IncentiveJourneyConfigDriver,
      invalidateMilestoneConfigInMem = invalidateConfigInMem LYT.IncentiveJourneyMilestoneConfigDriver,
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
      scheduleBulkUpload = Nothing,
      findSpecialLocationNameById =
        Just $ \locId -> do
          mbLoc <- QSpecialLocation.findById (Id locId)
          pure $ (.locationName) <$> mbLoc,
      loadJourneyMilestones = \merchantOpCityId journeyId ->
        SLJourney.loadJourneyMilestones (cast merchantOpCityId) journeyId
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
