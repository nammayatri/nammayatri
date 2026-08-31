module Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    putIncentiveJourneyUpdate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    putIncentiveJourneyMilestoneUpdate,
    getIncentiveJourneyStatsHistory,
    getIncentiveJourneyDriverAssignments,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    getIncentiveJourneyCohortList,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    deleteIncentiveJourneyCohortJourney,
    getIncentiveJourneyCohortJourneyList,
    postIncentiveJourneyAssign,
    deleteIncentiveJourneyUnassign,
    postIncentiveJourneyAssignBulkFromS3,
    getIncentiveJourneyAssignBulkFromS3List,
  )
where

import qualified API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney as Common
import qualified Dashboard.Common
import Data.Maybe (listToMaybe)
import Data.Time (Day)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.VehicleVariant as VecVariant
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common as IJC
import qualified Lib.IncentiveJourney.Domain.Action.Dashboard.Provider as LibProvider
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import Lib.Scheduler.JobStorageType.SchedulerType (createJobByTime)
import qualified Lib.Yudhishthira.Types.ConfigPilot as LYT
import SharedLogic.Allocator
  ( AllocatorJobType (..),
    BulkUserCohortMappingUploadJobData (..),
  )
import qualified SharedLogic.IncentiveJourney as SLJourney
import qualified SharedLogic.Merchant as SMerchant
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh

mkHandle :: ServiceHandle Environment.Flow
mkHandle =
  ServiceHandle
    { findMerchantByShortId = \shortId -> do
        merchant <- SMerchant.findMerchantByShortId (ShortId shortId.getShortId)
        pure
          IJC.MerchantInfo
            { id = cast merchant.id,
              shortId = ShortId merchant.shortId.getShortId
            },
      getMerchantOpCityId = \merchantInfo opCity -> do
        merchant <- SMerchant.findMerchantByShortId (ShortId merchantInfo.shortId.getShortId)
        CQMOC.getMerchantOpCityId Nothing merchant (Just opCity) <&> cast,
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
      findPersonById = \personId -> do
        mbPerson <- QPerson.findById (cast personId)
        pure $
          mbPerson <&> \p ->
            IJC.PersonInfo
              { id = cast p.id,
                merchantId = cast p.merchantId,
                merchantOperatingCityId = cast p.merchantOperatingCityId
              },
      findAssignmentsByUserId = \personId -> SLJourney.findAssignmentsByUserId (cast personId),
      findStatsHistoryByPersonId = \personId dayStart dayEnd mbLimit mbOffset ->
        QStats.findHistoryByPersonIdAndCreatedAtRange personId dayStart dayEnd mbLimit mbOffset,
      findStatsByPersonIdAndPeriodKey = \personId periodKey ->
        QStats.findByPersonIdAndPeriodKey personId periodKey,
      findStatsByPersonIdJourneyIdAndPeriodKey = \personId journeyId periodKey ->
        CQStats.findByPersonIdAndJourneyIdAndPeriodKey IJ.DriverActor personId journeyId periodKey,
      waiveDriverMilestone =
        Just $ \driverId merchantId merchantOpCityId journey milestoneId periodKey -> do
          transporterConfig <-
            getOneConfig
              (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
              Nothing
              >>= fromMaybeM (InvalidRequest "TransporterConfig not found")
          mbVehicle <- QVeh.findById (cast driverId)
          let vehCategory = fmap (VecVariant.castVehicleVariantToVehicleCategory . (.variant)) mbVehicle
          SLJourney.waiveDriverMilestone
            (cast driverId)
            (cast merchantId)
            (cast merchantOpCityId)
            transporterConfig
            journey
            milestoneId
            periodKey
            vehCategory
            Nothing,
      waiveRiderMilestone = Nothing,
      scheduleBulkUpload =
        Just $ \merchantId merchantOpCityId s3Path scheduledAt batchSize delaySecs runIdText -> do
          let jobData =
                BulkUserCohortMappingUploadJobData
                  { merchantId = cast merchantId,
                    merchantOperatingCityId = cast merchantOpCityId,
                    s3FilePath = s3Path,
                    offset = 0,
                    batchSize = batchSize,
                    rescheduleDelaySeconds = delaySecs,
                    runId = runIdText
                  }
          createJobByTime @_ @'BulkUserCohortMappingUpload (Just (cast merchantId)) (Just (cast merchantOpCityId)) scheduledAt jobData,
      findSpecialLocationNameById = Nothing,
      loadJourneyMilestones = \merchantOpCityId journeyId ->
        SLJourney.loadJourneyMilestones (cast merchantOpCityId) journeyId
    }

getIncentiveJourneyList ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe (Id Dashboard.Common.IncentiveJourney) ->
  Maybe Common.IncentiveJourneyType ->
  Environment.Flow Common.IncentiveJourneyListRes
getIncentiveJourneyList merchantShortId opCity =
  LibProvider.getIncentiveJourneyList mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyReq ->
  Environment.Flow Common.CreateIncentiveJourneyRes
postIncentiveJourneyCreate merchantShortId opCity =
  LibProvider.postIncentiveJourneyCreate mkHandle (ShortId merchantShortId.getShortId) opCity

putIncentiveJourneyUpdate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyReq ->
  Environment.Flow APISuccess
putIncentiveJourneyUpdate merchantShortId opCity =
  LibProvider.putIncentiveJourneyUpdate mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyMilestoneList ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.IncentiveJourney ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow Common.IncentiveJourneyMilestoneListRes
getIncentiveJourneyMilestoneList merchantShortId opCity =
  LibProvider.getIncentiveJourneyMilestoneList mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyMilestoneCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyMilestoneReq ->
  Environment.Flow Common.CreateIncentiveJourneyMilestoneRes
postIncentiveJourneyMilestoneCreate merchantShortId opCity =
  LibProvider.postIncentiveJourneyMilestoneCreate mkHandle (ShortId merchantShortId.getShortId) opCity

putIncentiveJourneyMilestoneUpdate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyMilestoneReq ->
  Environment.Flow APISuccess
putIncentiveJourneyMilestoneUpdate merchantShortId opCity =
  LibProvider.putIncentiveJourneyMilestoneUpdate mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyStatsHistory ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.Driver ->
  Maybe (Id Dashboard.Common.IncentiveJourney) ->
  Maybe Int ->
  Maybe Int ->
  Day ->
  Day ->
  Environment.Flow Common.IncentiveJourneyStatsHistoryRes
getIncentiveJourneyStatsHistory merchantShortId opCity =
  LibProvider.getIncentiveJourneyStatsHistory mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyDriverAssignments ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.Driver ->
  Environment.Flow Common.IncentiveJourneyDriverAssignmentListRes
getIncentiveJourneyDriverAssignments merchantShortId opCity =
  LibProvider.getIncentiveJourneyDriverAssignments mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyStatsWaiveOff ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.WaiveIncentiveJourneyMilestoneReq ->
  Environment.Flow APISuccess
postIncentiveJourneyStatsWaiveOff merchantShortId opCity =
  LibProvider.postIncentiveJourneyStatsWaiveOff mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyCohortCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortDetailsReq ->
  Environment.Flow Common.CreateCohortDetailsRes
postIncentiveJourneyCohortCreate merchantShortId opCity =
  LibProvider.postIncentiveJourneyCohortCreate mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyCohortList ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Environment.Flow Common.CohortDetailsListRes
getIncentiveJourneyCohortList merchantShortId opCity =
  LibProvider.getIncentiveJourneyCohortList mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyCohortJourneyCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortJourneyMappingReq ->
  Environment.Flow Common.CreateCohortJourneyMappingRes
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity =
  LibProvider.postIncentiveJourneyCohortJourneyCreate mkHandle (ShortId merchantShortId.getShortId) opCity

putIncentiveJourneyCohortJourneyUpdate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateCohortJourneyMappingReq ->
  Environment.Flow APISuccess
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity =
  LibProvider.putIncentiveJourneyCohortJourneyUpdate mkHandle (ShortId merchantShortId.getShortId) opCity

deleteIncentiveJourneyCohortJourney ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.CohortJourneyMapping ->
  Environment.Flow APISuccess
deleteIncentiveJourneyCohortJourney merchantShortId opCity =
  LibProvider.deleteIncentiveJourneyCohortJourney mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyCohortJourneyList ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe (Id Dashboard.Common.CohortDetails) ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Common.IncentiveJourneyType ->
  Environment.Flow Common.CohortJourneyMappingListRes
getIncentiveJourneyCohortJourneyList merchantShortId opCity =
  LibProvider.getIncentiveJourneyCohortJourneyList mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyAssign ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.AssignUserToIncentiveJourneyReq ->
  Environment.Flow APISuccess
postIncentiveJourneyAssign merchantShortId opCity =
  LibProvider.postIncentiveJourneyAssign mkHandle (ShortId merchantShortId.getShortId) opCity

deleteIncentiveJourneyUnassign ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UnassignUserFromIncentiveJourneyReq ->
  Environment.Flow APISuccess
deleteIncentiveJourneyUnassign merchantShortId opCity =
  LibProvider.deleteIncentiveJourneyUnassign mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyAssignBulkFromS3 ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.BulkAssignUserCohortFromS3Req ->
  Environment.Flow Common.BulkAssignUserCohortFromS3Res
postIncentiveJourneyAssignBulkFromS3 merchantShortId opCity =
  LibProvider.postIncentiveJourneyAssignBulkFromS3 mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyAssignBulkFromS3List ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BulkUserCohortMappingRunStatus ->
  Environment.Flow Common.BulkAssignUserCohortFromS3ListRes
getIncentiveJourneyAssignBulkFromS3List merchantShortId opCity =
  LibProvider.getIncentiveJourneyAssignBulkFromS3List mkHandle (ShortId merchantShortId.getShortId) opCity
