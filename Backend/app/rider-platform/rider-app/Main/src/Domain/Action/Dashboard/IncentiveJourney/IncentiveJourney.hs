module Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    putIncentiveJourneyUpdate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    putIncentiveJourneyMilestoneUpdate,
    getIncentiveJourneyStatsHistory,
    getIncentiveJourneyPersonAssignments,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    deleteIncentiveJourneyCohortJourney,
    getIncentiveJourneyCohortJourneyList,
    postIncentiveJourneyAssign,
    deleteIncentiveJourneyUnassign,
  )
where

import qualified API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney as Common
import qualified Dashboard.Common
import Data.Maybe (listToMaybe)
import Data.Time (Day)
import qualified Domain.Types.Merchant as DM
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common as IJC
import qualified Lib.IncentiveJourney.Domain.Action.Dashboard.Rider as LibRider
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.Yudhishthira.Types.ConfigPilot as LYT
import qualified SharedLogic.IncentiveJourney as SLJourney
import qualified SharedLogic.Merchant as SMerchant
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson

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
        CQMOC.getMerchantOpCityId merchant (Just opCity) <&> cast,
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
                CQJourney.findEnabledByMerchantIdAndMerchantOperatingCityId IJ.RiderActor mid merchantOpCityId
              (_, Just True) ->
                CQJourney.findEnabledByMerchantOperatingCityId IJ.RiderActor merchantOpCityId
              _ ->
                CQJourney.findByMerchantOperatingCityId IJ.RiderActor merchantOpCityId
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
            (Just $ CQJourney.findById IJ.RiderActor journeyId >>= maybe (pure []) (pure . (: []))),
      getMilestonesByJourneyId = \merchantOpCityId journeyId ->
        getConfig
          ( IncentiveJourneyMilestoneDimensions
              { merchantOperatingCityId = merchantOpCityId.getId,
                journeyId = Just journeyId,
                milestoneId = Nothing
              }
          )
          (Just $ CQMilestone.findByJourneyId IJ.RiderActor journeyId),
      clearJourneyCache = CQJourney.clearCache IJ.RiderActor,
      clearMilestoneCacheByJourneyId = CQMilestone.clearCacheByJourneyId IJ.RiderActor,
      clearAssignmentCacheByPersonId = CQAssignment.clearCacheByPersonId IJ.RiderActor,
      clearAssignmentCacheByCohortMappingId = CQAssignment.clearCacheByCohortMappingId IJ.RiderActor,
      invalidateJourneyConfigInMem = invalidateConfigInMem LYT.IncentiveJourneyConfigRider,
      invalidateMilestoneConfigInMem = invalidateConfigInMem LYT.IncentiveJourneyMilestoneConfigRider,
      getTimeDiffFromUtc = \merchantOpCityId -> do
        riderConfig <-
          getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
            >>= fromMaybeM (InvalidRequest "RiderConfig not found")
        pure riderConfig.timeDiffFromUtc,
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
        QStats.findStatsByPersonJourneyAndPeriod personId journeyId periodKey,
      waiveDriverMilestone = Nothing,
      waiveRiderMilestone =
        Just $ \personId merchantId merchantOpCityId journey milestoneId periodKey ->
          SLJourney.waiveRiderMilestone
            (cast personId)
            (cast merchantId)
            (cast merchantOpCityId)
            journey
            milestoneId
            periodKey,
      scheduleBulkUpload = Nothing,
      findSpecialLocationNameById = Nothing,
      loadJourneyMilestones = \merchantOpCityId journeyId ->
        -- Rider SharedLogic does not export loadJourneyMilestones; use ConfigPilot milestones.
        getConfig
          ( IncentiveJourneyMilestoneDimensions
              { merchantOperatingCityId = merchantOpCityId.getId,
                journeyId = Just journeyId,
                milestoneId = Nothing
              }
          )
          (Just $ CQMilestone.findByJourneyId IJ.RiderActor journeyId)
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
  LibRider.getIncentiveJourneyList mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyReq ->
  Environment.Flow Common.CreateIncentiveJourneyRes
postIncentiveJourneyCreate merchantShortId opCity =
  LibRider.postIncentiveJourneyCreate mkHandle (ShortId merchantShortId.getShortId) opCity

putIncentiveJourneyUpdate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyReq ->
  Environment.Flow APISuccess
putIncentiveJourneyUpdate merchantShortId opCity =
  LibRider.putIncentiveJourneyUpdate mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyMilestoneList ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.IncentiveJourney ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow Common.IncentiveJourneyMilestoneListRes
getIncentiveJourneyMilestoneList merchantShortId opCity =
  LibRider.getIncentiveJourneyMilestoneList mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyMilestoneCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyMilestoneReq ->
  Environment.Flow Common.CreateIncentiveJourneyMilestoneRes
postIncentiveJourneyMilestoneCreate merchantShortId opCity =
  LibRider.postIncentiveJourneyMilestoneCreate mkHandle (ShortId merchantShortId.getShortId) opCity

putIncentiveJourneyMilestoneUpdate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyMilestoneReq ->
  Environment.Flow APISuccess
putIncentiveJourneyMilestoneUpdate merchantShortId opCity =
  LibRider.putIncentiveJourneyMilestoneUpdate mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyStatsHistory ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.Person ->
  Maybe (Id Dashboard.Common.IncentiveJourney) ->
  Maybe Int ->
  Maybe Int ->
  Day ->
  Day ->
  Environment.Flow Common.IncentiveJourneyStatsHistoryRes
getIncentiveJourneyStatsHistory merchantShortId opCity =
  LibRider.getIncentiveJourneyStatsHistory mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyPersonAssignments ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.Person ->
  Environment.Flow Common.IncentiveJourneyPersonAssignmentListRes
getIncentiveJourneyPersonAssignments merchantShortId opCity =
  LibRider.getIncentiveJourneyPersonAssignments mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyStatsWaiveOff ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.WaiveIncentiveJourneyMilestoneReq ->
  Environment.Flow APISuccess
postIncentiveJourneyStatsWaiveOff merchantShortId opCity =
  LibRider.postIncentiveJourneyStatsWaiveOff mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyCohortCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortDetailsReq ->
  Environment.Flow Common.CreateCohortDetailsRes
postIncentiveJourneyCohortCreate merchantShortId opCity =
  LibRider.postIncentiveJourneyCohortCreate mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyCohortJourneyCreate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortJourneyMappingReq ->
  Environment.Flow Common.CreateCohortJourneyMappingRes
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity =
  LibRider.postIncentiveJourneyCohortJourneyCreate mkHandle (ShortId merchantShortId.getShortId) opCity

putIncentiveJourneyCohortJourneyUpdate ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateCohortJourneyMappingReq ->
  Environment.Flow APISuccess
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity =
  LibRider.putIncentiveJourneyCohortJourneyUpdate mkHandle (ShortId merchantShortId.getShortId) opCity

deleteIncentiveJourneyCohortJourney ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id Dashboard.Common.CohortJourneyMapping ->
  Environment.Flow APISuccess
deleteIncentiveJourneyCohortJourney merchantShortId opCity =
  LibRider.deleteIncentiveJourneyCohortJourney mkHandle (ShortId merchantShortId.getShortId) opCity

getIncentiveJourneyCohortJourneyList ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Common.IncentiveJourneyType ->
  Environment.Flow Common.CohortJourneyMappingListRes
getIncentiveJourneyCohortJourneyList merchantShortId opCity =
  LibRider.getIncentiveJourneyCohortJourneyList mkHandle (ShortId merchantShortId.getShortId) opCity

postIncentiveJourneyAssign ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.AssignUserToIncentiveJourneyReq ->
  Environment.Flow APISuccess
postIncentiveJourneyAssign merchantShortId opCity =
  LibRider.postIncentiveJourneyAssign mkHandle (ShortId merchantShortId.getShortId) opCity

deleteIncentiveJourneyUnassign ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UnassignUserFromIncentiveJourneyReq ->
  Environment.Flow APISuccess
deleteIncentiveJourneyUnassign merchantShortId opCity =
  LibRider.deleteIncentiveJourneyUnassign mkHandle (ShortId merchantShortId.getShortId) opCity
