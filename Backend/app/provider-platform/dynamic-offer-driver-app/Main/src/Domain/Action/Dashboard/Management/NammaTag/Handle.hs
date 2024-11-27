module Domain.Action.Dashboard.Management.NammaTag.Handle (kaalChakraHandle) where

import qualified Domain.Types.Person as DPerson
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Types
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import qualified Lib.Yudhishthira.Event.KaalChakra as KaalChakra
import qualified Lib.Yudhishthira.Types
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Person as QPerson

type HandlerFlow m r = (EsqDBFlow m r, MonadFlow m, CacheFlow m r, HasField "jobInfoMap" r (Map Text Bool), HasField "schedulerSetName" r Text, HasField "schedulerType" r Kernel.External.Types.SchedulerType, HasField "maxShards" r Int)

createFetchUserDataJob :: HandlerFlow m r => Lib.Yudhishthira.Types.UpdateKaalBasedTagsJobReq -> UTCTime -> m ()
createFetchUserDataJob req scheduledTime = do
  let jobData = Lib.Yudhishthira.Types.mkKaalChakraJobDataFromUpdateTagData req True
  maxShards <- asks (.maxShards)
  case req.chakra of
    Lib.Yudhishthira.Types.Daily -> QAllJ.createJobByTime @_ @'Daily scheduledTime maxShards jobData
    Lib.Yudhishthira.Types.Weekly -> QAllJ.createJobByTime @_ @'Weekly scheduledTime maxShards jobData
    Lib.Yudhishthira.Types.Monthly -> QAllJ.createJobByTime @_ @'Monthly scheduledTime maxShards jobData
    Lib.Yudhishthira.Types.Quarterly -> QAllJ.createJobByTime @_ @'Quarterly scheduledTime maxShards jobData

createUpdateUserTagDataJob :: HandlerFlow m r => Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Id Lib.Yudhishthira.Types.Event -> UTCTime -> m ()
createUpdateUserTagDataJob req eventId scheduledTime = do
  let jobData = Lib.Yudhishthira.Types.mkUpdateTagDataFromKaalChakraJobData req eventId
  maxShards <- asks (.maxShards)
  case req.chakra of
    Lib.Yudhishthira.Types.Daily -> QAllJ.createJobByTime @_ @'DailyUpdateTag scheduledTime maxShards jobData
    Lib.Yudhishthira.Types.Weekly -> QAllJ.createJobByTime @_ @'WeeklyUpdateTag scheduledTime maxShards jobData
    Lib.Yudhishthira.Types.Monthly -> QAllJ.createJobByTime @_ @'MonthlyUpdateTag scheduledTime maxShards jobData
    Lib.Yudhishthira.Types.Quarterly -> QAllJ.createJobByTime @_ @'QuarterlyUpdateTag scheduledTime maxShards jobData

-- Moved here because of overlapping instances for HasSchemaName SchedulerJob between bpp and kaal-chakra
kaalChakraHandle :: HandlerFlow m r => KaalChakra.Handle m Action
kaalChakraHandle =
  KaalChakra.Handle
    { getUserTags = \userId -> do
        mbDriver <- QPerson.findById $ cast @Lib.Yudhishthira.Types.User @DPerson.Person userId
        pure $ mbDriver <&> (\driver -> Lib.Yudhishthira.Types.TagNameValue <$> fromMaybe [] driver.driverTag),
      updateUserTags = \userId driverTags -> QPerson.updateDriverTag (Just $ Lib.Yudhishthira.Types.getTagNameValue <$> driverTags) (cast @Lib.Yudhishthira.Types.User @DPerson.Person userId),
      createFetchUserDataJob,
      createUpdateUserTagDataJob,
      action = kaalChakraAction . cast @Lib.Yudhishthira.Types.User @DPerson.Person
    }

kaalChakraAction :: HandlerFlow m r => Id DPerson.Person -> Action -> m ()
kaalChakraAction driverId action = case action of
  SAFE_TO_UNSAFE_COHORT -> do
    logInfo $ "Kaal chakra action: " <> show action <> "; driverId: " <> show driverId
  UNSAFE_TO_SAFE_COHORT -> do
    logInfo $ "Kaal chakra action: " <> show action <> "; driverId: " <> show driverId

data Action = SAFE_TO_UNSAFE_COHORT | UNSAFE_TO_SAFE_COHORT
  deriving (Show, Read)
