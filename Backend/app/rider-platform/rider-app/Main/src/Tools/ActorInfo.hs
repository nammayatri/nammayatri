module Tools.ActorInfo where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Scheduler

withMbActorInfo :: (HasCallStack, Finance.HasActorInfo m r) => Maybe ActorInfo -> m a -> m a
withMbActorInfo (Just ActorInfo {actorType, actorId}) action = Finance.withActorInfo actorType actorId action
withMbActorInfo Nothing action = do
  logWarning "ActorInfo not found"
  action

withJobIdActorInfoWrapper :: Finance.HasActorInfo m r => (Job e -> m a) -> (Job e -> m a)
withJobIdActorInfoWrapper jobHandler job@Job {id} = Finance.withActorInfo Finance.JOB (Just id.getId) $ jobHandler job

withRequestIdActorInfo :: Finance.HasActorInfo m r => m a -> m a
withRequestIdActorInfo action = do
  requestId <- asks (.requestId)
  Finance.withActorInfo Finance.SYSTEM requestId action

withPersonIdActorInfo :: Finance.HasActorInfo m r => Id DP.Person -> m a -> m a
withPersonIdActorInfo personId action = do
  Finance.withActorInfo Finance.PERSON (Just personId.getId) action

withMbPersonIdActorInfo :: (Finance.HasActorInfo m r, Log m) => Maybe (Id DP.Person) -> m a -> m a
withMbPersonIdActorInfo (Just personId) action = withPersonIdActorInfo personId action
withMbPersonIdActorInfo Nothing action = do
  logWarning "PersonId not found for actor info, using Unknown as default"
  action -- keep `Unknown requestId` as default

withDashboardPersonIdActorInfo :: Finance.HasActorInfo m r => Id DP.Person -> m a -> m a
withDashboardPersonIdActorInfo personId action = do
  Finance.withActorInfo Finance.DASHBOARD_PERSON (Just personId.getId) action

withDashboardMbPersonIdActorInfo :: (Finance.HasActorInfo m r, Log m) => Maybe (Id DP.Person) -> m a -> m a
withDashboardMbPersonIdActorInfo (Just personId) action = withDashboardPersonIdActorInfo personId action
withDashboardMbPersonIdActorInfo Nothing action = do
  logWarning "DashboardPersonId not found for actor info, using Unknown as default"
  action -- keep `Unknown requestId` as default
