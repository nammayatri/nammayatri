{-# LANGUAGE AllowAmbiguousTypes #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module SharedLogic.Scheduler
  ( SchedulerJobType (..),
    SSchedulerJobType (..),
    AllocateRentalJobData (..),
    scheduleJobByTime,
    receiveJobIds,
  )
where

import Data.Singletons.TH
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Queries

data SchedulerJobType
  = AllocateRental
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

genSingletons [''SchedulerJobType]
singEqInstances [''SchedulerJobType]
showSingInstances [''SchedulerJobType]

instance JobProcessor SchedulerJobType where
  restoreAnyJobInfo :: Sing (e :: SchedulerJobType) -> Text -> Maybe (AnyJobInfo SchedulerJobType)
  restoreAnyJobInfo SAllocateRental jobData = AnyJobInfo <$> restoreJobInfo SAllocateRental jobData

data AllocateRentalJobData = AllocateRentalJobData
  { bookingId :: Id DRB.Booking,
    shortOrgId :: ShortId DM.Subscriber
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

type instance JobContent 'AllocateRental = AllocateRentalJobData

instance {-# OVERLAPS #-} JobInfoProcessor 'AllocateRental

jobsGroupContainerKey :: Text
jobsGroupContainerKey = "DynamicDriverOfferApp:SchedulerStream"

scheduleJobByTime ::
  forall (e :: SchedulerJobType) m r.
  (EsqDBFlow m r, HedisFlow m r, JobInfoProcessor e, SingI e) =>
  UTCTime ->
  JobContent e ->
  m ()
scheduleJobByTime scheduledTime content = do
  jobId <-
    Esq.runTransaction $
      createJobByTime @SchedulerJobType @e scheduledTime content
  Redis.withCrossAppRedis $ Redis.xAdd jobsGroupContainerKey jobId

receiveJobIds ::
  (HedisFlow m r, FromJSON a) =>
  Redis.XGroupConsumerName ->
  Int ->
  m (Maybe [Redis.XReadGroupRes a])
receiveJobIds consumerName count = do
  Redis.withCrossAppRedis . Redis.xReadGroupOpts consumerName jobsGroupContainerKey $
    Redis.XReadOpts {block = Nothing, recordCount = Just $ fromIntegral count}
