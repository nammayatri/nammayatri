{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Dashboard.Scheduler where

import "dashboard-helper-api" Dashboard.Common (HideSecrets (hideSecrets))
import qualified Data.Aeson as A
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Lib.Scheduler.JobStorageType.SchedulerType (findAllWithinWindow, updateStatus)
import Lib.Scheduler.Types
import SharedLogic.Allocator (AllocatorJobType)

------------------ List Scheduler -------------------------
data ListSchedulerReq = ListSchedulerReq
  { from :: LocalTime,
    to :: Maybe LocalTime,
    status :: Maybe JobStatus,
    jobType :: Maybe AllocatorJobType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets ListSchedulerReq where
  hideSecrets = identity

newtype ListSchedulerResp = ListSchedulerResp [A.Value]
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

listScheduler :: ShortId DM.Merchant -> ListSchedulerReq -> Flow ListSchedulerResp
listScheduler _merchantShortId _req@ListSchedulerReq {..} = do
  res :: [AnyJob AllocatorJobType] <- findAllWithinWindow from to status (show <$> jobType)
  return $ ListSchedulerResp (toJSON <$> res)

----------------- Update Scheduler status --------------------------
data UpdateSchedulerReq = UpdateSchedulerReq
  { id :: Id AnyJob,
    status :: JobStatus,
    jobType :: AllocatorJobType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateSchedulerReq where
  hideSecrets = identity

updateScheduler :: ShortId DM.Merchant -> UpdateSchedulerReq -> Flow APISuccess
updateScheduler _merchantShortId _req@UpdateSchedulerReq {..} = do
  updateStatus (show jobType) status id
  pure Success
