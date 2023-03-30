{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Frontend
  ( GetPersonFlowStatusRes,
    FrontendEvent (..),
    NotifyEventReq (..),
    NotifyEventResp,
    getPersonFlowStatus,
    notifyEvent,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import Tools.Error

data GetPersonFlowStatusRes = GetPersonFlowStatusRes
  { oldStatus :: Maybe DPFS.FlowStatus,
    currentStatus :: DPFS.FlowStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FrontendEvent = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype NotifyEventReq = NotifyEventReq
  { event :: FrontendEvent
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type NotifyEventResp = APISuccess

getPersonFlowStatus :: (EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m) => Id DP.Person -> m GetPersonFlowStatusRes
getPersonFlowStatus personId = do
  -- should not be run in replica
  personStatus <- QPFS.getStatus personId >>= fromMaybeM (PersonNotFound personId.getId)
  case personStatus of
    DPFS.SEARCHING _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.GOT_ESTIMATE _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.WAITING_FOR_DRIVER_OFFERS _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.DRIVER_OFFERED_QUOTE _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.WAITING_FOR_DRIVER_ASSIGNMENT _ _ -> expirePersonStatusIfNeeded personStatus
    a -> return $ GetPersonFlowStatusRes Nothing a
  where
    expirePersonStatusIfNeeded personStatus = do
      now <- getCurrentTime
      if now < personStatus.validTill
        then return $ GetPersonFlowStatusRes Nothing personStatus
        else do
          Esq.runTransaction $ QPFS.updateStatus personId DPFS.IDLE
          return $ GetPersonFlowStatusRes (Just personStatus) DPFS.IDLE

notifyEvent :: (EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m) => Id DP.Person -> NotifyEventReq -> m NotifyEventResp
notifyEvent personId req = do
  case req.event of
    RATE_DRIVER_SKIPPED -> backToIDLE
    SEARCH_CANCELLED -> do
      activeBooking <- Esq.runInReplica $ QB.findLatestByRiderIdAndStatus personId DRB.activeBookingStatus
      whenJust activeBooking $ \_ -> throwError (InvalidRequest "ACTIVE_BOOKING_EXISTS")
      backToIDLE
  pure APISuccess.Success
  where
    backToIDLE = Esq.runTransaction $ QPFS.updateStatus personId DPFS.IDLE
