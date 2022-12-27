module Domain.Action.UI.Frontend
  ( GetPersonFlowStatusRes,
    FrontendEvent (..),
    NotifyEventReq (..),
    NotifyEventResp,
    getPersonFlowStatus,
    notifyEvent,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
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

getPersonFlowStatus :: (EsqDBFlow m r) => Id DP.Person -> m GetPersonFlowStatusRes
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

notifyEvent :: (EsqDBFlow m r) => Id DP.Person -> NotifyEventReq -> m NotifyEventResp
notifyEvent personId req = do
  case req.event of
    RATE_DRIVER_SKIPPED -> backToIDLE
    SEARCH_CANCELLED -> backToIDLE
  pure APISuccess.Success
  where
    backToIDLE = Esq.runTransaction $ QPFS.updateStatus personId DPFS.IDLE
