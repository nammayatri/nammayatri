module Domain.Action.UI.Frontend
  ( GetDriverFlowStatusRes,
    getDriverFlowStatus,
  )
where

import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import Tools.Error

data GetDriverFlowStatusRes = GetDriverFlowStatusRes
  { oldStatus :: Maybe DDFS.FlowStatus,
    currentStatus :: DDFS.FlowStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getDriverFlowStatus :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m GetDriverFlowStatusRes
getDriverFlowStatus personId = do
  -- should not be run in replica
  driverStatus <- QDFS.getStatus personId >>= fromMaybeM (PersonNotFound personId.getId)
  case driverStatus of
    DDFS.GOT_SEARCH_REQUEST _ _ -> expireDriverStatusIfNeeded driverStatus
    DDFS.OFFERED_QUOTE _ _ -> expireDriverStatusIfNeeded driverStatus
    a -> return $ GetDriverFlowStatusRes Nothing a
  where
    expireDriverStatusIfNeeded driverStatus = do
      now <- getCurrentTime
      if now < driverStatus.validTill
        then return $ GetDriverFlowStatusRes Nothing driverStatus
        else do
          driverInfo <- CDI.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
          if driverInfo.active
            then do
              Esq.runTransaction $ QDFS.updateStatus personId DDFS.ACTIVE
              return $ GetDriverFlowStatusRes (Just driverStatus) DDFS.ACTIVE
            else do
              Esq.runTransaction $ QDFS.updateStatus personId DDFS.IDLE
              return $ GetDriverFlowStatusRes (Just driverStatus) DDFS.IDLE
