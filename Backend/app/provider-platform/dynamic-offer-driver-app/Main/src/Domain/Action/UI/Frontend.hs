{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

getDriverFlowStatus :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m GetDriverFlowStatusRes
getDriverFlowStatus personId = do
  -- should not be run in replica
  driverStatus <- QDFS.getStatus personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
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
              Esq.runTransaction $ QDFS.updateStatus @m personId DDFS.ACTIVE
              return $ GetDriverFlowStatusRes (Just driverStatus) DDFS.ACTIVE
            else do
              Esq.runTransaction $ QDFS.updateStatus @m personId DDFS.IDLE
              return $ GetDriverFlowStatusRes (Just driverStatus) DDFS.IDLE
