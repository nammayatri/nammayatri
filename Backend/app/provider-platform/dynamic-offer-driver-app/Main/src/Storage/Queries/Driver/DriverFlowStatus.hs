{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Driver.DriverFlowStatus where

import Domain.Types.Driver.DriverFlowStatus
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Driver.DriverFlowStatus as BeamDFS

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DDFS.DriverFlowStatus -> m ()
create = createWithKV

deleteById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamDFS.personId (Se.Eq driverId)]

getStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe DDFS.FlowStatus)
getStatus (Id personId) = findOneWithKV [Se.Is BeamDFS.personId $ Se.Eq personId] <&> (DDFS.flowStatus <$>)

clearPaymentStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Bool -> m ()
clearPaymentStatus personId isActive = updateStatus' False personId (if isActive then DDFS.ACTIVE else DDFS.IDLE)

updateStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> DDFS.FlowStatus -> m ()
updateStatus = updateStatus' True

updateStatus' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Bool -> Id Person -> DDFS.FlowStatus -> m ()
updateStatus' checkForPayment personId flowStatus = do
  getStatus personId >>= \case
    Just ds | not checkForPayment || not (isPaymentOverdue ds) -> do
      now <- getCurrentTime
      updateOneWithKV
        [Se.Set BeamDFS.flowStatus flowStatus, Se.Set BeamDFS.updatedAt now]
        [Se.Is BeamDFS.personId $ Se.Eq (getId personId)]
    _ -> pure ()

instance FromTType' BeamDFS.DriverFlowStatus DriverFlowStatus where
  fromTType' BeamDFS.DriverFlowStatusT {..} = do
    pure $
      Just
        DriverFlowStatus
          { personId = Id personId,
            flowStatus = flowStatus,
            updatedAt = updatedAt
          }

instance ToTType' BeamDFS.DriverFlowStatus DriverFlowStatus where
  toTType' DriverFlowStatus {..} = do
    BeamDFS.DriverFlowStatusT
      { BeamDFS.personId = getId personId,
        BeamDFS.flowStatus = flowStatus,
        BeamDFS.updatedAt = updatedAt
      }
