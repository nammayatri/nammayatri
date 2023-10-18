{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person.PersonFlowStatus
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Person
import Domain.Types.Person.PersonFlowStatus
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Person.PersonFlowStatus as BeamPFS

create :: MonadFlow m => DPFS.PersonFlowStatus -> m ()
create = createWithKV

getStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe DPFS.FlowStatus)
getStatus (Id personId) = findOneWithKV [Se.Is BeamPFS.personId $ Se.Eq personId] <&> (DPFS.flowStatus <$>)

updateStatus :: MonadFlow m => Id Person -> DPFS.FlowStatus -> m ()
updateStatus (Id personId) flowStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamPFS.flowStatus flowStatus, Se.Set BeamPFS.updatedAt now]
    [Se.Is BeamPFS.personId $ Se.Eq personId]

deleteByPersonId :: MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamPFS.personId $ Se.Eq personId]

updateToIdleMultiple :: MonadFlow m => [Id Person] -> UTCTime -> m ()
updateToIdleMultiple personIds now =
  updateWithKV
    [Se.Set BeamPFS.flowStatus DPFS.IDLE, Se.Set BeamPFS.updatedAt now]
    [Se.Is BeamPFS.personId $ Se.In (getId <$> personIds)]

instance FromTType' BeamPFS.PersonFlowStatus PersonFlowStatus where
  fromTType' BeamPFS.PersonFlowStatusT {..} = do
    pure $
      Just
        PersonFlowStatus
          { personId = Id personId,
            flowStatus = flowStatus,
            updatedAt = updatedAt
          }

instance ToTType' BeamPFS.PersonFlowStatus PersonFlowStatus where
  toTType' PersonFlowStatus {..} = do
    BeamPFS.PersonFlowStatusT
      { BeamPFS.personId = getId personId,
        BeamPFS.flowStatus = flowStatus,
        BeamPFS.updatedAt = updatedAt
      }
