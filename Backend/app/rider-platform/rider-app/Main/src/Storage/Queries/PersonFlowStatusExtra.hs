{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonFlowStatusExtra where

import Domain.Types.Person
import Domain.Types.PersonFlowStatus
import qualified Domain.Types.PersonFlowStatus as DPFS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PersonFlowStatus as BeamPFS
import Storage.Queries.OrphanInstances.PersonFlowStatus

-- Extra code goes here --
create :: KvDbFlow m r => DPFS.PersonFlowStatus -> m ()
create = createWithKV

getStatus :: KvDbFlow m r => Id Person -> m (Maybe DPFS.FlowStatus)
getStatus (Id personId) = findOneWithKV [Se.Is BeamPFS.personId $ Se.Eq personId] <&> (DPFS.flowStatus <$>)

updateStatus :: KvDbFlow m r => Id Person -> DPFS.FlowStatus -> m ()
updateStatus (Id personId) flowStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamPFS.flowStatus flowStatus, Se.Set BeamPFS.updatedAt now]
    [Se.Is BeamPFS.personId $ Se.Eq personId]

deleteByPersonId :: KvDbFlow m r => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamPFS.personId $ Se.Eq personId]

updateToIdleMultiple :: KvDbFlow m r => [Id Person] -> UTCTime -> m ()
updateToIdleMultiple personIds now =
  updateWithKV
    [Se.Set BeamPFS.flowStatus DPFS.IDLE, Se.Set BeamPFS.updatedAt now]
    [Se.Is BeamPFS.personId $ Se.In (getId <$> personIds)]
