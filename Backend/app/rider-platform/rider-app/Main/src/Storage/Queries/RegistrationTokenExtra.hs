{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RegistrationTokenExtra where

import Domain.Types.Person
import Domain.Types.RegistrationToken
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT
import Storage.Queries.OrphanInstances.RegistrationToken

-- Extra code goes here --

deleteByPersonIdExceptNew :: KvDbFlow m r => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]

findByToken :: KvDbFlow m r => RegToken -> m (Maybe RegistrationToken)
findByToken token = findOneWithKV [Se.Is BeamRT.token $ Se.Eq token]

deleteByPersonId :: KvDbFlow m r => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId)]]

findAllByPersonId :: KvDbFlow m r => Id Person -> m [RegistrationToken]
findAllByPersonId personId = findAllWithKV [Se.Is BeamRT.entityId $ Se.Eq $ getId personId]

setDirectAuth :: KvDbFlow m r => Id RegistrationToken -> m ()
setDirectAuth (Id rtId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.verified True,
      Se.Set BeamRT.authMedium SIGNATURE,
      Se.Set BeamRT.authType DIRECT,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]
