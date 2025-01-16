module Storage.Queries.RegistrationTokenExtra where

import Domain.Types.Person
import Domain.Types.RegistrationToken as DRT
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT
import Storage.Queries.OrphanInstances.RegistrationToken ()

-- Extra code goes here --

getAlternateNumberAttempts :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m Int
getAlternateNumberAttempts (Id personId) = findOneWithKV [Se.Is BeamRT.entityId $ Se.Eq personId] <&> maybe 5 DRT.attempts

deleteByPersonIdExceptNew :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]
