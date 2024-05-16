{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RegistrationTokenExtra where

import Domain.Types.Person
import Domain.Types.RegistrationToken as DRT
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.OrphanInstances.RegistrationToken
import Tools.Error

-- Extra code goes here --

getAlternateNumberAttempts :: KvDbFlow m r => Id Person -> m Int
getAlternateNumberAttempts (Id personId) = findOneWithKV [Se.Is BeamRT.entityId $ Se.Eq personId] <&> maybe 5 DRT.attempts

deleteByPersonIdExceptNew :: KvDbFlow m r => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]
