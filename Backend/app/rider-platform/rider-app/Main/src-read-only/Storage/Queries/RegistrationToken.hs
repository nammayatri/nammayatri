{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RegistrationToken (module Storage.Queries.RegistrationToken, module ReExport) where

import qualified Domain.Types.RegistrationToken
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as Beam
import Storage.Queries.RegistrationTokenExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.RegistrationToken.RegistrationToken -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.RegistrationToken.RegistrationToken] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> m (Maybe Domain.Types.RegistrationToken.RegistrationToken))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

setVerified :: KvDbFlow m r => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> m ())
setVerified verified (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.verified verified, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateAttempts :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> m ())
updateAttempts attempts (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.attempts attempts, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> m (Maybe Domain.Types.RegistrationToken.RegistrationToken))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.RegistrationToken.RegistrationToken -> m ())
updateByPrimaryKey (Domain.Types.RegistrationToken.RegistrationToken {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.attempts attempts,
      Se.Set Beam.authExpiry authExpiry,
      Se.Set Beam.authMedium authMedium,
      Se.Set Beam.authType authType,
      Se.Set Beam.authValueHash authValueHash,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.info info,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.token token,
      Se.Set Beam.tokenExpiry tokenExpiry,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verified verified
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
