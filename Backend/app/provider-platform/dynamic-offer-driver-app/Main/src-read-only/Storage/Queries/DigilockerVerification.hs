{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DigilockerVerification (module Storage.Queries.DigilockerVerification, module ReExport) where

import qualified Domain.Types.DigilockerVerification
import qualified Domain.Types.DocStatus
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DigilockerVerification as Beam
import Storage.Queries.DigilockerVerificationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DigilockerVerification.DigilockerVerification -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DigilockerVerification.DigilockerVerification] -> m ())
createMany = traverse_ create

deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.DigilockerVerification.DigilockerVerification]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DigilockerVerification.DigilockerVerification -> m (Maybe Domain.Types.DigilockerVerification.DigilockerVerification))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByStateId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.DigilockerVerification.DigilockerVerification))
findByStateId stateId = do findOneWithKV [Se.Is Beam.stateId $ Se.Eq stateId]

findLatestByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.DigilockerVerification.DigilockerVerification]))
findLatestByDriverId limit offset driverId = do findAllWithOptionsKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)] (Se.Desc Beam.createdAt) limit offset

updateDocStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocStatus.DocStatusMap -> Kernel.Types.Id.Id Domain.Types.DigilockerVerification.DigilockerVerification -> m ())
updateDocStatus docStatus id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.docStatus docStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateSessionStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DigilockerVerification.SessionStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.DigilockerVerification.DigilockerVerification -> m ())
updateSessionStatus sessionStatus responseCode responseDescription id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.sessionStatus sessionStatus,
      Se.Set Beam.responseCode responseCode,
      Se.Set Beam.responseDescription responseDescription,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DigilockerVerification.DigilockerVerification -> m (Maybe Domain.Types.DigilockerVerification.DigilockerVerification))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DigilockerVerification.DigilockerVerification -> m ())
updateByPrimaryKey (Domain.Types.DigilockerVerification.DigilockerVerification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accessToken (accessToken <&> unEncrypted),
      Se.Set Beam.accessTokenExpiresAt accessTokenExpiresAt,
      Se.Set Beam.authorizationCode authorizationCode,
      Se.Set Beam.codeChallenge codeChallenge,
      Se.Set Beam.codeMethod codeMethod,
      Se.Set Beam.codeVerifier codeVerifier,
      Se.Set Beam.docStatus docStatus,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.responseCode responseCode,
      Se.Set Beam.responseDescription responseDescription,
      Se.Set Beam.scope scope,
      Se.Set Beam.sessionStatus sessionStatus,
      Se.Set Beam.stateId stateId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
