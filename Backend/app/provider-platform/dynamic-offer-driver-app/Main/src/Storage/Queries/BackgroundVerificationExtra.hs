module Storage.Queries.BackgroundVerificationExtra where

import Domain.Types.BackgroundVerification
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.BackgroundVerification as Beam
import Storage.Queries.OrphanInstances.BackgroundVerification ()

-- Extra code goes here --

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BackgroundVerification -> m ()
upsert backgroundVerification@(BackgroundVerification {..}) = do
  res <- findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
  if isJust res
    then
      updateWithKV
        [ Se.Set Beam.candidateId candidateId,
          Se.Set Beam.expiresAt expiresAt,
          Se.Set Beam.invitationId invitationId,
          Se.Set Beam.invitationStatus invitationStatus,
          Se.Set Beam.invitationUrl (showBaseUrl invitationUrl),
          Se.Set Beam.reportId reportId,
          Se.Set Beam.reportStatus reportStatus,
          Se.Set Beam.merchantId merchantId.getId,
          Se.Set Beam.merchantOperatingCityId merchantOperatingCityId.getId,
          Se.Set Beam.createdAt createdAt,
          Se.Set Beam.updatedAt updatedAt
        ]
        [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
    else createWithKV backgroundVerification
