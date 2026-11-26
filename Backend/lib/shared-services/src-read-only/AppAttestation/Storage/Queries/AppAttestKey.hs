{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AppAttestation.Storage.Queries.AppAttestKey where

import qualified AppAttestation.Domain.Types.AppAttestKey
import qualified AppAttestation.Domain.Types.Common
import qualified AppAttestation.Storage.Beam.AppAttestKey as Beam
import qualified AppAttestation.Storage.BeamFlow
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se

create :: (AppAttestation.Storage.BeamFlow.BeamFlow m r) => (AppAttestation.Domain.Types.AppAttestKey.AppAttestKey -> m ())
create = createWithKV

findByKeyId :: (AppAttestation.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe AppAttestation.Domain.Types.AppAttestKey.AppAttestKey))
findByKeyId keyId = do findOneWithKV [Se.Is Beam.keyId $ Se.Eq keyId]

updateStatus :: (AppAttestation.Storage.BeamFlow.BeamFlow m r) => (AppAttestation.Domain.Types.Common.AppAttestKeyStatus -> Kernel.Prelude.Text -> m ())
updateStatus status keyId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.keyId $ Se.Eq keyId]

updateUsage :: (AppAttestation.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Text -> m ())
updateUsage signCount useCount lastUsedAt keyId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.signCount signCount, Se.Set Beam.useCount useCount, Se.Set Beam.lastUsedAt lastUsedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.keyId $ Se.Eq keyId]

instance FromTType' Beam.AppAttestKey AppAttestation.Domain.Types.AppAttestKey.AppAttestKey where
  fromTType' (Beam.AppAttestKeyT {..}) = do
    pure $
      Just
        AppAttestation.Domain.Types.AppAttestKey.AppAttestKey
          { bundleId = bundleId,
            createdAt = createdAt,
            environment = environment,
            keyId = keyId,
            lastUsedAt = lastUsedAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            publicKey = publicKey,
            receipt = receipt,
            signCount = signCount,
            status = status,
            teamId = teamId,
            updatedAt = updatedAt,
            useCount = useCount
          }

instance ToTType' Beam.AppAttestKey AppAttestation.Domain.Types.AppAttestKey.AppAttestKey where
  toTType' (AppAttestation.Domain.Types.AppAttestKey.AppAttestKey {..}) = do
    Beam.AppAttestKeyT
      { Beam.bundleId = bundleId,
        Beam.createdAt = createdAt,
        Beam.environment = environment,
        Beam.keyId = keyId,
        Beam.lastUsedAt = lastUsedAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.publicKey = publicKey,
        Beam.receipt = receipt,
        Beam.signCount = signCount,
        Beam.status = status,
        Beam.teamId = teamId,
        Beam.updatedAt = updatedAt,
        Beam.useCount = useCount
      }
