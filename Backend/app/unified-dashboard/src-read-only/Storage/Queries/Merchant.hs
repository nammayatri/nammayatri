{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Merchant (module Storage.Queries.Merchant, module ReExport) where

import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as Beam
import Storage.Queries.MerchantExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Merchant.Merchant -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Merchant.Merchant] -> m ())
createMany = traverse_ create

findAllByShortIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant] -> m [Domain.Types.Merchant.Merchant])
findAllByShortIds shortId = do findAllWithKV [Se.Is Beam.shortId $ Se.In (Kernel.Types.Id.getShortId <$> shortId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Merchant.Merchant))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Merchant.Merchant))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

updateEnableStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> m ())
updateEnableStatus enabled shortId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Merchant.Merchant))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Merchant.Merchant -> m ())
updateByPrimaryKey (Domain.Types.Merchant.Merchant {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.authTokenEncrypted (authToken <&> unEncrypted . (.encrypted)),
      Se.Set Beam.authTokenHash (authToken <&> (.hash)),
      Se.Set Beam.defaultOperatingCity defaultOperatingCity,
      Se.Set Beam.domain domain,
      Se.Set Beam.enableGetRequestAuditLogs enableGetRequestAuditLogs,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.hasFleetMemberHierarchy hasFleetMemberHierarchy,
      Se.Set Beam.is2faMandatory is2faMandatory,
      Se.Set Beam.isStrongNameCheckRequired isStrongNameCheckRequired,
      Se.Set Beam.requireAdminApprovalForFleetOnboarding requireAdminApprovalForFleetOnboarding,
      Se.Set Beam.serverNames serverNames,
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.singleActiveSessionOnly singleActiveSessionOnly,
      Se.Set Beam.supportedOperatingCities supportedOperatingCities,
      Se.Set Beam.verifyFleetWhileLogin verifyFleetWhileLogin,
      Se.Set Beam.website website,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
