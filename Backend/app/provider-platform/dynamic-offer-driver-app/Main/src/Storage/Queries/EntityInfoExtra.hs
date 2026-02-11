module Storage.Queries.EntityInfoExtra where

import qualified Domain.Types.EntityInfo as DEI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.EntityInfo as Beam
import Storage.Queries.EntityInfo ()

-- Extra code goes here --
findAllByEntityIdTypeAndOpCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Text -> Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m [DEI.EntityInfo])
findAllByEntityIdTypeAndOpCity entityId entityType merchantId merchantOpCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOpCityId)
        ]
    ]
