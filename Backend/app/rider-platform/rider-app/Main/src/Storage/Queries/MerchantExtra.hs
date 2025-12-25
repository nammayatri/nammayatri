module Storage.Queries.MerchantExtra where

import Domain.Types.Merchant as DOrg
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as BeamM
import Storage.Queries.OrphanInstances.Merchant ()

-- Extra code goes here --
findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Merchant]
findAll = findAllWithKV [Se.Is BeamM.id $ Se.Not $ Se.Eq $ getId ""]

update :: (MonadFlow m, EsqDBFlow m r) => Merchant -> m ()
update org = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamM.name org.name,
      Se.Set BeamM.gatewayUrl (showBaseUrl org.gatewayUrl),
      Se.Set BeamM.registryUrl (showBaseUrl org.registryUrl),
      Se.Set BeamM.updatedAt now
    ]
    [Se.Is BeamM.id (Se.Eq (getId org.id))]

updateGeofencingConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> GeoRestriction -> GeoRestriction -> m ()
updateGeofencingConfig merchantId originRestriction destinationRestriction = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamM.originRestriction originRestriction,
      Se.Set BeamM.destinationRestriction destinationRestriction,
      Se.Set BeamM.updatedAt now
    ]
    [Se.Is BeamM.id (Se.Eq (getId merchantId))]
