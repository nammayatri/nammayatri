module Storage.Queries.MerchantExtra where

import Domain.Types.Merchant as DM
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as BeamM
import Storage.Queries.OrphanInstances.Merchant ()

-- Extra code goes here --
loadAllProviders :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Merchant]
loadAllProviders = do
  findAllWithDb [Se.And [Se.Is BeamM.status $ Se.Eq DM.APPROVED, Se.Is BeamM.enabled $ Se.Eq True]]

findAll :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Merchant]
findAll = findAllWithDb [Se.Is BeamM.id $ Se.Not $ Se.Eq $ getId ""]

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Merchant -> m ()
update org = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamM.name org.name,
      Se.Set BeamM.description org.description,
      Se.Set BeamM.headCount org.headCount,
      Se.Set BeamM.enabled org.enabled,
      Se.Set BeamM.updatedAt now,
      Se.Set BeamM.fromTime org.fromTime
    ]
    [Se.Is BeamM.id (Se.Eq (getId org.id))]

findAllShortIdById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Merchant] -> m [ShortId Merchant]
findAllShortIdById merchantIds =
  (DM.shortId <$>)
    <$> findAllWithKV
      [Se.Is BeamM.id $ Se.In (getId <$> merchantIds)]

updateGeofencingConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> GeoRestriction -> GeoRestriction -> m ()
updateGeofencingConfig merchantId originRestriction destinationRestriction = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamM.originRestriction originRestriction,
      Se.Set BeamM.destinationRestriction destinationRestriction,
      Se.Set BeamM.updatedAt now
    ]
    [Se.Is BeamM.id (Se.Eq (getId merchantId))]
