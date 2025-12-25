module Storage.Queries.LocationExtra where

import Domain.Types.Location
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Location as BeamL
import Storage.Queries.OrphanInstances.Location ()

updateAddress :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Location -> LocationAddress -> m ()
updateAddress (Id blId) LocationAddress {..} = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamL.street street,
      Se.Set BeamL.door door,
      Se.Set BeamL.city city,
      Se.Set BeamL.state state,
      Se.Set BeamL.country country,
      Se.Set BeamL.building building,
      Se.Set BeamL.areaCode areaCode,
      Se.Set BeamL.area area,
      Se.Set BeamL.fullAddress fullAddress,
      Se.Set BeamL.updatedAt now
    ]
    [Se.Is BeamL.id (Se.Eq blId)]

getBookingLocs ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id Location] ->
  m [Location]
getBookingLocs locationIds = findAllWithKV [Se.Is BeamL.id $ Se.In $ getId <$> locationIds]
