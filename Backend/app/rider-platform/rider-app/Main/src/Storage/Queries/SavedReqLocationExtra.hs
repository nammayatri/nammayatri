module Storage.Queries.SavedReqLocationExtra where

import Domain.Types.Person (Person)
import Domain.Types.SavedReqLocation
import Kernel.Beam.Functions
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SavedReqLocation as BeamSRL
import Storage.Queries.OrphanInstances.SavedReqLocation ()

-- Extra code goes here --
findAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [SavedReqLocation]
findAllByRiderId perId = findAllWithOptionsKV [Se.Is BeamSRL.riderId $ Se.Eq (getId perId)] (Se.Desc BeamSRL.updatedAt) Nothing Nothing

deleteByRiderIdAndTag :: (MonadFlow m, EsqDBFlow m r) => Id Person -> Text -> m ()
deleteByRiderIdAndTag perId addressTag = deleteWithKV [Se.And [Se.Is BeamSRL.riderId (Se.Eq (getId perId)), Se.Is BeamSRL.tag (Se.Eq addressTag)]]

findAllByRiderIdAndTag :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Text -> m [SavedReqLocation]
findAllByRiderIdAndTag perId addressTag = findAllWithKV [Se.And [Se.Is BeamSRL.riderId (Se.Eq (getId perId)), Se.Is BeamSRL.tag (Se.Eq addressTag)]]

findByLatLonAndRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> LatLong -> m (Maybe SavedReqLocation)
findByLatLonAndRiderId personId LatLong {..} = findOneWithKV [Se.And [Se.Is BeamSRL.lat (Se.Eq lat), Se.Is BeamSRL.lon (Se.Eq lon), Se.Is BeamSRL.riderId (Se.Eq (getId personId))]]

countAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m Int
countAllByRiderId perId = findAllWithKV [Se.Is BeamSRL.riderId $ Se.Eq (getId perId)] <&> length
