{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PopularLocation (module Storage.Queries.PopularLocation, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.PopularLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PopularLocation as Beam
import Storage.Queries.PopularLocationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PopularLocation.PopularLocation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PopularLocation.PopularLocation] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.PopularLocation.PopularLocation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PopularLocation.PopularLocation -> m ())
updateByPrimaryKey (Domain.Types.PopularLocation.PopularLocation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.rating rating,
      Se.Set Beam.type_ type_,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq id]]
