{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ServicePeopleCategory (module Storage.Queries.ServicePeopleCategory, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ServicePeopleCategory as Beam
import Storage.Queries.ServicePeopleCategoryExtra as ReExport
import Storage.Queries.Transformers.ServicePeopleCategory

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ServicePeopleCategory.ServicePeopleCategory] -> m ())
createMany = traverse_ create

findAllServicePeopleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m [Domain.Types.ServicePeopleCategory.ServicePeopleCategory])
findAllServicePeopleCategory id = do findAllWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m (Maybe Domain.Types.ServicePeopleCategory.ServicePeopleCategory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m ())
updateByPrimaryKey (Domain.Types.ServicePeopleCategory.ServicePeopleCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cancellationCharges (convertCancellationChargesToTable cancellationCharges),
      Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) pricePerUnit),
      Se.Set Beam.pricePerUnit ((.amount) pricePerUnit),
      Se.Set Beam.pricingType (Kernel.Prelude.Just pricingType),
      Se.Set Beam.timeBounds (Kernel.Prelude.Just timeBounds),
      Se.Set Beam.vendorSplitDetails (Data.Aeson.toJSON <$> vendorSplitDetails),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
