{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AppDynamicLogic where

import qualified Data.Aeson
import qualified Data.String.Conversions
import qualified Data.Text
import qualified Domain.Types.AppDynamicLogic
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TimeBound
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AppDynamicLogic as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppDynamicLogic.AppDynamicLogic -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AppDynamicLogic.AppDynamicLogic] -> m ())
createMany = traverse_ create

findByMerchantOpCityAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Data.Text.Text -> m [Domain.Types.AppDynamicLogic.AppDynamicLogic])
findByMerchantOpCityAndDomain limit offset merchantOperatingCityId domain = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.domain $ Se.Eq domain
        ]
    ]
    (Se.Asc Beam.order)
    limit
    offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Text.Text -> Data.Text.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m (Maybe Domain.Types.AppDynamicLogic.AppDynamicLogic))
findByPrimaryKey domain name merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.name $ Se.Eq name,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppDynamicLogic.AppDynamicLogic -> m ())
updateByPrimaryKey (Domain.Types.AppDynamicLogic.AppDynamicLogic {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.logic ((Data.String.Conversions.cs . Data.Aeson.encode) logic),
      Se.Set Beam.order order,
      Se.Set Beam.timeBounds (Kernel.Prelude.Just timeBounds),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.name $ Se.Eq name, Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]]

instance FromTType' Beam.AppDynamicLogic Domain.Types.AppDynamicLogic.AppDynamicLogic where
  fromTType' (Beam.AppDynamicLogicT {..}) = do
    pure $
      Just
        Domain.Types.AppDynamicLogic.AppDynamicLogic
          { description = description,
            domain = domain,
            logic = (Kernel.Prelude.fromMaybe Data.Aeson.Null . Data.Aeson.decode . Data.String.Conversions.cs) logic,
            name = name,
            order = order,
            timeBounds = Kernel.Prelude.fromMaybe Domain.Types.TimeBound.Unbounded timeBounds,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogic Domain.Types.AppDynamicLogic.AppDynamicLogic where
  toTType' (Domain.Types.AppDynamicLogic.AppDynamicLogic {..}) = do
    Beam.AppDynamicLogicT
      { Beam.description = description,
        Beam.domain = domain,
        Beam.logic = (Data.String.Conversions.cs . Data.Aeson.encode) logic,
        Beam.name = name,
        Beam.order = order,
        Beam.timeBounds = Kernel.Prelude.Just timeBounds,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
