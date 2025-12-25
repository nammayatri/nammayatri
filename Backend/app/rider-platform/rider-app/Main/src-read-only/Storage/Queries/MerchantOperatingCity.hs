{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantOperatingCity where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantOperatingCity as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantOperatingCity.MerchantOperatingCity] -> m ())
createMany = traverse_ create

findAllByMerchantIdAndState ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m [Domain.Types.MerchantOperatingCity.MerchantOperatingCity])
findAllByMerchantIdAndState merchantId state = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.state $ Se.Eq state]]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByMerchantIdAndCity merchantId city = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.city $ Se.Eq city]]

findByMerchantShortIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByMerchantShortIdAndCity merchantShortId city = do findOneWithKV [Se.And [Se.Is Beam.merchantShortId $ Se.Eq (Kernel.Types.Id.getShortId merchantShortId), Se.Is Beam.city $ Se.Eq city]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateByPrimaryKey (Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.city city,
      Se.Set Beam.country country,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverOfferMerchantOperatingCityId driverOfferMerchantOperatingCityId,
      Se.Set Beam.lat lat,
      Se.Set Beam.long long,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantShortId (Kernel.Types.Id.getShortId merchantShortId),
      Se.Set Beam.state state,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  fromTType' (Beam.MerchantOperatingCityT {..}) = do
    pure $
      Just
        Domain.Types.MerchantOperatingCity.MerchantOperatingCity
          { city = city,
            country = country,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverOfferMerchantOperatingCityId = driverOfferMerchantOperatingCityId,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            long = long,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantShortId = Kernel.Types.Id.ShortId merchantShortId,
            state = state,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  toTType' (Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..}) = do
    Beam.MerchantOperatingCityT
      { Beam.city = city,
        Beam.country = country,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverOfferMerchantOperatingCityId = driverOfferMerchantOperatingCityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.long = long,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantShortId = Kernel.Types.Id.getShortId merchantShortId,
        Beam.state = state,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
