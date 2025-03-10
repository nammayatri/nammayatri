{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantOperatingCity where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
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

findAllByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m [Domain.Types.MerchantOperatingCity.MerchantOperatingCity])
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

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

instance FromTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  fromTType' (Beam.MerchantOperatingCityT {..}) = do
    pure $
      Just
        Domain.Types.MerchantOperatingCity.MerchantOperatingCity
          { city = city,
            country = country,
            currency = fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            id = Kernel.Types.Id.Id id,
            language = language,
            location = Kernel.External.Maps.Types.LatLong lat lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantShortId = Kernel.Types.Id.ShortId merchantShortId,
            state = state,
            supportNumber = supportNumber
          }

instance ToTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  toTType' (Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..}) = do
    Beam.MerchantOperatingCityT
      { Beam.city = city,
        Beam.country = country,
        Beam.currency = Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.lat = (.lat) location,
        Beam.lon = (.lon) location,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantShortId = Kernel.Types.Id.getShortId merchantShortId,
        Beam.state = state,
        Beam.supportNumber = supportNumber
      }
