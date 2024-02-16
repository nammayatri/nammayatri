{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.InterCityTravelCities where

import qualified Domain.Types.InterCityTravelCities
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.InterCityTravelCities as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.InterCityTravelCities.InterCityTravelCities -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.InterCityTravelCities.InterCityTravelCities] -> m ()
createMany = traverse_ create

findByMerchantAndState :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m ([Domain.Types.InterCityTravelCities.InterCityTravelCities])
findByMerchantAndState (Kernel.Types.Id.Id merchantId) state = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.state $ Se.Eq state
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe (Domain.Types.InterCityTravelCities.InterCityTravelCities))
findByPrimaryKey cityName (Kernel.Types.Id.Id merchantId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.cityName $ Se.Eq cityName,
          Se.Is Beam.merchantId $ Se.Eq merchantId
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.InterCityTravelCities.InterCityTravelCities -> m ()
updateByPrimaryKey Domain.Types.InterCityTravelCities.InterCityTravelCities {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.lat lat,
      Se.Set Beam.lng lng,
      Se.Set Beam.state state,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.cityName $ Se.Eq cityName,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

instance FromTType' Beam.InterCityTravelCities Domain.Types.InterCityTravelCities.InterCityTravelCities where
  fromTType' Beam.InterCityTravelCitiesT {..} = do
    pure $
      Just
        Domain.Types.InterCityTravelCities.InterCityTravelCities
          { cityName = cityName,
            lat = lat,
            lng = lng,
            merchantId = Kernel.Types.Id.Id merchantId,
            state = state,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.InterCityTravelCities Domain.Types.InterCityTravelCities.InterCityTravelCities where
  toTType' Domain.Types.InterCityTravelCities.InterCityTravelCities {..} = do
    Beam.InterCityTravelCitiesT
      { Beam.cityName = cityName,
        Beam.lat = lat,
        Beam.lng = lng,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.state = state,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
