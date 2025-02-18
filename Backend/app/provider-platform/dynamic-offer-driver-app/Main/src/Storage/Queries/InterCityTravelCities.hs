{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.InterCityTravelCities where

import Data.Either
import qualified Database.Beam as B
import qualified Domain.Types.InterCityTravelCities
import qualified Domain.Types.Merchant
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.InterCityTravelCities as Beam
import qualified Storage.Beam.InterCityTravelCities.InterCityTravelCitiesGeom as BeamGeom

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.InterCityTravelCities.InterCityTravelCities -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.InterCityTravelCities.InterCityTravelCities] -> m ())
createMany = traverse_ create

findByMerchantAndState ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m [Domain.Types.InterCityTravelCities.InterCityTravelCities])
findByMerchantAndState merchantId state = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.state $ Se.Eq state]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.InterCityTravelCities.InterCityTravelCities))
findByPrimaryKey cityName merchantId = do findOneWithKV [Se.And [Se.Is Beam.cityName $ Se.Eq cityName, Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.InterCityTravelCities.InterCityTravelCities -> m ())
updateByPrimaryKey (Domain.Types.InterCityTravelCities.InterCityTravelCities {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.lat lat, Se.Set Beam.lng lng, Se.Set Beam.state state, Se.Set Beam.createdAt createdAt, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.cityName $ Se.Eq cityName,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

findInterCityAreasContainingGps :: forall m r. (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> m [Domain.Types.InterCityTravelCities.InterCityTravelCities]
findInterCityAreasContainingGps gps = do
  dbConf <- getReplicaBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \Beam.InterCityTravelCitiesT {} ->
                containsPoint' (gps.lon, gps.lat)
            )
            $ B.all_ (BeamCommon.interCityTravelCities BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

instance FromTType' Beam.InterCityTravelCities Domain.Types.InterCityTravelCities.InterCityTravelCities where
  fromTType' (Beam.InterCityTravelCitiesT {..}) = do
    pure $
      Just
        Domain.Types.InterCityTravelCities.InterCityTravelCities
          { cityName = cityName,
            lat = lat,
            lng = lng,
            merchantId = Kernel.Types.Id.Id merchantId,
            state = state,
            geom = Nothing,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamGeom.InterCityTravelCitiesGeom Domain.Types.InterCityTravelCities.InterCityTravelCities where
  toTType' (Domain.Types.InterCityTravelCities.InterCityTravelCities {..}) = do
    BeamGeom.InterCityTravelCitiesGeomT
      { BeamGeom.cityName = cityName,
        BeamGeom.lat = lat,
        BeamGeom.lng = lng,
        BeamGeom.geom = geom,
        BeamGeom.merchantId = Kernel.Types.Id.getId merchantId,
        BeamGeom.state = state,
        BeamGeom.createdAt = createdAt,
        BeamGeom.updatedAt = updatedAt
      }
