{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Serviceability
  ( checkServiceability,
    checkServiceabilityAndGetCity,
    ServiceabilityRes (..),
  )
where

import API.UI.HotSpot
import qualified Domain.Types.HotSpot as DHotSpot
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import Kernel.Beam.Functions
import Kernel.External.Maps.Types hiding (geometry)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person as CQP
import Storage.Queries.Geometry (findGeometriesContaining)
import Tools.Error

data ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool,
    city :: Maybe Context.City,
    specialLocation :: Maybe DSpecialLocation.SpecialLocation,
    geoJson :: Maybe Text,
    hotSpotInfo :: [DHotSpot.HotSpotInfo],
    blockRadius :: Maybe Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

checkServiceability ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  LatLong ->
  Bool ->
  m ServiceabilityRes
checkServiceability settingAccessor (personId, merchantId) location shouldUpdatePerson = do
  let merchId = merchantId
  geoConfig <- fmap (.geofencingConfig) $ QMerchant.findById merchId >>= fromMaybeM (MerchantNotFound merchId.getId)
  let geoRestriction = settingAccessor geoConfig
  DHotSpot.HotSpotResponse {..} <- getHotspot location merchantId
  case geoRestriction of
    Unrestricted -> do
      let serviceable = True
      specialLocationBody <- QSpecialLocation.findSpecialLocationByLatLong location
      let city = Nothing
      pure ServiceabilityRes {serviceable = serviceable, specialLocation = fst <$> specialLocationBody, geoJson = snd <$> specialLocationBody, ..}
    Regions regions -> do
      geometry <-
        runInReplica $
          findGeometriesContaining location regions >>= \case
            [] -> do
              logError $ "No geometry found for location: " <> show location <> " for regions: " <> show regions <> " personId: " <> personId.getId
              pure Nothing
            (g : _) -> pure $ Just g

      let serviceable = isJust geometry
          city = (.city) <$> geometry
      _ <- upsertPersonCityInformation personId merchantId shouldUpdatePerson city
      if serviceable
        then do
          specialLocationBody <- QSpecialLocation.findSpecialLocationByLatLong location
          pure ServiceabilityRes {serviceable = serviceable, specialLocation = fst <$> specialLocationBody, geoJson = snd <$> specialLocationBody, ..}
        else pure ServiceabilityRes {serviceable = serviceable, specialLocation = Nothing, geoJson = Nothing, ..}

data ServiceabilityCityRes = ServiceabilityCityRes
  { serviceable :: Bool,
    city :: Maybe Context.City
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

checkServiceabilityAndGetCity ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  Bool ->
  LatLong ->
  m ServiceabilityCityRes
checkServiceabilityAndGetCity settingAccessor (personId, merchantId) shouldUpdatePerson location = do
  let merchId = merchantId
  geoConfig <- fmap (.geofencingConfig) $ QMerchant.findById merchId >>= fromMaybeM (MerchantNotFound merchId.getId)
  let geoRestriction = settingAccessor geoConfig
  case geoRestriction of
    Unrestricted -> pure ServiceabilityCityRes {serviceable = True, city = Nothing}
    Regions regions -> do
      geometry <-
        runInReplica $
          findGeometriesContaining location regions >>= \case
            [] -> do
              logError $ "No geometry found for location: " <> show location <> " for regions: " <> show regions <> " personId: " <> personId.getId
              pure Nothing
            (g : _) -> pure $ Just g

      let serviceable = isJust geometry
          city = (.city) <$> geometry
      _ <- upsertPersonCityInformation personId merchantId shouldUpdatePerson city
      pure ServiceabilityCityRes {..}

upsertPersonCityInformation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> Id Merchant.Merchant -> Bool -> Maybe Context.City -> m ()
upsertPersonCityInformation personId merchantId shouldUpdatePerson mbCity = when shouldUpdatePerson $
  whenJust mbCity $ \city' -> do
    personCityInfo <- CQP.findCityInfoById personId >>= fromMaybeM (PersonCityInformationDoesNotExist $ "personId:- " <> personId.getId)
    when (personCityInfo.currentCity /= city') $ do
      merchantOperatingCity <-
        CQMOC.findByMerchantIdAndCity merchantId city'
          >>= fromMaybeM
            ( MerchantOperatingCityNotFound $
                "merchantId:- " <> merchantId.getId <> " ,city:- " <> show city'
            )
      CQP.updateCityInfoById personId city' merchantOperatingCity.id
