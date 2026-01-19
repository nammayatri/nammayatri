module Storage.Queries.SearchRequestForDriverExtra where

import qualified Data.Text.Encoding as TE
import qualified Data.Time as T
import Domain.Types.Common as Domain
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchTry
import qualified Domain.Types.TransporterConfig as TC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version (DeviceType (..))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD
import qualified Storage.Cac.TransporterConfig as CTC
import Storage.Queries.OrphanInstances.SearchRequestForDriver ()
import qualified Storage.Queries.Person as QP

-- Extra code goes here --

searchReqestForDriverkey :: Text -> Text
searchReqestForDriverkey prefix = "searchRequestForDriver_" <> prefix

searchReqestForDriverkeyExpiry :: Integer
searchReqestForDriverkeyExpiry = 5 * 60 -- 5 mins

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => [SearchRequestForDriver] -> m ()
createMany = traverse_ createOne
  where
    createOne :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => SearchRequestForDriver -> m ()
    createOne srd = do
      now <- getCurrentTime
      (personOS, merchantOpCityId) <- getDriverMobileType (Domain.driverId srd)
      transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing
      isDriverValid <- isDriverValidToCache (transporterConfig <&> TC.cachedDevicesOSForSearchRequest) personOS
      when (isDriverValid && srd.status == Domain.Active) $ do
        let driverId = getId $ Domain.driverId srd
        clearOldSrfdIds driverId
        void $ Hedis.withCrossAppRedis $ Hedis.zAddExp (searchReqestForDriverkey driverId) (getId $ Domain.id srd) (round $ utcToMilliseconds now) (fromInteger searchReqestForDriverkeyExpiry)
      createWithKV srd

    clearOldSrfdIds :: (MonadFlow m, HedisFlow m r) => Text -> m ()
    clearOldSrfdIds driverId = do
      now <- getCurrentTime
      let startTime = T.addUTCTime (- 1 * fromIntegral (searchReqestForDriverkeyExpiry + 1)) now -- sorted set key would expire beyond this anyways
      let endTime = T.addUTCTime (-60) now -- conservative 1min lookback time to accomodate different configs for SearchRequestForDriver expiry
      void $ Hedis.withCrossAppRedis $ Hedis.zRemRangeByScore (searchReqestForDriverkey driverId) (utcToMilliseconds startTime) (utcToMilliseconds endTime)

setInactiveBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => Id SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = do
  srfds <- findAllWithKV [Se.And [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  now <- getCurrentTime
  mapM_
    ( \s -> do
        (personOS, merchantOpCityId) <- getDriverMobileType (Domain.driverId s)
        transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing
        isDriverValid <- isDriverValidToCache (transporterConfig <&> TC.cachedDevicesOSForSearchRequest) personOS
        when isDriverValid do
          void $ Hedis.withCrossAppRedis $ Hedis.zRem (searchReqestForDriverkey $ getId $ Domain.driverId s) [getId $ Domain.id s]
    )
    srfds -- this will remove the key from redis
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive, Se.Set BeamSRFD.updatedAt (Just now)]
    [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]

setInactiveAndPulledByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id SearchRequestForDriver] -> m ()
setInactiveAndPulledByIds srdIds = do
  srfds <- findAllWithKV [Se.And [Se.Is BeamSRFD.id (Se.In $ (.getId) <$> srdIds), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  now <- getCurrentTime
  mapM_
    ( \s -> do
        (personOS, merchantOpCityId) <- getDriverMobileType (Domain.driverId s)
        transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing
        isDriverValid <- isDriverValidToCache (transporterConfig <&> TC.cachedDevicesOSForSearchRequest) personOS
        when isDriverValid do
          void $ Hedis.withCrossAppRedis $ Hedis.zRem (searchReqestForDriverkey $ getId $ Domain.driverId s) [getId $ Domain.id s]
    )
    srfds -- this will remove the key from redis
  updateWithKV
    [ Se.Set BeamSRFD.status Domain.Inactive,
      Se.Set BeamSRFD.response (Just Domain.Pulled),
      Se.Set BeamSRFD.updatedAt (Just now)
    ]
    [Se.Is BeamSRFD.id (Se.In $ (.getId) <$> srdIds)]

findByDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  now <- getCurrentTime
  (personOS, merchantOpCityId) <- getDriverMobileType (Id driverId)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing
  isDriverValid <- isDriverValidToCache (transporterConfig <&> TC.cachedDevicesOSForSearchRequest) personOS
  if isDriverValid
    then do
      let startTime = T.addUTCTime (-60) now -- conservative 1min lookback time to accomodate different configs for SearchRequestForDriver expiry
      srfdIds <- Hedis.runInSecondaryRedis $ Hedis.withCrossAppRedis $ Hedis.zRangeByScore (searchReqestForDriverkey driverId) (utcToMilliseconds startTime) (utcToMilliseconds now)
      findAllWithOptionsKV [Se.And [Se.Is BeamSRFD.id $ Se.In (map TE.decodeUtf8 srfdIds), Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]] (Se.Desc BeamSRFD.searchRequestValidTill) Nothing Nothing
    else do
      findAllWithOptionsKV [Se.And [Se.Is BeamSRFD.driverId $ Se.Eq driverId, Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]] (Se.Desc BeamSRFD.searchRequestValidTill) Nothing Nothing

findByDriverAndSearchTryId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id SearchTry -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchTryId (Id driverId) (Id searchTryId) =
  findOneWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.driverId $ Se.Eq driverId]
        )
    ]

deleteByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => Id Person -> m ()
deleteByDriverId (Id personId) = do
  (personOS, merchantOpCityId) <- getDriverMobileType (Id personId)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing
  isDriverValid <- isDriverValidToCache (transporterConfig <&> TC.cachedDevicesOSForSearchRequest) personOS
  when isDriverValid $
    void $ Hedis.withCrossAppRedis $ Hedis.del (searchReqestForDriverkey personId)
  deleteWithKV
    [Se.Is BeamSRFD.driverId (Se.Eq personId)]

getDriverMobileType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe DeviceType, Id MerchantOperatingCity)
getDriverMobileType personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let personOs :: Maybe DeviceType = person.clientDevice <&> (.deviceType)
  pure (personOs, person.merchantOperatingCityId)

isDriverValidToCache :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe [DeviceType] -> Maybe DeviceType -> m Bool
isDriverValidToCache validDeviceTypes driverDeviceType = do
  case (validDeviceTypes, driverDeviceType) of
    (Just valDeviceTypes, Just dDeviceType) -> pure $ dDeviceType `elem` valDeviceTypes
    _ -> pure False

findAnyAcceptedBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchTry -> m (Maybe SearchRequestForDriver)
findAnyAcceptedBySTId (Id searchTryId) = do
  findOneWithKV [Se.And [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId, Se.Is BeamSRFD.response $ Se.Eq (Just Domain.Accept)]]
