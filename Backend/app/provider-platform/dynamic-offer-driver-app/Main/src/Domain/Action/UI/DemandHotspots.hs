{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DemandHotspots
  ( getDriverDemandHotspots,
    updateDemandHotspotsOnSearch,
    updateDemandHotspotsOnBooking,
  )
where

import API.Types.UI.DemandHotspots
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Geohash as Geohash
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as T
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.TransporterConfig
import Environment
import EulerHS.Prelude hiding (id, map, mapM_, whenJust)
import GHC.Num.Integer (integerToInt)
import Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateAplhaNumbericCode, getCurrentTime)
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Time (utcToMilliseconds)
import Servant hiding (throwError)
import qualified Storage.Cac.TransporterConfig as CCT
import Tools.Auth

-- What it is:
-- Entire city is divided into geohashes, whenever we encounter a search, we increase the frequency and whenver a booking happens
-- we decrease the frequency, so hotpsots depics unserved searches in that geohash.
--
-- How it works:
--
-- Redis data structures we are using - set and sorted set
-- Set - For storing all geohashes we have in a city
-- Sorted Set - Storing searches categorised by cityId and geohash
--
-- On Search - We add a uuid to the sorted set of that city and geohash
-- On Booking - We reomve the uuid with highest score from sorted set of that city and geohash

getDriverDemandHotspots ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow GetDemandHotspotsResp
  )
getDriverDemandHotspots (_, _, merchantOpCityId) = do
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      if configs.enableDemandHotspots
        then do
          let cachedResultKey = mkDemandHotspotCachedKey merchantOpCityId.getId
          cachedResult :: Maybe GetDemandHotspotsResp <- Redis.safeGet cachedResultKey
          case cachedResult of
            Just res -> return res
            Nothing -> calculateDemandHotspots configs cachedResultKey
        else do
          throwError $ InvalidRequest "Demand Hotspots is not enabled"
    _ -> throwError $ InvalidRequest "Demand Hotspots feature configs not set"
  where
    calculateDemandHotspots configs cachedResultKey =
      Redis.withWaitAndLockRedis (mkHotspotsCalculationLockKey merchantOpCityId.getId) 10 10000 $ do
        --1e4 microseconds
        cachedResult :: Maybe GetDemandHotspotsResp <- Redis.safeGet cachedResultKey
        case cachedResult of
          Just res -> return res
          Nothing -> do
            activeGeohashes :: [Text] <- Redis.sMembers (mkGeohashSetKey merchantOpCityId.getId)
            now <- getCurrentTime
            let allSortedSetsKeysWithGeohash :: [(Text, Text)] = map (\gh -> (mkDemandHotspotSortedSetKey merchantOpCityId.getId gh, gh)) activeGeohashes
                expiryTimeForSSObjects = T.addUTCTime (- 1 * 60 * fromIntegral configs.analysisDurationMinutes) now
            mapM_ (\(key, _) -> Redis.zRemRangeByScore key 0 (utcToMilliseconds expiryTimeForSSObjects)) allSortedSetsKeysWithGeohash
            results :: [(Int, Maybe Maps.LatLong)] <- mapM getFrequencyWithGeohash allSortedSetsKeysWithGeohash
            let finalResults :: [(Int, Maps.LatLong)] = take configs.noOfGeohashesToReturn $ sortBy (flip compare) $ filter (\(freq, _) -> freq > 0) [(freq, lat) | (freq, Just lat) <- results]
            let resp =
                  GetDemandHotspotsResp
                    { createdAt = now,
                      expiryAt = T.addUTCTime (60 * fromIntegral configs.resultDurationMinutes) now,
                      hotspotsDetails = map (uncurry HotspotsDetails) finalResults
                    }
            Redis.setExp cachedResultKey resp (60 * configs.resultDurationMinutes)
            return resp

    getFrequencyWithGeohash (sortedSetKey, geohash) = do
      frequency <- fmap integerToInt (Redis.zCard sortedSetKey)
      when (frequency == 0) $ do
        void $ Redis.srem (mkGeohashSetKey merchantOpCityId.getId) [geohash]
      let mblatlon :: Maybe (Double, Double) = Geohash.decode $ T.unpack geohash
      case mblatlon of
        Just (lat, lon) ->
          return (frequency, Just $ Maps.LatLong lat lon)
        Nothing -> do
          logDebug $ "Decode function gave Nothing for the geohash: " <> geohash <> " in demand hotspots"
          return (frequency, Nothing)

updateDemandHotspotsOnSearch :: Id MerchantOperatingCity -> TransporterConfig -> Maps.LatLong -> Flow ()
updateDemandHotspotsOnSearch merchantOpCityId transporterConfig latlong = do
  uuid <- generateAplhaNumbericCode 6
  now <- getCurrentTime
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when configs.enableDemandHotspots $ do
        let mbGeohash = Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)
        whenJust mbGeohash $ \geohash -> do
          let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
              geohashSetKey = mkGeohashSetKey merchantOpCityId.getId
              expirationSecond = 60 * configs.analysisDurationMinutes
          Redis.zAddExp sortedSetKey uuid (round $ utcToMilliseconds now) expirationSecond
          Redis.sAddExp geohashSetKey [geohash] expirationSecond
    _ -> logDebug "Demand hotspots not enabled or configs not set on search"

updateDemandHotspotsOnBooking ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id MerchantOperatingCity ->
  TransporterConfig ->
  Maps.LatLong ->
  m ()
updateDemandHotspotsOnBooking merchantOpCityId transporterConfig latlong = do
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when configs.enableDemandHotspots $ do
        let mbGeohash = Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)
        whenJust mbGeohash $ \geohash -> do
          let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
          elements <- Redis.zRange sortedSetKey (-1) (-1)
          let mbLastElement = TE.decodeUtf8 <$> listToMaybe elements
          whenJust mbLastElement $ \lastElement -> do
            res <- Redis.zRem sortedSetKey [lastElement]
            logDebug $ "Last element " <> lastElement <> " and deleted members count " <> show res
    _ -> logDebug "Demand hotspots not enabled or configs not set on booking"

mkDemandHotspotCachedKey :: Text -> Text
mkDemandHotspotCachedKey opCityId = "DH:CK:cityId:" <> opCityId

mkGeohashSetKey :: Text -> Text
mkGeohashSetKey opCityId = "DH:GSK:cityId:" <> opCityId

mkDemandHotspotSortedSetKey :: Text -> Text -> Text
mkDemandHotspotSortedSetKey opCityId geohash = "DH:cityId:" <> opCityId <> "GH:" <> geohash

mkHotspotsCalculationLockKey :: Text -> Text
mkHotspotsCalculationLockKey opCityId = "DH:CalcLock:cityId:" <> opCityId
