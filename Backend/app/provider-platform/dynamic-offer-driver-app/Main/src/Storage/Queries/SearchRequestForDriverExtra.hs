{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequestForDriverExtra where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as T
import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Kernel.Utils.Version
import qualified Sequelize as Se
import SharedLogic.DriverPool.Types
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.OrphanInstances.SearchRequestForDriver
import qualified Storage.Queries.SearchRequest as QR

-- Extra code goes here --

searchReqestForDriverkey :: Text -> Text
searchReqestForDriverkey prefix = "searchRequestForDriver_" <> prefix

searchReqestForDriverkeyExpiry :: Integer
searchReqestForDriverkeyExpiry = 5 * 60 -- 5 mins

createMany :: (KvDbFlow m r, HedisFlow m r) => [SearchRequestForDriver] -> m ()
createMany = traverse_ createOne
  where
    createOne :: (KvDbFlow m r, HedisFlow m r) => SearchRequestForDriver -> m ()
    createOne srd = do
      now <- getCurrentTime
      when (srd.status == Domain.Active) $ do
        let driverId = getId $ Domain.driverId srd
        clearOldSrfdIds driverId
        void $
          Hedis.withCrossAppRedis $ Hedis.zAddExp (searchReqestForDriverkey driverId) (getId $ Domain.id srd) (round $ utcToMilliseconds now) (fromInteger searchReqestForDriverkeyExpiry)
      createWithKV srd

    clearOldSrfdIds :: (MonadFlow m, HedisFlow m r) => Text -> m ()
    clearOldSrfdIds driverId = do
      now <- getCurrentTime
      let startTime = T.addUTCTime (-1 * (fromIntegral $ searchReqestForDriverkeyExpiry + 1)) now -- sorted set key would expire beyond this anyways
      let endTime = T.addUTCTime (-60) now -- conservative 1min lookback time to accomodate different configs for SearchRequestForDriver expiry
      void $ Hedis.withCrossAppRedis $ Hedis.zRemRangeByScore (searchReqestForDriverkey driverId) (utcToMilliseconds startTime) (utcToMilliseconds endTime)

setInactiveBySTId :: (KvDbFlow m r, HedisFlow m r) => Id SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = do
  srfds <- findAllWithKV [Se.And [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  mapM_ (\s -> void $ Hedis.withCrossAppRedis $ Hedis.zRem (searchReqestForDriverkey $ getId $ Domain.driverId s) [getId $ Domain.id s]) srfds -- this will remove the key from redis
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]

setInactiveAndPulledByIds :: KvDbFlow m r => [Id SearchRequestForDriver] -> m ()
setInactiveAndPulledByIds srdIds = do
  srfd <- findAllWithKV [Se.And [Se.Is BeamSRFD.id (Se.In $ (.getId) <$> srdIds), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  mapM_ (\s -> void $ Hedis.withCrossAppRedis $ Hedis.zRem (searchReqestForDriverkey $ getId $ Domain.driverId s) [getId $ Domain.id s]) srfd -- this will remove the key from redis
  updateWithKV
    [ Se.Set BeamSRFD.status Domain.Inactive,
      Se.Set BeamSRFD.response (Just Domain.Pulled)
    ]
    [Se.Is BeamSRFD.id (Se.In $ (.getId) <$> srdIds)]

findByDriver :: (KvDbFlow m r, HedisFlow m r) => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  now <- getCurrentTime
  let startTime = T.addUTCTime (-60) now -- conservative 1min lookback time to accomodate different configs for SearchRequestForDriver expiry
  srfdIds <-
    Hedis.withCrossAppRedis $ Hedis.zRangeByScore (searchReqestForDriverkey driverId) (utcToMilliseconds startTime) (utcToMilliseconds now)
  findAllWithOptionsKV [Se.And [Se.Is BeamSRFD.id $ Se.In (map TE.decodeUtf8 srfdIds), Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]] (Se.Desc BeamSRFD.searchRequestValidTill) Nothing Nothing

findByDriverAndSearchTryId :: KvDbFlow m r => Id Person -> Id SearchTry -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchTryId (Id driverId) (Id searchTryId) =
  findOneWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.driverId $ Se.Eq driverId]
        )
    ]

deleteByDriverId :: (KvDbFlow m r, HedisFlow m r) => Id Person -> m ()
deleteByDriverId (Id personId) = do
  void $ Hedis.del (searchReqestForDriverkey personId)
  deleteWithKV
    [Se.Is BeamSRFD.driverId (Se.Eq personId)]
