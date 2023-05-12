{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MerchantConfig
  ( updateCustomerFraudCounters,
    updateCancelledByDriverFraudCounters,
    updateSearchFraudCounters,
    updateTotalRidesCounters,
    anyFraudDetected,
    mkCancellationKey,
    mkCancellationByDriverKey,
    searchFraudDetected,
    takeAction,
    bookingInfoStoreKey,
    BookingInfoStore (..),
  )
where

import Data.Foldable.Extra
import Data.Monoid
import qualified Data.Text as T
import qualified Domain.Types.Booking.Type as BT
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantConfig as DMC
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking.BookingLocation as QBL
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as RT
import Tools.Auth (authTokenCacheKey)

data Factors = MoreCancelling | MoreSearching | MoreCancelledByDriver | TotalRides

data BookingInfoStore = BookingInfoStore
  { placeId :: Maybe Text,
    description :: Text,
    lastAccessed :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

bookingInfoStoreKey :: Text
bookingInfoStoreKey = "Customer:BookingInfoStore"

mkCancellationKey :: Text -> Text -> Text
mkCancellationKey ind idtxt = "Customer:CancellationCount:" <> idtxt <> ":" <> ind

mkCancellationByDriverKey :: Text -> Text -> Text
mkCancellationByDriverKey ind idtxt = "Customer:CancellationByDriverCount:" <> idtxt <> ":" <> ind

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey idtxt = "Customer:TotalRidesCount:" <> idtxt

mkSearchCounterKey :: Text -> Text -> Text
mkSearchCounterKey ind idtxt = "Customer:SearchCounter:" <> idtxt <> ":" <> ind

updateSearchFraudCounters :: (HasCacheConfig r, HedisFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateSearchFraudCounters riderId merchantConfigs = Redis.withCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.simulatedSearchCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkSearchCounterKey ind riderId.getId)

updateCancelledByDriverFraudCounters :: (HasCacheConfig r, HedisFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateCancelledByDriverFraudCounters riderId merchantConfigs = Redis.withCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.simulatedBookingCancelledByDriverCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkCancellationByDriverKey ind riderId.getId)

updateCustomerFraudCounters :: (HasCacheConfig r, HedisFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateCustomerFraudCounters riderId merchantConfigs = Redis.withCrossAppRedis $ do
  mapM_ (\mc -> incrementCount mc.id.getId mc.simulatedBookingCancellationCountWindow) merchantConfigs
  where
    incrementCount ind = SWC.incrementWindowCount (mkCancellationKey ind riderId.getId)

updateTotalRidesCounters :: (HedisFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> m ()
updateTotalRidesCounters riderId = Redis.withCrossAppRedis $ do
  _ <- getTotalRidesCount riderId
  let key = mkTotalRidesKey riderId.getId
  void $ Redis.incr key

getTotalRidesCount :: (HedisFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> m Int
getTotalRidesCount riderId = Redis.withCrossAppRedis $ do
  let key = mkTotalRidesKey riderId.getId
  mbTotalCount <- Redis.safeGet key
  case mbTotalCount of
    Just totalCount -> pure totalCount
    Nothing -> do
      totalCount <- runInReplica $ QB.findCountByRideIdAndStatus riderId BT.COMPLETED
      Redis.setExp key totalCount 14400
      pure totalCount

searchFraudDetected :: (HedisFlow m r, HasCacheConfig r, MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> Id DM.Merchant -> [DMC.MerchantConfig] -> m (Maybe DMC.MerchantConfig)
searchFraudDetected riderId merchantId = checkFraudDetected riderId merchantId [MoreSearching]

anyFraudDetected :: (HedisFlow m r, HasCacheConfig r, MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> Id DM.Merchant -> [DMC.MerchantConfig] -> m (Maybe DMC.MerchantConfig)
anyFraudDetected riderId merchantId = checkFraudDetected riderId merchantId [MoreCancelling, MoreCancelledByDriver, MoreSearching, TotalRides]

checkFraudDetected :: (HedisFlow m r, HasCacheConfig r, MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> Id DM.Merchant -> [Factors] -> [DMC.MerchantConfig] -> m (Maybe DMC.MerchantConfig)
checkFraudDetected riderId merchantId factors merchantConfigs = Redis.withCrossAppRedis $ do
  useFraudDetection <- maybe False (.useFraudDetection) <$> CMSUC.findByMerchantId merchantId
  if useFraudDetection
    then findM (\mc -> and <$> mapM (getFactorResult mc) factors) merchantConfigs
    else pure Nothing
  where
    getFactorResult mc factor =
      case factor of
        MoreCancelling -> do
          cancelledBookingCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkCancellationKey mc.id.getId riderId.getId) mc.simulatedBookingCancellationCountWindow
          pure $ cancelledBookingCount >= mc.simulatedBookingCancellationCountThreshold
        MoreCancelledByDriver -> do
          cancelledBookingByDriverCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkCancellationByDriverKey mc.id.getId riderId.getId) mc.simulatedBookingCancelledByDriverCountWindow
          pure $ cancelledBookingByDriverCount >= mc.simulatedBookingCancelledByDriverCountThreshold
        MoreSearching -> do
          serachCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkSearchCounterKey mc.id.getId riderId.getId) mc.simulatedSearchCountWindow
          pure $ serachCount >= mc.simulatedSearchCountThreshold
        TotalRides -> do
          totalRidesCount :: Int <- getTotalRidesCount riderId
          pure $ totalRidesCount <= mc.simulatedBookingTotalCountThreshold

addBookingInfoCache :: (HedisFlow m r, HasCacheConfig r, MonadFlow m, EsqDBReplicaFlow m r) => Id Person.Person -> m ()
addBookingInfoCache riderId = do
  now <- getCurrentTime
  allBookingLocationIds <- runInReplica $ QB.findAllBookingLocationIdsByRiderId riderId (Just 20) (Just 0)
  let locationIds = foldl' (\acc (fromLoc, mbToLoc) -> maybe (fromLoc : acc) (\toLoc -> toLoc : fromLoc : acc) mbToLoc) [] allBookingLocationIds
  allBookingLocations <- runInReplica $ QBL.findAllByIds locationIds
  mapM_ (\bl -> Redis.hSetExp bookingInfoStoreKey (getAddrStr bl.address) (BookingInfoStore bl.address.placeId (getAddrDscp bl.address) now) 1209600) allBookingLocations -- 14 days
  where
    getAddrStr LocationAddress {..} = T.intercalate ", " $ catMaybes [door, building, street, city, state, country, areaCode]
    getAddrDscp LocationAddress {..} = fromMaybe "" $ getFirst (First door <> First building <> First street <> First city <> First state <> First country <> First areaCode)

takeAction :: (HedisFlow m r, HasCacheConfig r, MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> Maybe (Id DMC.MerchantConfig) -> Bool -> m ()
takeAction riderId mcId shouldSimulate = do
  regTokens <- RT.findAllByPersonId riderId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key
  runNoTransaction $ do
    RT.deleteByPersonId riderId
    if shouldSimulate
      then QP.updateSoftAction riderId mcId True
      else QP.updateHardAction riderId mcId True
  addBookingInfoCache riderId
