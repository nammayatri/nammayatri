{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DriverInformation where

import Domain.Types.DriverInformation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.DriverInformation as Queries

create :: DriverInformation -> Esq.SqlDB ()
create = Queries.create

fetchAllByIds :: Esq.Transactionable m => Id Merchant -> [Id Driver] -> m [DriverInformation]
fetchAllByIds = Queries.fetchAllByIds

fetchAllAvailableByIds :: Esq.Transactionable m => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds = Queries.fetchAllAvailableByIds

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeDriverInformationIdKey id) >>= \case
    Just a -> pure $ Just a
    Nothing -> flip whenJust (cacheDriverInformation id) /=<< Queries.findById id

updateActivity :: CacheFlow m r => Finalize m -> Id Person.Driver -> Bool -> Esq.SqlDB ()
updateActivity finalize driverId isActive = do
  Queries.updateActivity driverId isActive
  finalize $ clearDriverInfoCache driverId

updateEnabledState :: CacheFlow m r => Finalize m -> Id Person.Driver -> Bool -> Esq.SqlDB ()
updateEnabledState finalize driverId isEnabled = do
  Queries.updateEnabledState driverId isEnabled
  finalize $ clearDriverInfoCache driverId

updateBlockedState :: CacheFlow m r => Finalize m -> Id Person.Driver -> Bool -> Esq.SqlDB ()
updateBlockedState finalize driverId isBlocked = do
  Queries.updateBlockedState driverId isBlocked
  finalize $ clearDriverInfoCache driverId

updateRental :: CacheFlow m r => Finalize m -> Id Driver -> Bool -> Esq.SqlDB ()
updateRental finalize driverId isRental = do
  Queries.updateRental driverId isRental
  finalize $ clearDriverInfoCache driverId

updateOnRide :: CacheFlow m r => Finalize m -> Id Person.Driver -> Bool -> Esq.SqlDB ()
updateOnRide finalize driverId onRide = do
  Queries.updateOnRide driverId onRide
  finalize $ clearDriverInfoCache driverId

-- this function created because all queries wishfully should be in one transaction
updateNotOnRideMultiple :: CacheFlow m r => Finalize m -> [Id Person.Driver] -> Esq.SqlDB ()
updateNotOnRideMultiple finalize driverIds = do
  Queries.updateNotOnRideMultiple driverIds
  finalize $ forM_ driverIds clearDriverInfoCache

updateDowngradingOptions :: CacheFlow m r => Finalize m -> Id Driver -> Bool -> Bool -> Esq.SqlDB ()
updateDowngradingOptions finalize driverId canDowngradeToSedan canDowngradeToHatchback = do
  Queries.updateDowngradingOptions driverId canDowngradeToSedan canDowngradeToHatchback
  finalize $ clearDriverInfoCache driverId

deleteById :: CacheFlow m r => Finalize m -> Id Person.Driver -> Esq.SqlDB ()
deleteById finalize driverId = do
  Queries.deleteById driverId
  finalize $ clearDriverInfoCache driverId

findAllWithLimitOffsetByMerchantId ::
  Esq.Transactionable m =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId = Queries.findAllWithLimitOffsetByMerchantId

getDriversWithOutdatedLocationsToMakeInactive :: Esq.Transactionable m => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive = Queries.getDriversWithOutdatedLocationsToMakeInactive

countDrivers :: Esq.Transactionable m => Id Merchant -> m (Int, Int)
countDrivers = Queries.countDrivers

--------- Caching logic -------------------

clearDriverInfoCache :: CacheFlow m r => Id Person.Driver -> m ()
clearDriverInfoCache = Hedis.withCrossAppRedis . Hedis.del . makeDriverInformationIdKey

cacheDriverInformation :: CacheFlow m r => Id Person.Driver -> DriverInformation -> m ()
cacheDriverInformation driverId driverInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeDriverInformationIdKey driverId) driverInfo expTime

makeDriverInformationIdKey :: Id Person.Driver -> Text
makeDriverInformationIdKey id = "static-offer-driver-app:CachedQueries:DriverInformation:DriverId-" <> id.getId
