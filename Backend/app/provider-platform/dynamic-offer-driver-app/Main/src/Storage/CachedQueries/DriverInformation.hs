{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DriverInformation where

import Domain.Types.DriverInformation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.DriverInformation as Queries
import qualified Storage.Queries.Person as QueriesPerson

create :: L.MonadFlow m => DriverInformation -> m ()
create = Queries.create

findById :: (CacheFlow m r, L.MonadFlow m) => Id Person.Driver -> m (Maybe DriverInformation)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeDriverInformationIdKey id) >>= \case
    Just a -> pure $ Just a
    Nothing -> flip whenJust (cacheDriverInformation id) /=<< Queries.findById id

updateActivity :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> Maybe DriverMode -> m (MeshResult ())
updateActivity driverId isActive mode = do
  clearDriverInfoCache driverId
  Queries.updateActivity driverId isActive mode

updateEnabledState :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> m (MeshResult ())
updateEnabledState driverId isEnabled = do
  clearDriverInfoCache driverId
  Queries.updateEnabledState driverId isEnabled

updateAadhaarVerifiedState :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> m ()
updateAadhaarVerifiedState driverId isVerified = do
  clearDriverInfoCache driverId
  -- Esq.runNoTransaction $ Queries.updateAadhaarVerifiedState driverId isVerified
  void $ Queries.updateAadhaarVerifiedState driverId isVerified

updateEnabledVerifiedState :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> Bool -> m (MeshResult ())
updateEnabledVerifiedState driverId isEnabled isVerified = do
  clearDriverInfoCache driverId
  Queries.updateEnabledVerifiedState driverId isEnabled isVerified

updateBlockedState :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> m (MeshResult ())
updateBlockedState driverId isBlocked = do
  clearDriverInfoCache driverId
  Queries.updateBlockedState driverId isBlocked

verifyAndEnableDriver :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person -> m (MeshResult ())
verifyAndEnableDriver driverId = do
  clearDriverInfoCache (cast driverId)
  Queries.verifyAndEnableDriver driverId

updateOnRide :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> m (MeshResult ())
updateOnRide driverId onRide = do
  clearDriverInfoCache driverId
  Queries.updateOnRide driverId onRide

updatePendingPayment :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Bool -> Id Person.Driver -> m ()
updatePendingPayment isPending driverId = do
  clearDriverInfoCache driverId
  -- Esq.runTransaction $ Queries.updatePendingPayment isPending driverId
  void $ Queries.updatePendingPayment isPending driverId

updateSubscription :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Bool -> Id Person.Driver -> m ()
updateSubscription isSubscribed driverId = do
  clearDriverInfoCache driverId
  -- Esq.runTransaction $ Queries.updateSubscription isSubscribed driverId
  void $ Queries.updateSubscription isSubscribed driverId

-- this function created because all queries wishfully should be in one transaction
updateNotOnRideMultiple :: (L.MonadFlow m, MonadTime m) => [Id Person.Driver] -> m (MeshResult ())
updateNotOnRideMultiple = Queries.updateNotOnRideMultiple

deleteById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m ()
deleteById driverId = do
  clearDriverInfoCache driverId
  Queries.deleteById driverId

findAllWithLimitOffsetByMerchantId ::
  (Esq.Transactionable m, Esq.EsqDBReplicaFlow m r) =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId = Queries.findAllWithLimitOffsetByMerchantId

-- Shifted this to person.hs as seen below because of cyclic dependency.
-- getDriversWithOutdatedLocationsToMakeInactive :: Esq.Transactionable m => UTCTime -> m [Person]
-- getDriversWithOutdatedLocationsToMakeInactive = Queries.getDriversWithOutdatedLocationsToMakeInactive

getDriversWithOutdatedLocationsToMakeInactive :: (L.MonadFlow m, Log m) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive = QueriesPerson.getDriversWithOutdatedLocationsToMakeInactive

addReferralCode :: (CacheFlow m r, L.MonadFlow m, MonadTime m) => Id Person -> EncryptedHashedField 'AsEncrypted Text -> m (MeshResult ())
addReferralCode personId code = do
  clearDriverInfoCache (cast personId)
  Queries.addReferralCode personId code

countDrivers :: L.MonadFlow m => Id Merchant -> m (Int, Int)
countDrivers = Queries.countDrivers

updateDowngradingOptions :: (L.MonadFlow m, MonadTime m) => Id Person -> Bool -> Bool -> Bool -> m (MeshResult ())
updateDowngradingOptions = Queries.updateDowngradingOptions

--------- Caching logic -------------------

clearDriverInfoCache :: (CacheFlow m r) => Id Person.Driver -> m ()
clearDriverInfoCache = Hedis.withCrossAppRedis . Hedis.del . makeDriverInformationIdKey

cacheDriverInformation :: (CacheFlow m r) => Id Person.Driver -> DriverInformation -> m ()
cacheDriverInformation driverId driverInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeDriverInformationIdKey driverId) driverInfo expTime

makeDriverInformationIdKey :: Id Person.Driver -> Text
makeDriverInformationIdKey id = "driver-offer:CachedQueries:DriverInformation:DriverId-" <> id.getId
