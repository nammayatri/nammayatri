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
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow, EsqLocRepDBFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Storage.Queries.DriverInformation as Queries

create :: DriverInformation -> Esq.SqlDB ()
create = Queries.create

findById :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeDriverInformationIdKey id) >>= \case
    Just a -> pure $ Just a
    Nothing -> flip whenJust (cacheDriverInformation id) /=<< Queries.findById id

updateActivity :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> Maybe DriverMode -> m ()
updateActivity driverId isActive mode = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateActivity driverId isActive mode

updateEnabledState :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateEnabledState driverId isEnabled = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateEnabledState driverId isEnabled

updateAadhaarVerifiedState :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateAadhaarVerifiedState driverId isVerified = do
  clearDriverInfoCache driverId
  Esq.runNoTransaction $ Queries.updateAadhaarVerifiedState driverId isVerified

updateEnabledVerifiedState :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> Bool -> m ()
updateEnabledVerifiedState driverId isEnabled isVerified = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateEnabledVerifiedState driverId isEnabled isVerified

updateBlockedState :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateBlockedState driverId isBlocked = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateBlockedState driverId isBlocked

verifyAndEnableDriver :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> m ()
verifyAndEnableDriver driverId = do
  clearDriverInfoCache (cast driverId)
  Esq.runTransaction $ Queries.verifyAndEnableDriver driverId

updateOnRide :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateOnRide driverId onRide = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateOnRide driverId onRide

updatePendingPayment :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Bool -> Id Person.Driver -> m ()
updatePendingPayment isPending driverId = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updatePendingPayment isPending driverId

updateSubscription :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Bool -> Id Person.Driver -> m ()
updateSubscription isSubscribed driverId = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateSubscription isSubscribed driverId

-- this function created because all queries wishfully should be in one transaction
updateNotOnRideMultiple :: [Id Person.Driver] -> Esq.SqlDB ()
updateNotOnRideMultiple = Queries.updateNotOnRideMultiple

deleteById :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m ()
deleteById driverId = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.deleteById driverId

findAllWithLimitOffsetByMerchantId ::
  (Esq.Transactionable m, EsqDBReplicaFlow m r) =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId = Queries.findAllWithLimitOffsetByMerchantId

getDriversWithOutdatedLocationsToMakeInactive :: (Esq.Transactionable m, EsqLocRepDBFlow m r) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive = Queries.getDriversWithOutdatedLocationsToMakeInactive

addReferralCode :: (Hedis.CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> EncryptedHashedField 'AsEncrypted Text -> m ()
addReferralCode personId code = do
  clearDriverInfoCache (cast personId)
  Esq.runTransaction $ Queries.addReferralCode personId code

countDrivers :: Esq.Transactionable m => Id Merchant -> m (Int, Int)
countDrivers = Queries.countDrivers

updateDowngradingOptions :: Id Person -> Bool -> Bool -> Bool -> Esq.SqlDB ()
updateDowngradingOptions = Queries.updateDowngradingOptions

--------- Caching logic -------------------

clearDriverInfoCache :: (Hedis.CacheFlow m r) => Id Person.Driver -> m ()
clearDriverInfoCache = Hedis.withCrossAppRedis . Hedis.del . makeDriverInformationIdKey

cacheDriverInformation :: (Hedis.CacheFlow m r) => Id Person.Driver -> DriverInformation -> m ()
cacheDriverInformation driverId driverInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeDriverInformationIdKey driverId) driverInfo expTime

makeDriverInformationIdKey :: Id Person.Driver -> Text
makeDriverInformationIdKey id = "driver-offer:CachedQueries:DriverInformation:DriverId-" <> id.getId
