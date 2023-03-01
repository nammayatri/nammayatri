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
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.DriverInformation as Queries

create :: DriverInformation -> Esq.SqlDB m ()
create = Queries.create

findById :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeDriverInformationIdKey id) >>= \case
    Just a -> pure $ Just a
    Nothing -> flip whenJust (cacheDriverInformation id) /=<< Queries.findById (Proxy @m) id

updateActivity :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateActivity driverId isActive = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateActivity @m driverId isActive

updateEnabledState :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateEnabledState driverId isEnabled = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateEnabledState @m driverId isEnabled

updateEnabledVerifiedState :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> Bool -> m ()
updateEnabledVerifiedState driverId isEnabled isVerified = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateEnabledVerifiedState @m driverId isEnabled isVerified

updateBlockedState :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateBlockedState driverId isBlocked = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateBlockedState @m driverId isBlocked

verifyAndEnableDriver :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> m ()
verifyAndEnableDriver driverId = do
  clearDriverInfoCache (cast driverId)
  Esq.runTransaction $ Queries.verifyAndEnableDriver @m driverId

updateOnRide :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateOnRide driverId onRide = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateOnRide @m driverId onRide

-- this function created because all queries wishfully should be in one transaction
updateNotOnRideMultiple :: [Id Person.Driver] -> Esq.SqlDB m ()
updateNotOnRideMultiple = Queries.updateNotOnRideMultiple

deleteById :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m ()
deleteById driverId = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.deleteById @m driverId

findAllWithLimitOffsetByMerchantId ::
  Esq.Transactionable ma m =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  Proxy ma ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId = Queries.findAllWithLimitOffsetByMerchantId

getDriversWithOutdatedLocationsToMakeInactive :: Esq.Transactionable ma m => UTCTime -> Proxy ma -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive = Queries.getDriversWithOutdatedLocationsToMakeInactive

addReferralCode :: forall m r. (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> EncryptedHashedField 'AsEncrypted Text -> m ()
addReferralCode personId code = do
  clearDriverInfoCache (cast personId)
  Esq.runTransaction $ Queries.addReferralCode @m personId code

countDrivers :: Esq.Transactionable ma m => Id Merchant -> Proxy ma -> m (Int, Int)
countDrivers = Queries.countDrivers

--------- Caching logic -------------------

clearDriverInfoCache :: (CacheFlow m r) => Id Person.Driver -> m ()
clearDriverInfoCache = Hedis.withCrossAppRedis . Hedis.del . makeDriverInformationIdKey

cacheDriverInformation :: (CacheFlow m r) => Id Person.Driver -> DriverInformation -> m ()
cacheDriverInformation driverId driverInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeDriverInformationIdKey driverId) driverInfo expTime

makeDriverInformationIdKey :: Id Person.Driver -> Text
makeDriverInformationIdKey id = "driver-offer:CachedQueries:DriverInformation:DriverId-" <> id.getId
