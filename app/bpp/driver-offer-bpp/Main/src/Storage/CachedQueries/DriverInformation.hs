{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DriverInformation where

import Beckn.External.Encryption
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Domain.Types.DriverInformation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.DriverInformation as Queries

create :: DriverInformation -> Esq.SqlDB ()
create = Queries.create

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findById id =
  Hedis.get (makeDriverInformationIdKey id) >>= \case
    Just a -> pure $ Just a
    Nothing -> flip whenJust (cacheDriverInformation id) /=<< Queries.findById id

updateActivity :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateActivity driverId isActive = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateActivity driverId isActive

updateEnabledState :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateEnabledState driverId isEnabled = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateEnabledState driverId isEnabled 

updateEnabledVerifiedState :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> Bool -> m ()
updateEnabledVerifiedState driverId isEnabled isVerified = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateEnabledVerifiedState driverId isEnabled isVerified

updateBlockedState :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateBlockedState driverId isBlocked = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateBlockedState driverId isBlocked

verifyAndEnableDriver :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> m ()
verifyAndEnableDriver driverId = do
  clearDriverInfoCache (cast driverId)
  Esq.runTransaction $ Queries.verifyAndEnableDriver driverId

updateOnRide :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> Bool -> m ()
updateOnRide driverId onRide = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.updateOnRide driverId onRide

deleteById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m ()
deleteById driverId = do
  clearDriverInfoCache driverId
  Esq.runTransaction $ Queries.deleteById driverId

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

addReferralCode :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> EncryptedHashedField 'AsEncrypted Text -> m ()
addReferralCode personId code = do
  clearDriverInfoCache (cast personId)
  Esq.runTransaction $ Queries.addReferralCode personId code

countDrivers :: Esq.Transactionable m => Id Merchant -> m (Int, Int)
countDrivers = Queries.countDrivers

--------- Caching logic -------------------

clearDriverInfoCache :: (CacheFlow m r) => Id Person.Driver -> m ()
clearDriverInfoCache = Hedis.del . makeDriverInformationIdKey

cacheDriverInformation :: (CacheFlow m r) => Id Person.Driver -> DriverInformation -> m ()
cacheDriverInformation driverId driverInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeDriverInformationIdKey driverId) driverInfo expTime

makeDriverInformationIdKey :: Id Person.Driver -> Text
makeDriverInformationIdKey id = "CachedQueries:DriverInformation:DriverId-" <> id.getId
