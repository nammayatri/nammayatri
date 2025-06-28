{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.Common (verifyPerson, cleanCachedTokens, cleanCachedTokensByMerchantId, cleanCachedTokensByMerchantIdAndCity, cleanCachedTokensOfMerchantAndCity, AuthFlow, authTokenCacheKey, tokenActivityCacheKey) where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.App
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Common as Utils
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Error

type AuthFlow m r =
  ( BeamFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days, "registrationTokenInactivityTimeout" ::: Seconds],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  )

verifyPerson ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  m (Id DP.Person, Id DMerchant.Merchant, City.City)
verifyPerson token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)

  -- First check if token exists in cache
  mbTuple <- getKeyRedis key
  (personId, merchantId, city) <- case mbTuple of
    Just (personId, merchantId, city) -> do
      -- For cached tokens, check activity in Redis
      now <- getCurrentTime
      checkTokenActivityInRedis token now
      return (personId, merchantId, city)
    Nothing -> do
      mbTupleOld <- getKeyRedisOld key
      case mbTupleOld of
        Just (personId, merchantId) -> do
          -- For old cache format, check activity in Redis
          now <- getCurrentTime
          checkTokenActivityInRedis token now
          city <- getCity merchantId
          return (personId, merchantId, city)
        Nothing -> do
          -- For tokens not in cache, verify from DB
          sr <- verifyToken token
          let personId = sr.personId
          let merchantId = sr.merchantId
          let city = sr.operatingCity
          now <- getCurrentTime
          checkTokenActivityInRedis token now
          -- Add token back to Redis with fresh expiry
          setExRedis key (personId, merchantId, city) authTokenCacheExpiry

          return (personId, merchantId, city)

  return (personId, merchantId, city)
  where
    getKeyRedisOld :: Redis.HedisFlow m r => Text -> m (Maybe (Id DP.Person, Id DMerchant.Merchant))
    getKeyRedisOld = Redis.get

    getKeyRedis :: Redis.HedisFlow m r => Text -> m (Maybe (Id DP.Person, Id DMerchant.Merchant, City.City))
    getKeyRedis = Redis.get

    setExRedis :: Redis.HedisFlow m r => Text -> (Id DP.Person, Id DMerchant.Merchant, City.City) -> Int -> m ()
    setExRedis = Redis.setExp

    getCity :: AuthFlow m r => Id DMerchant.Merchant -> m City.City
    getCity merchantId' = QMerchant.findById merchantId' >>= fmap (.defaultOperatingCity) . fromMaybeM (MerchantNotFound merchantId'.getId)

authTokenCacheKey ::
  HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text] =>
  RegToken ->
  m Text
authTokenCacheKey regToken = do
  authTokenCacheKeyPrefix <- asks (.authTokenCacheKeyPrefix)
  pure $ authTokenCacheKeyPrefix <> regToken

tokenActivityCacheKey ::
  HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text] =>
  RegToken ->
  m Text
tokenActivityCacheKey regToken = do
  authTokenCacheKeyPrefix <- asks (.authTokenCacheKeyPrefix)
  pure $ authTokenCacheKeyPrefix <> "activity:" <> regToken

-- Check if token has been inactive for too long
checkTokenActivityInRedis ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  UTCTime ->
  m ()
checkTokenActivityInRedis token now = do
  key <- tokenActivityCacheKey token
  mbLastActivity <- Redis.get key
  case mbLastActivity of
    Nothing -> do
      -- If no activity record exists in Redis, this is the first API call
      -- Create an activity record with current timestamp
      updateTokenActivityInRedis token now
    Just lastActivity -> do
      -- Check inactivity timeout with Redis data
      inactivityTimeout <- getSeconds <$> asks (.registrationTokenInactivityTimeout)
      let inactivityTimeoutNominal = (fromIntegral inactivityTimeout :: Double)
      let timeDiff = diffUTCTime now lastActivity
      when (realToFrac timeDiff > (inactivityTimeoutNominal :: Double)) $ do
        -- When token is expired due to inactivity, delete it from database, so that a new token will be generated on next login
        mbToken <- QR.findByToken token
        case mbToken of
          Just regToken -> QR.deleteById regToken.id
          Nothing -> pure ()
        -- Also delete from Redis cache
        void $ Redis.del key
        -- Throw token expired error
        Utils.throwError TokenExpired

      -- Update activity timestamp
      updateTokenActivityInRedis token now

-- Update token activity timestamp in Redis
updateTokenActivityInRedis ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  UTCTime ->
  m ()
updateTokenActivityInRedis token now = do
  key <- tokenActivityCacheKey token
  Redis.set key now

verifyToken ::
  ( BeamFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  RegToken ->
  m DR.RegistrationToken
verifyToken regToken = do
  QR.findByToken regToken
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateToken ::
  ( BeamFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  DR.RegistrationToken ->
  m DR.RegistrationToken
validateToken sr = do
  unless (sr.enabled) $ Utils.throwError UserDisabled
  registrationTokenExpiry <- asks (.registrationTokenExpiry)
  let nominal = realToFrac . daysToSeconds $ registrationTokenExpiry
  expired <- Utils.isExpired nominal sr.createdAt
  when expired $ do
    QR.deleteById sr.id
    Utils.throwError TokenExpired
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity sr.personId sr.merchantId sr.operatingCity
  when (isNothing mbMerchantAccess) $ do
    QR.deleteById sr.id
    Utils.throwError AccessDenied
  return sr

cleanCachedTokens ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  m ()
cleanCachedTokens personId = do
  regTokens <- QR.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    key <- authTokenCacheKey regToken.token
    void $ Redis.del key
    activityKey <- tokenActivityCacheKey regToken.token
    void $ Redis.del activityKey

cleanCachedTokensByMerchantId ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  m ()
cleanCachedTokensByMerchantId personId merchantId = do
  regTokens <- QR.findAllByPersonIdAndMerchantId personId merchantId
  for_ regTokens $ \regToken -> do
    key <- authTokenCacheKey regToken.token
    void $ Redis.del key
    activityKey <- tokenActivityCacheKey regToken.token
    void $ Redis.del activityKey

cleanCachedTokensOfMerchantAndCity ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DMerchant.Merchant ->
  City.City ->
  m ()
cleanCachedTokensOfMerchantAndCity merchantId city = do
  regTokens <- QR.findAllByMerchantIdAndCity merchantId city
  for_ regTokens $ \regToken -> do
    key <- authTokenCacheKey regToken.token
    void $ Redis.del key
    activityKey <- tokenActivityCacheKey regToken.token
    void $ Redis.del activityKey

cleanCachedTokensByMerchantIdAndCity ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  City.City ->
  m ()
cleanCachedTokensByMerchantIdAndCity personId merchantId city = do
  regTokens <- QR.findAllByPersonIdAndMerchantIdAndCity personId merchantId city
  for_ regTokens $ \regToken -> do
    key <- authTokenCacheKey regToken.token
    void $ Redis.del key
    activityKey <- tokenActivityCacheKey regToken.token
    void $ Redis.del activityKey
