{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.Common (verifyPerson, cleanCachedTokens, cleanCachedTokensByMerchantId, cleanCachedTokensByMerchantIdAndCity, cleanCachedTokensOfMerchantAndCity, AuthFlow, authTokenCacheKey, tokenActivityCacheKey, checkPasswordExpiry) where

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
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Error

type AuthFlow m r =
  ( BeamFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days, "registrationTokenInactivityTimeout" ::: Maybe Seconds],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  )

verifyPerson ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  m (Id DP.Person, Id DMerchant.Merchant, City.City)
verifyPerson token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  currentTime <- getCurrentTime

  -- First check if token exists in cache
  mbTuple <- getKeyRedis key
  (personId, merchantId, city) <- case mbTuple of
    Just (personId, merchantId, city) -> do
      -- For cached tokens, check activity in Redis
      checkTokenActivityInRedis token currentTime
      return (personId, merchantId, city)
    Nothing -> do
      mbTupleOld <- getKeyRedisOld key
      case mbTupleOld of
        Just (personId, merchantId) -> do
          -- For old cache format, check activity in Redis
          checkTokenActivityInRedis token currentTime
          city <- getCity merchantId
          return (personId, merchantId, city)
        Nothing -> do
          -- For tokens not in cache, verify from DB
          -- Use catchAny to handle errors and clean up activity keys
          sr <-
            catchAny
              (verifyToken token)
              ( \e -> do
                  -- Clean up any activity keys if verification fails
                  mbInactivityTimeout <- asks (.registrationTokenInactivityTimeout)
                  whenJust mbInactivityTimeout $ \_ -> do
                    activityKey <- tokenActivityCacheKey token
                    Redis.del activityKey
                  throwM e
              )
          let personId = sr.personId
          let merchantId = sr.merchantId
          let city = sr.operatingCity
          checkTokenActivityInRedis token currentTime
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
checkTokenActivityInRedis token currentTime = do
  mbInactivityTimeout <- asks (.registrationTokenInactivityTimeout)
  -- Skip activity tracking completely when timeout is not configured
  whenJust mbInactivityTimeout $ \inactivityTimeout -> do
    key <- tokenActivityCacheKey token
    mbLastActivity <- Redis.get key

    -- Check inactivity timeout with Redis data if we have previous activity
    whenJust mbLastActivity $ \lastActivity -> do
      let timeDiff = diffUTCTime currentTime lastActivity
      when (timeDiff > fromIntegral (getSeconds inactivityTimeout)) $ do
        -- When token is expired due to inactivity, delete it from database first
        mbToken <- QR.findByToken token
        whenJust mbToken $ \regToken ->
          QR.deleteById regToken.id

        -- Clean up Redis keys (common for both cases)
        Redis.del key
        authKey <- authTokenCacheKey token
        Redis.del authKey
        -- Throw token expired error
        Utils.throwError TokenExpired

    -- Update activity timestamp (happens for both new and existing activities)
    updateTokenActivityInRedis token currentTime

-- Update token activity timestamp in Redis
updateTokenActivityInRedis ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  UTCTime ->
  m ()
updateTokenActivityInRedis token currentTime = do
  key <- tokenActivityCacheKey token
  -- Set activity with a TTL slightly longer than token expiry to ensure cleanup
  registrationTokenExpiry <- asks (.registrationTokenExpiry)
  let ttl = daysToSeconds registrationTokenExpiry + 86400 -- Add 1 day buffer
  Redis.setExp key currentTime (fromIntegral ttl)

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

checkPasswordExpiry ::
  ( BeamFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  DP.Person ->
  m ()
checkPasswordExpiry person = do
  when (isJust person.passwordHash) $ do
    now <- getCurrentTime
    passwordExpiryDays <- asks (.passwordExpiryDays)
    whenJust passwordExpiryDays $ \days -> do
      when (isNothing person.passwordUpdatedAt) $
        QPerson.updatePersonPasswordUpdatedAt person.id
      let passwordUpdatedAt = fromMaybe now person.passwordUpdatedAt
          secondsSinceUpdate = diffUTCTime now passwordUpdatedAt
          expiryLimit = fromIntegral days * 86400
      when (secondsSinceUpdate > expiryLimit) $
        throwError $ InvalidRequest "Your password has expired. Please reset or contact admin."
