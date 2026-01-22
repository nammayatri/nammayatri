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
import qualified Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.RegistrationToken as QR
import qualified Tools.Error

type AuthFlow m r =
  ( Storage.Beam.BeamFlow.BeamFlow m r,
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

  mbTuple <- getKeyRedis key
  (personId, merchantId, city) <- case mbTuple of
    Just (personId, merchantId, city) -> do
      checkTokenActivityInRedis token currentTime
      return (personId, merchantId, city)
    Nothing -> do
      mbTupleOld <- getKeyRedisOld key
      case mbTupleOld of
        Just (personId, merchantId) -> do
          checkTokenActivityInRedis token currentTime
          city <- getCity merchantId
          return (personId, merchantId, city)
        Nothing -> do
          sr <-
            catchAny
              (verifyToken token)
              ( \e -> do
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

checkTokenActivityInRedis ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  UTCTime ->
  m ()
checkTokenActivityInRedis token currentTime = do
  mbInactivityTimeout <- asks (.registrationTokenInactivityTimeout)
  whenJust mbInactivityTimeout $ \inactivityTimeout -> do
    key <- tokenActivityCacheKey token
    mbLastActivity <- Redis.get key

    whenJust mbLastActivity $ \lastActivity -> do
      let timeDiff = diffUTCTime currentTime lastActivity
      when (timeDiff > fromIntegral (getSeconds inactivityTimeout)) $ do
        mbToken <- QR.findByToken token
        whenJust mbToken $ \regToken ->
          QR.deleteById regToken.id

        Redis.del key
        authKey <- authTokenCacheKey token
        Redis.del authKey
        Utils.throwError TokenExpired

    updateTokenActivityInRedis token currentTime

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
  ( Storage.Beam.BeamFlow.BeamFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  RegToken ->
  m DR.RegistrationToken
verifyToken regToken = do
  QR.findByToken regToken
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateToken ::
  ( Storage.Beam.BeamFlow.BeamFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  DR.RegistrationToken ->
  m DR.RegistrationToken
validateToken sr = do
  unless (sr.enabled) $ Utils.throwError Tools.Error.UserDisabled
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
  ( Storage.Beam.BeamFlow.BeamFlow m r,
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
  ( Storage.Beam.BeamFlow.BeamFlow m r,
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
  ( Storage.Beam.BeamFlow.BeamFlow m r,
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
  ( Storage.Beam.BeamFlow.BeamFlow m r,
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
  ( Storage.Beam.BeamFlow.BeamFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  DP.Person ->
  m ()
checkPasswordExpiry person = do
  now <- getCurrentTime
  passwordExpiryDays <- asks (.passwordExpiryDays)
  whenJust passwordExpiryDays $ \days -> do
    let passwordUpdatedAt = person.passwordUpdatedAt
        secondsSinceUpdate = diffUTCTime now passwordUpdatedAt
        expiryLimit = fromIntegral days * 86400
    when (secondsSinceUpdate > expiryLimit) $
      throwError $ InvalidRequest "Your password has expired. Please reset or contact admin."
