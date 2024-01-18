{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.Common (verifyPerson, cleanCachedTokens, cleanCachedTokensByMerchantId, cleanCachedTokensByMerchantIdAndCity, AuthFlow) where

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

type AuthFlow m r =
  ( BeamFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  )

verifyPerson ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  m (Id DP.Person, Id DMerchant.Merchant, City.City)
verifyPerson token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbTuple <- getKeyRedis key
  (personId, merchantId, city) <- case mbTuple of
    Just (personId, merchantId, city) -> return (personId, merchantId, city)
    Nothing -> do
      mbTupleOld <- getKeyRedisOld key
      case mbTupleOld of
        Just (personId, merchantId) -> do
          city <- getCity merchantId
          return (personId, merchantId, city)
        Nothing -> do
          sr <- verifyToken token
          let personId = sr.personId
          let merchantId = sr.merchantId
          let city = sr.operatingCity
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
  registrationTokenExpiry <- asks (.registrationTokenExpiry)
  let nominal = realToFrac . daysToSeconds $ registrationTokenExpiry
  expired <- Utils.isExpired nominal sr.createdAt
  when expired $ do
    -- Esq.runTransaction $
    QR.deleteById sr.id
    Utils.throwError TokenExpired
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity sr.personId sr.merchantId sr.operatingCity
  when (isNothing mbMerchantAccess) $ do
    -- Esq.runTransaction $
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
