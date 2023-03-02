{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Tools.Auth.Common (verifyPerson, cleanCachedTokens, cleanCachedTokensByMerchantId, AuthFlow) where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Common as Utils
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.RegistrationToken as QR

type AuthFlow m r =
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  )

verifyPerson ::
  (AuthFlow m r, Redis.HedisFlow m r) =>
  RegToken ->
  m (Id DP.Person, Id DMerchant.Merchant)
verifyPerson token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbTuple <- getKeyRedis key
  (personId, merchantId) <- case mbTuple of
    Just (personId, merchantId) -> return (personId, merchantId)
    Nothing -> do
      sr <- verifyToken token
      let personId = sr.personId
      let merchantId = sr.merchantId
      setExRedis key (personId, merchantId) authTokenCacheExpiry
      return (personId, merchantId)
  return (personId, merchantId)
  where
    getKeyRedis :: Redis.HedisFlow m r => Text -> m (Maybe (Id DP.Person, Id DMerchant.Merchant))
    getKeyRedis = Redis.get

    setExRedis :: Redis.HedisFlow m r => Text -> (Id DP.Person, Id DMerchant.Merchant) -> Int -> m ()
    setExRedis = Redis.setExp

authTokenCacheKey ::
  HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text] =>
  RegToken ->
  m Text
authTokenCacheKey regToken = do
  authTokenCacheKeyPrefix <- asks (.authTokenCacheKeyPrefix)
  pure $ authTokenCacheKeyPrefix <> regToken

verifyToken ::
  forall m r.
  ( EsqDBFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  RegToken ->
  m DR.RegistrationToken
verifyToken regToken = do
  QR.findByToken regToken (Proxy @m)
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateToken ::
  forall m r.
  ( EsqDBFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  DR.RegistrationToken ->
  m DR.RegistrationToken
validateToken sr = do
  registrationTokenExpiry <- asks (.registrationTokenExpiry)
  let nominal = realToFrac . daysToSeconds $ registrationTokenExpiry
  expired <- Utils.isExpired nominal sr.createdAt
  when expired $ do
    Esq.runTransaction $
      QR.deleteById @m sr.id
    Utils.throwError TokenExpired
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantId sr.personId sr.merchantId (Proxy @m)
  when (isNothing mbMerchantAccess) $ do
    Esq.runTransaction $
      QR.deleteById @m sr.id
    Utils.throwError AccessDenied
  return sr

cleanCachedTokens ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  m ()
cleanCachedTokens personId = do
  regTokens <- QR.findAllByPersonId personId (Proxy @m)
  for_ regTokens $ \regToken -> do
    key <- authTokenCacheKey regToken.token
    void $ Redis.del key

cleanCachedTokensByMerchantId ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  m ()
cleanCachedTokensByMerchantId personId merchantId = do
  regTokens <- QR.findAllByPersonIdAndMerchantId personId merchantId (Proxy @m)
  for_ regTokens $ \regToken -> do
    key <- authTokenCacheKey regToken.token
    void $ Redis.del key
