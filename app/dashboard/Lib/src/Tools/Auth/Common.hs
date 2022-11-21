{-# LANGUAGE AllowAmbiguousTypes #-}

module Tools.Auth.Common (verifyPerson, cleanCachedTokens, cleanCachedTokensByMerchantId, AuthFlow) where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Beckn.Utils.Common as Utils
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.RegistrationToken as QR

type AuthFlow m r =
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  )

verifyPerson ::
  (AuthFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r) =>
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
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  RegToken ->
  m DR.RegistrationToken
verifyToken regToken = do
  QR.findByToken regToken
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateToken ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
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
      QR.deleteById sr.id
    Utils.throwError TokenExpired
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantId sr.personId sr.merchantId
  when (isNothing mbMerchantAccess) $ do
    Esq.runTransaction $
      QR.deleteById sr.id
    Utils.throwError AccessDenied
  return sr

cleanCachedTokens ::
  ( EsqDBReplicaFlow m r,
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
  ( EsqDBReplicaFlow m r,
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
