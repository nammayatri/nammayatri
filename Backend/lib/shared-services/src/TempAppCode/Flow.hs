{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TempAppCode.Flow
  ( generateTempAppCode,
    redeemTempAppCode,
    mkCodeKey,
  )
where

import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import TempAppCode.Types

mkCodeKey :: TempAppCodeCfg -> Text -> Text
mkCodeKey cfg code = cfg.keyPrefix <> code

mkLimitKey :: TempAppCodeCfg -> Text -> Text
mkLimitKey cfg code = cfg.limitKeyPrefix <> code

mkGenerateLimitKey :: TempAppCodeCfg -> Text -> Text
mkGenerateLimitKey cfg personId = cfg.limitKeyPrefix <> "gen:" <> personId

mkGlobalFailureKey :: TempAppCodeCfg -> Text
mkGlobalFailureKey cfg = cfg.limitKeyPrefix <> "global-failures"

nextCode :: (MonadFlow m, Redis.HedisFlow m r) => TempAppCodeCfg -> m Text
nextCode cfg = case cfg.codeStrategy of
  Guid -> generateGUID
  NumericCounter modulus -> show . (`mod` modulus) <$> Redis.incr cfg.counterKey
  RandomCode len -> liftIO $ randomCode len

-- | Crockford base32: no I, L, O or U, so a code is easy to read back and type. 32 symbols divide
-- 256 evenly, so mapping each random byte onto the alphabet adds no bias.
randomCode :: Int -> IO Text
randomCode len = do
  bytes :: BS.ByteString <- getRandomBytes len
  pure $ T.pack [T.index alphabet (fromIntegral b `mod` 32) | b <- BS.unpack bytes]
  where
    alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

generateTempAppCode ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  TempAppCodeCfg ->
  Text ->
  m TempAppCodeRes
generateTempAppCode cfg personId = do
  let limitKey = mkGenerateLimitKey cfg personId
  attempts <- Redis.incr limitKey
  when (attempts > toInteger cfg.maxAttempts) $ throwError $ InvalidRequest "Too many attempts"
  Redis.expire limitKey cfg.attemptWindowSeconds
  code <- nextCode cfg
  Redis.setExp (mkCodeKey cfg code) (TempAppCodeValue {personId = personId, consumeOnRead = cfg.consumeOnRead}) cfg.ttlSeconds
  now <- getCurrentTime
  pure $ TempAppCodeRes {code = code, expiresAt = addUTCTime (fromIntegral cfg.ttlSeconds) now}

redeemTempAppCode ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  TempAppCodeCfg ->
  Text ->
  m (Maybe Text)
redeemTempAppCode cfg code = do
  let globalFailureKey = mkGlobalFailureKey cfg
  globalFailures :: Maybe Integer <- Redis.get globalFailureKey
  when (fromMaybe 0 globalFailures >= toInteger cfg.globalFailureLimit) $ throwError $ InvalidRequest "Too many attempts"
  let limitKey = mkLimitKey cfg code
  attempts <- Redis.incr limitKey
  when (attempts > toInteger cfg.maxAttempts) $ throwError $ InvalidRequest "Too many attempts"
  Redis.expire limitKey cfg.attemptWindowSeconds
  let codeKey = mkCodeKey cfg code
  mbValue :: Maybe TempAppCodeValue <- Redis.safeGet codeKey
  mbPersonId <- case mbValue of
    Nothing -> pure Nothing
    Just value
      | not value.consumeOnRead -> pure (Just value.personId)
      | otherwise -> do
        let claimKey = codeKey <> ":claimed"
        claim <- Redis.incr claimKey
        Redis.expire claimKey cfg.ttlSeconds
        if claim == 1
          then do
            Redis.del codeKey
            pure (Just value.personId)
          else pure Nothing
  when (isNothing mbPersonId) $ do
    failures <- Redis.incr globalFailureKey
    -- Expiry is set only when the window opens; resetting it on every failure would let a steady
    -- trickle of genuine mistakes keep the counter alive until it locks everyone out.
    when (failures == 1) $ Redis.expire globalFailureKey cfg.globalFailureWindowSeconds
  pure mbPersonId
