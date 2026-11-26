{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Challenge
  ( Challenge (..),
    ChallengeUse (..),
    challengeTtlSeconds,
    issueChallenge,
    consumeChallenge,
  )
where

import AppAttestation.Canonical (encodeBase64Url)
import Crypto.Random (getRandomBytes)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CloudTag (cloudCodeOfId)
import Kernel.Types.Version (cloudTypeFromCode, localCloudType)
import Kernel.Utils.Common

data Challenge = Challenge
  { challengeId :: Text,
    challenge :: Text,
    expiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

challengeKey :: Text -> Text
challengeKey challengeId = "AppAttestation:Challenge:" <> challengeId

consumedKey :: Text -> Text
consumedKey challengeId = "AppAttestation:ChallengeConsumed:" <> challengeId

challengeBytes :: Int
challengeBytes = 32

challengeTtlSeconds :: Int
challengeTtlSeconds = 300

issueChallenge :: (MonadFlow m, CacheFlow m r) => m Challenge
issueChallenge = do
  challengeId <- generateGUID
  challenge <- encodeBase64Url <$> liftIO (getRandomBytes challengeBytes)
  now <- getCurrentTime
  Hedis.setExp (challengeKey challengeId) challenge challengeTtlSeconds
  pure Challenge {expiresAt = addUTCTime (fromIntegral challengeTtlSeconds) now, ..}

data ChallengeUse
  = ChallengeConsumed Text
  | ChallengeReplayed
  | ChallengeUnknown
  deriving (Show, Eq)

consumeChallenge :: (MonadFlow m, CacheFlow m r) => Text -> m ChallengeUse
consumeChallenge challengeId = inIssuingCloud challengeId $ do
  Hedis.safeGet (challengeKey challengeId) >>= \case
    Nothing -> do
      consumed <- Hedis.safeGet @Bool (consumedKey challengeId)
      pure $ if isJust consumed then ChallengeReplayed else ChallengeUnknown
    Just challenge -> do
      firstUse <- Hedis.setNxExpire (consumedKey challengeId) challengeTtlSeconds True
      if firstUse
        then do
          Hedis.del (challengeKey challengeId)
          pure $ ChallengeConsumed challenge
        else pure ChallengeReplayed

inIssuingCloud :: (MonadFlow m, CacheFlow m r) => Text -> m a -> m a
inIssuingCloud challengeId action =
  case cloudCodeOfId challengeId >>= cloudTypeFromCode of
    Just issuingCloud | Just issuingCloud /= localCloudType -> do
      peers <- asks (.peerHedisClusterEnvs)
      maybe action (\peerEnv -> local (Hedis.withCloudRedisEnv peerEnv) action) (lookup issuingCloud peers)
    _ -> action
