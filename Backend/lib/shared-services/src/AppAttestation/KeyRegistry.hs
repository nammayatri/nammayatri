{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.KeyRegistry
  ( RegisterKeyReq (..),
    RegisterKeyRes (..),
    KeyRegistrationStatus (..),
    createChallenge,
    registerKey,
  )
where

import qualified AppAttestation.AppAttest.Verify as AppAttest
import AppAttestation.Canonical
import AppAttestation.Challenge
import qualified AppAttestation.Domain.Types.AppAttestKey as DKey
import AppAttestation.Domain.Types.AppAttestationConfig (AppAttestationConfig (..))
import AppAttestation.Domain.Types.Common
import AppAttestation.Limit (withinLimit)
import qualified AppAttestation.Storage.BeamFlow as BeamFlow
import qualified AppAttestation.Storage.Queries.AppAttestKey as QKey
import AppAttestation.Tools.Error
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Kernel.Types.Beckn.City (City)
import Kernel.Types.Error (AuthError (HitsLimitError), GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common

data RegisterKeyReq = RegisterKeyReq
  { merchantId :: Text,
    city :: Maybe City,
    challengeId :: Text,
    keyId :: Text,
    attestation :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data KeyRegistrationStatus = REGISTERED
  deriving (Show, Eq, Generic)

keyRegistrationStatusOptions :: A.Options
keyRegistrationStatusOptions = A.defaultOptions {A.tagSingleConstructors = True}

instance ToJSON KeyRegistrationStatus where
  toJSON = A.genericToJSON keyRegistrationStatusOptions

instance FromJSON KeyRegistrationStatus where
  parseJSON = A.genericParseJSON keyRegistrationStatusOptions

instance ToSchema KeyRegistrationStatus where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema (OpenApi.fromAesonOptions keyRegistrationStatusOptions)

newtype RegisterKeyRes = RegisterKeyRes
  { status :: KeyRegistrationStatus
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

createChallenge :: (MonadFlow m, CacheFlow m r) => Maybe Text -> m Challenge
createChallenge mbClientIp = do
  whenJust mbClientIp $ enforceIpLimit "Challenge" challengesPerIpPerMinute
  issueChallenge

registerKey :: BeamFlow.BeamFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe AppAttestationConfig -> Maybe Text -> RegisterKeyReq -> m RegisterKeyRes
registerKey merchantId merchantOperatingCityId mbConfig mbClientIp req = do
  whenJust mbClientIp $ enforceIpLimit "KeyRegistration" keyRegistrationsPerIpPerMinute
  let allowedApps = maybe [] (.iosApps) mbConfig
      rootPems = AppAttest.appleAppAttestationRootCaPem : maybeToList (mbConfig >>= (.appAttestRootCertPem))
  when (null allowedApps) $ throwError (AttestationNotConfigured merchantId.getId)
  QKey.findByKeyId req.keyId >>= \case
    Just existing
      | existing.status == ACTIVE -> pure ()
      | otherwise -> throwError $ AttestationKeyRejected "KEY_REVOKED"
    Nothing -> do
      challenge <-
        consumeChallenge req.challengeId >>= \case
          ChallengeConsumed value -> pure value
          _ -> throwError AttestationChallengeInvalid
      keyId <- decodeBase64Lenient req.keyId & fromMaybeM (AttestationKeyRejected "KEY_ID_MALFORMED")
      attestation <- decodeBase64Lenient req.attestation & fromMaybeM (AttestationKeyRejected "ATTESTATION_MALFORMED")
      trustAnchors <-
        AppAttest.trustAnchorsFromPem (T.intercalate "\n" rootPems)
          & fromMaybeM (InternalError "App Attest root certificate is not a valid PEM")
      now <- getCurrentTime
      let check =
            AppAttest.AttestationCheck
              { clientDataHash = keyAttestationDigest challenge,
                ..
              }
      case AppAttest.verifyAttestation check attestation of
        Left reason -> do
          logInfo $ "App Attest key rejected: " <> show reason
          throwError . AttestationKeyRejected $ show reason
        Right verified ->
          QKey.create
            DKey.AppAttestKey
              { keyId = req.keyId,
                merchantId,
                merchantOperatingCityId,
                teamId = verified.iosApp.teamId,
                bundleId = verified.iosApp.bundleId,
                environment = verified.iosApp.environment,
                publicKey = TE.decodeUtf8 $ B64.encode verified.publicKey,
                signCount = 0,
                receipt = TE.decodeUtf8 . B64.encode <$> verified.receipt,
                status = ACTIVE,
                useCount = 0,
                lastUsedAt = Nothing,
                createdAt = now,
                updatedAt = now
              }
  pure $ RegisterKeyRes REGISTERED

challengesPerIpPerMinute :: Int
challengesPerIpPerMinute = 300

keyRegistrationsPerIpPerMinute :: Int
keyRegistrationsPerIpPerMinute = 60

enforceIpLimit :: (MonadFlow m, CacheFlow m r) => Text -> Int -> Text -> m ()
enforceIpLimit scope limit clientIp =
  unlessM (withinLimit ("Ip:" <> scope <> ":" <> clientIp) limit windowSeconds) $
    throwError $ HitsLimitError windowSeconds
  where
    windowSeconds = 60
