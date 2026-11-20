{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Gate
  ( AttestationContext (..),
    GateRequest (..),
    GateVerdict (..),
    GateFlow,
    runGate,
    SilentDrop (..),
    markSilentlyDropped,
    isSilentlyDropped,
    findSilentDrop,
    useSilentDropAttempt,
  )
where

import qualified AppAttestation.AppAttest.Verify as AppAttest
import AppAttestation.Canonical
import AppAttestation.Captcha (verifyCaptcha)
import AppAttestation.Challenge (ChallengeUse (..), consumeChallenge)
import qualified AppAttestation.Domain.Types.AppAttestKey as DKey
import AppAttestation.Domain.Types.AppAttestationConfig
import AppAttestation.Domain.Types.Common
import AppAttestation.Event
import AppAttestation.Limit (withinLimit)
import AppAttestation.Metrics (recordAttestationResult)
import qualified AppAttestation.PlayIntegrity.Client as PlayIntegrity
import qualified AppAttestation.PlayIntegrity.Types as PlayIntegrity
import qualified AppAttestation.PlayIntegrity.Verify as PlayIntegrity
import AppAttestation.Policy
import qualified AppAttestation.Storage.BeamFlow as BeamFlow
import qualified AppAttestation.Storage.Queries.AppAttestKey as QKey
import AppAttestation.Tools.Error
import AppAttestation.Types
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Types (AwaitingError (..), Microseconds (..))
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (AuthError (HitsLimitError))
import Kernel.Types.Id
import Kernel.Types.Version (DeviceType (..), Version, textToVersion, versionToText)
import Kernel.Utils.Common hiding (ERROR, Microseconds)

type GateFlow m r =
  ( BeamFlow.BeamFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    HasKafkaProducer r,
    MonadMask m
  )

data AttestationContext m = AttestationContext
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    config :: AppAttestationConfig,
    loadPlayIntegrityConfig :: m (Maybe PlayIntegrityConfig),
    loadCaptchaConfig :: m (Maybe CaptchaConfig)
  }

data GateRequest = GateRequest
  { service :: Text,
    endpoint :: Endpoint,
    canonicalRequest :: CanonicalRequest,
    headers :: AttestationHeaders,
    userKind :: UserKind,
    usesWhatsappOtp :: Bool,
    clientIp :: Maybe Text,
    identifierHash :: Maybe DbHash,
    clientVersion :: Maybe Version,
    deviceType :: Maybe DeviceType,
    device :: Maybe Text
  }

data GateVerdict
  = Proceed
  | SilentlyDrop
  deriving (Show, Eq)

data Resolution
  = Allowed
  | AllowedViaFallback FallbackMethod
  | SilentlyDropped
  | Rejected
  | FallbackRequired [FallbackMethod]
  | FallbackLimited
  | ChallengeInvalid
  deriving (Show, Eq)

runGate :: GateFlow m r => m (Maybe (AttestationContext m)) -> GateRequest -> m GateVerdict
runGate loadContext request =
  withTryCatch "app-attestation-config" loadContext >>= \case
    Left err -> do
      logError $ "App attestation config could not be read, not checking: " <> show err
      pure Proceed
    Right Nothing -> pure Proceed
    Right (Just ctx)
      | ctx.config.mode == OFF -> pure Proceed
      | not (enforcedFor ctx.config.mode request.userKind) -> do
        fork "app-attestation-shadow" . void $ assess ctx request
        pure Proceed
      | otherwise ->
        assess ctx request >>= \case
          Allowed -> pure Proceed
          AllowedViaFallback _ -> pure Proceed
          SilentlyDropped -> pure SilentlyDrop
          Rejected -> throwError AttestationFailed
          FallbackRequired methods -> throwError $ AttestationRequired methods
          FallbackLimited -> throwError $ HitsLimitError fallbackWindowSeconds
          ChallengeInvalid -> throwError AttestationChallengeInvalid

assess :: GateFlow m r => AttestationContext m -> GateRequest -> m Resolution
assess ctx request = do
  let config = ctx.config
  startedAt <- getCurrentTime
  result <- classifyMissing config request <$> verifyWithinTimeout ctx request
  let actions = PolicyActions {onInvalid = config.onInvalid, onMissing = config.onMissing, onUnsupportedOrError = config.onUnsupportedOrError}
      decision = decide config.mode actions request.userKind result.outcome
      enforcedDecision = decide ENFORCE_ALL actions request.userKind result.outcome
      needsNewChallenge = enforcedFor config.mode request.userKind && any (`elem` result.reasons) [CHALLENGE_UNKNOWN, KEY_BUSY]
  resolution <- case decision of
    _ | needsNewChallenge -> pure ChallengeInvalid
    DecisionAllow -> pure Allowed
    DecisionSilentDrop -> pure SilentlyDropped
    DecisionReject -> pure Rejected
    DecisionFallback -> resolveFallback ctx request
  finishedAt <- getCurrentTime
  record ctx request result resolution enforcedDecision (diffMilliseconds finishedAt startedAt)
  pure resolution
  where
    diffMilliseconds finishedAt startedAt = round $ diffUTCTime finishedAt startedAt * 1000

record :: GateFlow m r => AttestationContext m -> GateRequest -> AttestationResult -> Resolution -> Decision -> Int -> m ()
record ctx request result resolution enforcedDecision latencyMs = do
  eventId <- generateGUID
  timestamp <- getCurrentTime
  let decision = resolutionLabel resolution
  recordAttestationResult
    request.service
    ctx.merchantOperatingCityId.getId
    (maybe "NONE" show result.platform)
    (show request.endpoint)
    (show result.outcome)
    decision
  when (result.outcome /= VALID) $
    logInfo $ "App attestation " <> show result.outcome <> " " <> show result.reasons <> " -> " <> decision
  publishAttestationEvent
    AttestationEvent
      { service = request.service,
        merchantId = ctx.merchantId.getId,
        merchantOperatingCityId = ctx.merchantOperatingCityId.getId,
        endpoint = request.endpoint,
        mode = ctx.config.mode,
        platform = result.platform,
        outcome = result.outcome,
        reasons = result.reasons,
        enforcedDecision = decisionLabel enforcedDecision,
        fallbackMethod = case resolution of
          AllowedViaFallback method -> Just method
          _ -> Nothing,
        userKind = request.userKind,
        verdictSummary = decodeUtf8 . A.encode <$> result.verdictSummary,
        clientVersion = versionToText <$> request.clientVersion,
        device = request.device,
        clientIp = request.clientIp,
        identifierHash = identifierHashText request,
        ..
      }

resolutionLabel :: Resolution -> Text
resolutionLabel = \case
  Allowed -> "ALLOW"
  AllowedViaFallback _ -> "ALLOW_VIA_FALLBACK"
  SilentlyDropped -> "SILENT_DROP"
  Rejected -> "REJECT"
  FallbackRequired _ -> "FALLBACK_REQUIRED"
  FallbackLimited -> "FALLBACK_LIMITED"
  ChallengeInvalid -> "CHALLENGE_INVALID"

decisionLabel :: Decision -> Text
decisionLabel = \case
  DecisionAllow -> "ALLOW"
  DecisionFallback -> "FALLBACK"
  DecisionSilentDrop -> "SILENT_DROP"
  DecisionReject -> "REJECT"

classifyMissing :: AppAttestationConfig -> GateRequest -> AttestationResult -> AttestationResult
classifyMissing config request result
  | result.outcome == MISSING && attestingVersion = result {outcome = INVALID, reasons = [MISSING_ON_ATTESTING_VERSION]}
  | otherwise = result
  where
    attestingVersion = fromMaybe False $ do
      clientVersion <- request.clientVersion
      minVersion <- case request.deviceType of
        Just ANDROID -> config.minAndroidAttestingAppVersion
        Just IOS -> config.minIosAttestingAppVersion
        Nothing -> Nothing
      (clientVersion >=) <$> rightToMaybe (textToVersion minVersion)

verifyWithinTimeout :: GateFlow m r => AttestationContext m -> GateRequest -> m AttestationResult
verifyWithinTimeout ctx request = case parseEvidence request.headers of
  NoEvidence -> pure $ resultOf Nothing MISSING [NO_ATTESTATION]
  MalformedEvidence platform reason -> pure $ resultOf platform INVALID [reason]
  ClientUnsupported platform _ -> pure $ resultOf platform UNSUPPORTED [CLIENT_REPORTED_UNSUPPORTED]
  AndroidEvidence {..}
    | T.length token > maxTokenLength -> pure $ resultOf (Just ANDROID_PLAY_INTEGRITY) INVALID [TOKEN_TOO_LARGE]
    | otherwise -> bounded ANDROID_PLAY_INTEGRITY $ verifyAndroid ctx request challengeId token
  IosEvidence {..}
    | T.length assertion > maxTokenLength || T.length keyId > maxKeyIdLength -> pure $ resultOf (Just IOS_APP_ATTEST) INVALID [TOKEN_TOO_LARGE]
    | otherwise -> bounded IOS_APP_ATTEST $ verifyIos ctx.config request challengeId keyId assertion
  where
    bounded platform verification = do
      awaitable <- awaitableFork "app-attestation-verify" verification
      L.await (Just . Microseconds $ fromIntegral ctx.config.platformTimeoutMs * 1000) awaitable >>= \case
        Right result -> pure result
        Left AwaitingTimeout -> failed platform PLATFORM_TIMEOUT "timed out"
        Left (ForkedFlowError err) -> failed platform PLATFORM_ERROR err
    failed platform reason err = do
      logError $ "App attestation verification failed: " <> err
      pure $ resultOf (Just platform) ERROR [reason]

verifyAndroid :: GateFlow m r => AttestationContext m -> GateRequest -> Text -> Text -> m AttestationResult
verifyAndroid ctx request challengeId integrityToken =
  consumeChallenge challengeId >>= \case
    ChallengeReplayed -> pure $ android INVALID [CHALLENGE_REPLAYED]
    ChallengeUnknown -> pure $ android ERROR [CHALLENGE_UNKNOWN]
    ChallengeConsumed challenge -> do
      mbPlayIntegrityConfig <- if null config.androidApps then pure Nothing else ctx.loadPlayIntegrityConfig
      case mbPlayIntegrityConfig of
        Nothing -> pure $ android ERROR [PLATFORM_NOT_CONFIGURED]
        Just playIntegrityConfig -> do
          now <- getCurrentTime
          let checks =
                PlayIntegrity.VerdictChecks
                  { allowedApps = config.androidApps,
                    expectedRequestHash = androidRequestHash request.canonicalRequest challenge,
                    tokenMaxAgeSeconds = config.tokenMaxAgeSeconds,
                    minDeviceVerdict = config.minAndroidDeviceVerdict,
                    requirePlayRecognizedApp = config.requireAndroidPlayRecognizedApp,
                    requireLicensedApp = config.requireAndroidLicensedApp,
                    maxRecentDeviceActivityLevel = config.maxAndroidRecentDeviceActivityLevel,
                    rejectedAppAccessRisks = config.rejectedAndroidAppAccessRisks,
                    ..
                  }
          decodeWithAnyApp playIntegrityConfig (map (.packageName) config.androidApps) <&> \case
            Left PlayIntegrity.TokenRejected -> android INVALID [TOKEN_DECODE_FAILED]
            Left (PlayIntegrity.PlatformFailure _) -> android ERROR [PLATFORM_ERROR]
            Right payload ->
              let reasons = PlayIntegrity.verifyVerdict checks payload
               in withVerdictSummary (androidVerdictSummary payload) $ android (if null reasons then VALID else INVALID) reasons
  where
    config = ctx.config
    android = resultOf (Just ANDROID_PLAY_INTEGRITY)

    decodeWithAnyApp playIntegrityConfig = tryApps Nothing
      where
        tryApps platformFailure [] = pure . Left $ fromMaybe PlayIntegrity.TokenRejected platformFailure
        tryApps platformFailure (packageName : others) =
          PlayIntegrity.decodeIntegrityToken playIntegrityConfig packageName integrityToken >>= \case
            Right payload -> pure $ Right payload
            Left PlayIntegrity.TokenRejected -> tryApps platformFailure others
            Left failure -> tryApps (Just $ fromMaybe failure platformFailure) others

androidVerdictSummary :: PlayIntegrity.TokenPayload -> Value
androidVerdictSummary payload =
  object
    [ "appRecognitionVerdict" .= (payload.appIntegrity >>= (.appRecognitionVerdict)),
      "versionCode" .= (payload.appIntegrity >>= (.versionCode)),
      "deviceRecognitionVerdict" .= (payload.deviceIntegrity >>= (.deviceRecognitionVerdict)),
      "deviceActivityLevel" .= (payload.deviceIntegrity >>= (.recentDeviceActivity) >>= (.deviceActivityLevel)),
      "appLicensingVerdict" .= (payload.accountDetails >>= (.appLicensingVerdict)),
      "appsDetected" .= (payload.environmentDetails >>= (.appAccessRiskVerdict) >>= (.appsDetected)),
      "playProtectVerdict" .= (payload.environmentDetails >>= (.playProtectVerdict))
    ]

verifyIos :: GateFlow m r => AppAttestationConfig -> GateRequest -> Text -> Text -> Text -> m AttestationResult
verifyIos config request challengeId keyId rawAssertion =
  consumeChallenge challengeId >>= \case
    ChallengeReplayed -> pure $ ios INVALID [CHALLENGE_REPLAYED]
    ChallengeUnknown -> pure $ ios ERROR [CHALLENGE_UNKNOWN]
    ChallengeConsumed challenge
      | null config.iosApps -> pure $ ios ERROR [PLATFORM_NOT_CONFIGURED]
      | otherwise -> case decodeBase64Lenient rawAssertion of
        Nothing -> pure $ ios INVALID [ASSERTION_MALFORMED]
        Just assertion ->
          Hedis.whenWithLockRedisAndReturnValue (keyLock keyId) keyLockSeconds (verifyWithKey challenge assertion)
            <&> either (const $ ios ERROR [KEY_BUSY]) identity
  where
    ios = resultOf (Just IOS_APP_ATTEST)

    verifyWithKey challenge assertion =
      QKey.findByKeyId keyId >>= \case
        Nothing -> pure $ ios INVALID [KEY_NOT_REGISTERED]
        Just key
          | key.status /= ACTIVE -> pure $ ios INVALID [KEY_REVOKED]
          | not (keyAppAllowed key) -> pure $ ios INVALID [APP_NOT_ALLOWED]
          | otherwise -> case decodeBase64Lenient key.publicKey of
            Nothing -> pure $ ios ERROR [PLATFORM_ERROR]
            Just publicKey -> do
              let check =
                    AppAttest.AssertionCheck
                      { clientDataHash = requestDigest request.canonicalRequest challenge,
                        teamId = key.teamId,
                        bundleId = key.bundleId,
                        storedSignCount = key.signCount,
                        ..
                      }
              case AppAttest.verifyAssertion check assertion of
                Left reason -> pure $ ios INVALID [reason]
                Right signCount -> do
                  now <- getCurrentTime
                  QKey.updateUsage signCount (key.useCount + 1) (Just now) keyId
                  withinVelocity <- maybe (pure True) (\limit -> withinLimit (keyVelocityKey keyId) limit secondsInDay) config.maxOtpPerIosKeyPerDay
                  pure $ if withinVelocity then ios VALID [] else ios INVALID [KEY_VELOCITY_EXCEEDED]

    keyAppAllowed :: DKey.AppAttestKey -> Bool
    keyAppAllowed key =
      any (\allowed -> allowed.teamId == key.teamId && allowed.bundleId == key.bundleId && allowed.environment == key.environment) config.iosApps

resolveFallback :: GateFlow m r => AttestationContext m -> GateRequest -> m Resolution
resolveFallback ctx request = do
  let fallback = ctx.config.fallback
  satisfied <- firstSatisfied fallback fallback.methods
  case satisfied of
    Nothing -> pure $ FallbackRequired fallback.methods
    Just method -> do
      withinPhoneLimit <- maybe (pure True) (\hash -> withinLimit (fallbackPhoneKey hash) fallback.maxOtpPerPhonePerHour fallbackWindowSeconds) (identifierHashText request)
      withinIpLimit <- maybe (pure True) (\ip -> withinLimit (fallbackIpKey ip) fallback.maxOtpPerIpPerHour fallbackWindowSeconds) request.clientIp
      pure $ if withinPhoneLimit && withinIpLimit then AllowedViaFallback method else FallbackLimited
  where
    firstSatisfied _ [] = pure Nothing
    firstSatisfied fallback (method : others) =
      methodSatisfied fallback method >>= \ok -> if ok then pure (Just method) else firstSatisfied fallback others

    methodSatisfied fallback = \case
      WHATSAPP_OTP -> pure request.usesWhatsappOtp
      CAPTCHA -> case request.headers.captchaToken of
        Nothing -> pure False
        Just captchaToken ->
          ctx.loadCaptchaConfig >>= \case
            Nothing -> pure False
            Just captchaConfig -> verifyCaptcha captchaConfig fallback captchaToken request.clientIp

identifierHashText :: GateRequest -> Maybe Text
identifierHashText request =
  request.identifierHash >>= \hash -> case A.toJSON hash of
    A.String hex -> Just hex
    _ -> Nothing

maxTokenLength :: Int
maxTokenLength = 16384

maxKeyIdLength :: Int
maxKeyIdLength = 128

fallbackWindowSeconds :: Int
fallbackWindowSeconds = 3600

secondsInDay :: Int
secondsInDay = 86400

keyLockSeconds :: Int
keyLockSeconds = 10

keyLock :: Text -> Text
keyLock keyId = "AppAttestation:KeyLock:" <> keyId

keyVelocityKey :: Text -> Text
keyVelocityKey keyId = "AppAttestation:KeyVelocity:" <> keyId

fallbackPhoneKey :: Text -> Text
fallbackPhoneKey identifierHash = "AppAttestation:Fallback:Identifier:" <> identifierHash

fallbackIpKey :: Text -> Text
fallbackIpKey clientIp = "AppAttestation:Fallback:Ip:" <> clientIp

silentDropKey :: Text -> Text
silentDropKey authId = "AppAttestation:SilentDrop:" <> authId

silentDropTtlSeconds :: Int
silentDropTtlSeconds = 86400

markSilentlyDropped :: CacheFlow m r => Text -> Id MerchantOperatingCity -> Int -> m ()
markSilentlyDropped authId merchantOperatingCityId attempts =
  Hedis.setExp (silentDropKey authId) SilentDrop {..} silentDropTtlSeconds

isSilentlyDropped :: CacheFlow m r => Text -> m Bool
isSilentlyDropped authId = isJust <$> findSilentDrop authId

useSilentDropAttempt :: CacheFlow m r => SilentDrop -> Text -> m ()
useSilentDropAttempt silentDrop authId =
  when (silentDrop.attempts > 0) $
    Hedis.runInMultiCloudRedisWrite $ Hedis.setExp (silentDropKey authId) silentDrop {attempts = silentDrop.attempts - 1} silentDropTtlSeconds

data SilentDrop = SilentDrop
  { merchantOperatingCityId :: Id MerchantOperatingCity,
    attempts :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

findSilentDrop :: CacheFlow m r => Text -> m (Maybe SilentDrop)
findSilentDrop authId = Hedis.runInMultiCloudRedisMaybeResult (Hedis.safeGet (silentDropKey authId))
