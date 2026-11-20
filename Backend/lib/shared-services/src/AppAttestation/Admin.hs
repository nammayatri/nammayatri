{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Admin
  ( AttestationConfigRes (..),
    AttestationConfigUpdateReq (..),
    RevokeIosKeyReq (..),
    OptionalSetting (..),
    AttestationSettings (..),
    AttestationConfigUpdate (..),
    toConfigRes,
    applyConfigUpdate,
    refreshWhenStored,
    revokeIosKey,
  )
where

import qualified AppAttestation.AppAttest.Verify as AppAttest
import qualified AppAttestation.Domain.Types.AppAttestKey as Fields
import AppAttestation.Domain.Types.AppAttestationConfig
import AppAttestation.Domain.Types.Common
import AppAttestation.Policy (enforces)
import qualified AppAttestation.Storage.BeamFlow as BeamFlow
import qualified AppAttestation.Storage.Queries.AppAttestKey as QKey
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption (encrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.HideSecrets (HideSecrets (..))
import Kernel.Types.Id
import Kernel.Types.Version (textToVersion)
import Kernel.Utils.Common
import qualified Kernel.Utils.JWT as JWT

data AttestationConfigRes = AttestationConfigRes
  { configured :: Bool,
    mode :: AttestationMode,
    onInvalid :: AttestationAction,
    onMissing :: AttestationAction,
    onUnsupportedOrError :: AttestationAction,
    minAndroidDeviceVerdict :: AndroidDeviceVerdict,
    requireAndroidPlayRecognizedApp :: Bool,
    requireAndroidLicensedApp :: Bool,
    maxAndroidRecentDeviceActivityLevel :: Maybe Int,
    rejectedAndroidAppAccessRisks :: [Text],
    androidApps :: [AndroidAppConfig],
    iosApps :: [IosAppConfig],
    minAndroidAttestingAppVersion :: Maybe Text,
    minIosAttestingAppVersion :: Maybe Text,
    playIntegrityUrl :: Maybe Text,
    playIntegrityServiceAccountSet :: Bool,
    customAppAttestRootCert :: Bool,
    tokenMaxAgeSeconds :: Int,
    platformTimeoutMs :: Int,
    maxOtpPerIosKeyPerDay :: Maybe Int,
    fallbackMethods :: [FallbackMethod],
    fallbackMaxOtpPerPhonePerHour :: Int,
    fallbackMaxOtpPerIpPerHour :: Int,
    captchaVerifyUrl :: Maybe Text,
    captchaSecretSet :: Bool,
    captchaMinScore :: Maybe Double,
    captchaAllowedHostnames :: Maybe [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data AttestationConfigUpdateReq = AttestationConfigUpdateReq
  { mode :: Maybe AttestationMode,
    onInvalid :: Maybe AttestationAction,
    onMissing :: Maybe AttestationAction,
    onUnsupportedOrError :: Maybe AttestationAction,
    minAndroidDeviceVerdict :: Maybe AndroidDeviceVerdict,
    requireAndroidPlayRecognizedApp :: Maybe Bool,
    requireAndroidLicensedApp :: Maybe Bool,
    maxAndroidRecentDeviceActivityLevel :: Maybe Int,
    rejectedAndroidAppAccessRisks :: Maybe [Text],
    androidApps :: Maybe [AndroidAppConfig],
    iosApps :: Maybe [IosAppConfig],
    minAndroidAttestingAppVersion :: Maybe Text,
    minIosAttestingAppVersion :: Maybe Text,
    playIntegrityUrl :: Maybe Text,
    playIntegrityServiceAccountJson :: Maybe Text,
    appAttestRootCertPem :: Maybe Text,
    tokenMaxAgeSeconds :: Maybe Int,
    platformTimeoutMs :: Maybe Int,
    maxOtpPerIosKeyPerDay :: Maybe Int,
    fallbackMethods :: Maybe [FallbackMethod],
    fallbackMaxOtpPerPhonePerHour :: Maybe Int,
    fallbackMaxOtpPerIpPerHour :: Maybe Int,
    captchaVerifyUrl :: Maybe Text,
    captchaSecret :: Maybe Text,
    captchaMinScore :: Maybe Double,
    captchaAllowedHostnames :: Maybe [Text],
    unset :: Maybe [OptionalSetting]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data OptionalSetting
  = MAX_ANDROID_RECENT_DEVICE_ACTIVITY_LEVEL
  | MIN_ANDROID_ATTESTING_APP_VERSION
  | MIN_IOS_ATTESTING_APP_VERSION
  | MAX_OTP_PER_IOS_KEY_PER_DAY
  | CAPTCHA_MIN_SCORE
  | CAPTCHA_ALLOWED_HOSTNAMES
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets AttestationConfigUpdateReq where
  hideSecrets req =
    req{playIntegrityServiceAccountJson = "***" <$ req.playIntegrityServiceAccountJson,
        captchaSecret = "***" <$ req.captchaSecret
       }

newtype RevokeIosKeyReq = RevokeIosKeyReq
  { keyId :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RevokeIosKeyReq where
  hideSecrets = identity

data AttestationSettings = AttestationSettings
  { config :: Maybe AppAttestationConfig,
    playIntegrityConfig :: Maybe PlayIntegrityConfig,
    captchaConfig :: Maybe CaptchaConfig
  }

data AttestationConfigUpdate = AttestationConfigUpdate
  { settings :: AttestationSettings,
    changedPlayIntegrityConfig :: Maybe PlayIntegrityConfig,
    changedCaptchaConfig :: Maybe CaptchaConfig
  }

defaultPlayIntegrityUrl :: Text
defaultPlayIntegrityUrl = "https://playintegrity.googleapis.com"

applyConfigUpdate :: (MonadFlow m, EncFlow m r) => AttestationConfigUpdateReq -> AttestationSettings -> m AttestationConfigUpdate
applyConfigUpdate req current = do
  validateRequest req
  let existing = fromMaybe defaultAppAttestationConfig current.config
      fallback = existing.fallback
      playIntegrityRequested = isJust req.playIntegrityServiceAccountJson || isJust req.playIntegrityUrl
      captchaRequested = isJust req.captchaSecret || isJust req.captchaVerifyUrl
  newPlayIntegrityConfig <- updatedPlayIntegrityConfig current.playIntegrityConfig
  newCaptchaConfig <- updatedCaptchaConfig current.captchaConfig
  let newConfig =
        existing
          { mode = fromMaybe existing.mode req.mode,
            onInvalid = fromMaybe existing.onInvalid req.onInvalid,
            onMissing = fromMaybe existing.onMissing req.onMissing,
            onUnsupportedOrError = fromMaybe existing.onUnsupportedOrError req.onUnsupportedOrError,
            minAndroidDeviceVerdict = fromMaybe existing.minAndroidDeviceVerdict req.minAndroidDeviceVerdict,
            requireAndroidPlayRecognizedApp = fromMaybe existing.requireAndroidPlayRecognizedApp req.requireAndroidPlayRecognizedApp,
            requireAndroidLicensedApp = fromMaybe existing.requireAndroidLicensedApp req.requireAndroidLicensedApp,
            maxAndroidRecentDeviceActivityLevel = unlessUnset MAX_ANDROID_RECENT_DEVICE_ACTIVITY_LEVEL req.maxAndroidRecentDeviceActivityLevel existing.maxAndroidRecentDeviceActivityLevel,
            rejectedAndroidAppAccessRisks = fromMaybe existing.rejectedAndroidAppAccessRisks req.rejectedAndroidAppAccessRisks,
            androidApps = fromMaybe existing.androidApps req.androidApps,
            iosApps = fromMaybe existing.iosApps req.iosApps,
            minAndroidAttestingAppVersion = unlessUnset MIN_ANDROID_ATTESTING_APP_VERSION req.minAndroidAttestingAppVersion existing.minAndroidAttestingAppVersion,
            minIosAttestingAppVersion = unlessUnset MIN_IOS_ATTESTING_APP_VERSION req.minIosAttestingAppVersion existing.minIosAttestingAppVersion,
            appAttestRootCertPem = maybe existing.appAttestRootCertPem (\pem -> if T.null (T.strip pem) then Nothing else Just pem) req.appAttestRootCertPem,
            tokenMaxAgeSeconds = fromMaybe existing.tokenMaxAgeSeconds req.tokenMaxAgeSeconds,
            platformTimeoutMs = fromMaybe existing.platformTimeoutMs req.platformTimeoutMs,
            maxOtpPerIosKeyPerDay = unlessUnset MAX_OTP_PER_IOS_KEY_PER_DAY req.maxOtpPerIosKeyPerDay existing.maxOtpPerIosKeyPerDay,
            fallback =
              fallback
                { methods = fromMaybe fallback.methods req.fallbackMethods,
                  maxOtpPerPhonePerHour = fromMaybe fallback.maxOtpPerPhonePerHour req.fallbackMaxOtpPerPhonePerHour,
                  maxOtpPerIpPerHour = fromMaybe fallback.maxOtpPerIpPerHour req.fallbackMaxOtpPerIpPerHour,
                  captchaMinScore = unlessUnset CAPTCHA_MIN_SCORE req.captchaMinScore fallback.captchaMinScore,
                  captchaAllowedHostnames = unlessUnset CAPTCHA_ALLOWED_HOSTNAMES req.captchaAllowedHostnames fallback.captchaAllowedHostnames
                }
          }
      settings = AttestationSettings {config = Just newConfig, playIntegrityConfig = newPlayIntegrityConfig, captchaConfig = newCaptchaConfig}
  when (enforces newConfig.mode) $ do
    when (null newConfig.androidApps && null newConfig.iosApps) $
      throwError $ InvalidRequest "Add at least one Android or iOS app before enforcing"
    when (not (null newConfig.androidApps) && isNothing newPlayIntegrityConfig) $
      throwError $ InvalidRequest "Play Integrity credentials are required to enforce with Android apps"
  pure
    AttestationConfigUpdate
      { settings,
        changedPlayIntegrityConfig = if playIntegrityRequested then newPlayIntegrityConfig else Nothing,
        changedCaptchaConfig = if captchaRequested then newCaptchaConfig else Nothing
      }
  where
    unlessUnset :: OptionalSetting -> Maybe a -> Maybe a -> Maybe a
    unlessUnset setting requested stored
      | setting `elem` fromMaybe [] req.unset = Nothing
      | otherwise = requested <|> stored

    updatedPlayIntegrityConfig existingConfig =
      case (req.playIntegrityServiceAccountJson, existingConfig) of
        (Nothing, Nothing) -> pure Nothing
        (Nothing, Just playIntegrityConfig) -> do
          url <- maybe (pure playIntegrityConfig.url) parseUrl req.playIntegrityUrl
          pure $ Just playIntegrityConfig {url}
        (Just serviceAccountJson, _) -> do
          url <- parseUrl . fromMaybe (maybe defaultPlayIntegrityUrl (showBaseUrl . (.url)) existingConfig) $ req.playIntegrityUrl
          encryptedServiceAccount <- encrypt serviceAccountJson
          pure $ Just PlayIntegrityConfig {url, serviceAccountJson = encryptedServiceAccount}

    updatedCaptchaConfig existingConfig =
      case (req.captchaSecret, existingConfig) of
        (Nothing, Nothing) -> pure Nothing
        (Nothing, Just captchaConfig) -> do
          verifyUrl <- maybe (pure captchaConfig.verifyUrl) parseUrl req.captchaVerifyUrl
          pure $ Just captchaConfig {verifyUrl}
        (Just secretText, _) -> do
          rawUrl <- req.captchaVerifyUrl <|> (showBaseUrl . (.verifyUrl) <$> existingConfig) & fromMaybeM (InvalidRequest "captchaVerifyUrl is required with captchaSecret")
          verifyUrl <- parseUrl rawUrl
          secret <- encrypt secretText
          pure $ Just CaptchaConfig {verifyUrl, secret}

    parseUrl raw = parseBaseUrl raw & fromMaybeM (InvalidRequest $ "Invalid URL: " <> raw)

refreshWhenStored :: MonadFlow m => UTCTime -> m (Maybe UTCTime) -> m () -> m ()
refreshWhenStored savedAt readStoredUpdatedAt refresh =
  fork "app-attestation-credentials-stored" $ do
    stored <- waitUntilStored storedCheckAttempts
    refresh
    logInfo $
      if stored
        then "App attestation credentials reached the database; config caches cleared again"
        else "App attestation credentials not visible in the database after the wait; config caches cleared again"
  where
    waitUntilStored attemptsLeft = do
      stored <- any (>= addUTCTime (-1) savedAt) <$> readStoredUpdatedAt
      if stored || attemptsLeft <= 0
        then pure stored
        else threadDelaySec storedCheckInterval >> waitUntilStored (attemptsLeft - 1)

storedCheckAttempts :: Int
storedCheckAttempts = 150

storedCheckInterval :: Seconds
storedCheckInterval = Seconds 2

revokeIosKey :: BeamFlow.BeamFlow m r => Id MerchantOperatingCity -> RevokeIosKeyReq -> m APISuccess
revokeIosKey merchantOperatingCityId req = do
  key <- QKey.findByKeyId req.keyId >>= fromMaybeM (InvalidRequest "App Attest key not found")
  unless (key.merchantOperatingCityId == merchantOperatingCityId) $ throwError (InvalidRequest "App Attest key not found")
  when (key.status /= REVOKED) $ QKey.updateStatus REVOKED req.keyId
  logInfo $ "App Attest key revoked: " <> req.keyId
  pure Success

validateRequest :: MonadFlow m => AttestationConfigUpdateReq -> m ()
validateRequest req = do
  whenJust req.playIntegrityServiceAccountJson $ \serviceAccountJson ->
    whenLeft (A.eitherDecodeStrict @JWT.ServiceAccount $ TE.encodeUtf8 serviceAccountJson) $ \_ ->
      throwError $ InvalidRequest "playIntegrityServiceAccountJson is not a Google service account key"
  whenJust req.appAttestRootCertPem $ \pem ->
    unless (T.null (T.strip pem) || isJust (AppAttest.trustAnchorsFromPem pem)) $
      throwError $ InvalidRequest "appAttestRootCertPem is not a PEM certificate"
  forM_ [("tokenMaxAgeSeconds", req.tokenMaxAgeSeconds), ("platformTimeoutMs", req.platformTimeoutMs), ("maxOtpPerIosKeyPerDay", req.maxOtpPerIosKeyPerDay), ("fallbackMaxOtpPerPhonePerHour", req.fallbackMaxOtpPerPhonePerHour), ("fallbackMaxOtpPerIpPerHour", req.fallbackMaxOtpPerIpPerHour)] $ \(name, value) ->
    whenJust value $ \v -> unless (v > 0) . throwError . InvalidRequest $ name <> " must be positive"
  forM_ [("minAndroidAttestingAppVersion", req.minAndroidAttestingAppVersion), ("minIosAttestingAppVersion", req.minIosAttestingAppVersion)] $ \(name, value) ->
    whenJust value $ \version ->
      whenLeft (textToVersion version) $ \_ ->
        throwError . InvalidRequest $ name <> " must be a version like 1.2.3"
  forM_ setInRequest $ \(setting, isSet) ->
    when (isSet && setting `elem` fromMaybe [] req.unset) . throwError . InvalidRequest $ show setting <> " cannot be set and unset in one request"
  whenJust req.maxAndroidRecentDeviceActivityLevel $ \level ->
    unless (level >= 1 && level <= 4) $ throwError $ InvalidRequest "maxAndroidRecentDeviceActivityLevel must be between 1 and 4"
  whenJust req.androidApps $ \apps ->
    when (any (\app' -> T.null app'.packageName || null app'.certSha256Digests) apps) $
      throwError $ InvalidRequest "Every Android app needs a package name and at least one signing certificate digest"
  whenJust req.iosApps $ \apps ->
    when (any (\app' -> T.null app'.teamId || T.null app'.bundleId) apps) $
      throwError $ InvalidRequest "Every iOS app needs a team id and a bundle id"
  where
    setInRequest =
      [ (MAX_ANDROID_RECENT_DEVICE_ACTIVITY_LEVEL, isJust req.maxAndroidRecentDeviceActivityLevel),
        (MIN_ANDROID_ATTESTING_APP_VERSION, isJust req.minAndroidAttestingAppVersion),
        (MIN_IOS_ATTESTING_APP_VERSION, isJust req.minIosAttestingAppVersion),
        (MAX_OTP_PER_IOS_KEY_PER_DAY, isJust req.maxOtpPerIosKeyPerDay),
        (CAPTCHA_MIN_SCORE, isJust req.captchaMinScore),
        (CAPTCHA_ALLOWED_HOSTNAMES, isJust req.captchaAllowedHostnames)
      ]
    whenLeft (Left err) action = action err
    whenLeft (Right _) _ = pure ()

toConfigRes :: AttestationSettings -> AttestationConfigRes
toConfigRes settings =
  let AppAttestationConfig {..} = fromMaybe defaultAppAttestationConfig settings.config
   in AttestationConfigRes
        { configured = isJust settings.config,
          playIntegrityUrl = showBaseUrl . (.url) <$> settings.playIntegrityConfig,
          playIntegrityServiceAccountSet = isJust settings.playIntegrityConfig,
          customAppAttestRootCert = isJust appAttestRootCertPem,
          fallbackMethods = fallback.methods,
          fallbackMaxOtpPerPhonePerHour = fallback.maxOtpPerPhonePerHour,
          fallbackMaxOtpPerIpPerHour = fallback.maxOtpPerIpPerHour,
          captchaVerifyUrl = showBaseUrl . (.verifyUrl) <$> settings.captchaConfig,
          captchaSecretSet = isJust settings.captchaConfig,
          captchaMinScore = fallback.captchaMinScore,
          captchaAllowedHostnames = fallback.captchaAllowedHostnames,
          ..
        }
