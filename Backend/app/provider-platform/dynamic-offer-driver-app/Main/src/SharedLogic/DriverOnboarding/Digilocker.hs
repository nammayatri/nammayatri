module SharedLogic.DriverOnboarding.Digilocker
  ( getDigiLockerConfig,
    verifyDigiLockerEnabled,
    getAllowedVehicleCategories,
    constructDigiLockerAuthUrl,
    getDigiLockerAuthorizationUrl,
    base64UrlEncodeNoPadding,
    generateCodeChallenge,
  )
where

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.DigilockerVerification as DDV
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import qualified Kernel.External.Verification.Digilocker.Types as DigilockerTypes
import qualified Kernel.External.Verification.Interface as Verification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Network.HTTP.Types.URI as URI
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.DigilockerVerification as QDV
import Tools.Error

getDigiLockerConfig :: Id DMOC.MerchantOperatingCity -> Flow DigilockerTypes.DigiLockerCfg
getDigiLockerConfig merchantOpCityId = do
  transporterConfig <-
    CQTC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  unless (transporterConfig.digilockerEnabled == Just True) $
    throwError DigiLockerNotEnabled

  let serviceName = DMSC.VerificationService Verification.DigiLocker
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOpCityId
      >>= fromMaybeM (InternalError "DigiLocker service config not found. Please configure DigiLocker in merchant_service_config table.")

  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig (Verification.DigiLockerConfig config) ->
      return config
    _ -> throwError $ InternalError "Invalid DigiLocker service config type"

verifyDigiLockerEnabled :: Id DMOC.MerchantOperatingCity -> Flow ()
verifyDigiLockerEnabled merchantOpCityId = do
  transporterConfig <-
    CQTC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  unless (fromMaybe False transporterConfig.digilockerEnabled) $
    throwError DigiLockerNotEnabled

  logInfo $ "DigiLocker initiate - Verified DigiLocker is enabled for merchantOpCityId: " <> merchantOpCityId.getId

-- | Get allowed vehicle categories for DigiLocker from config
-- Note: Assumes DigiLocker is already enabled (checked before calling this function)
-- Uses getDigiLockerConfig to get the typed config, then extracts allowedVehicleCategories
-- The field is mandatory in the config type, with a default provided during JSON parsing
getAllowedVehicleCategories :: Id DMOC.MerchantOperatingCity -> Flow [DVC.VehicleCategory]
getAllowedVehicleCategories merchantOpCityId = do
  -- Get the typed DigiLocker config (reuses cached query)
  config <- getDigiLockerConfig merchantOpCityId

  -- Extract allowedVehicleCategories from config and convert Text to VehicleCategory
  let categories = catMaybes $ map textToVehicleCategory config.allowedVehicleCategories

  -- Return parsed categories, or default if parsing failed or empty
  if null categories
    then do
      logInfo $ "DigiLocker getAllowedVehicleCategories - Using default categories for merchantOpCityId: " <> merchantOpCityId.getId <> ". Config had: " <> show config.allowedVehicleCategories
      return defaultAllowedVehicleCategories
    else return categories
  where
    -- Default allowed vehicle categories if parsing fails or config is empty
    defaultAllowedVehicleCategories = [DVC.AUTO_CATEGORY, DVC.CAR, DVC.MOTORCYCLE]

    -- Convert Text to VehicleCategory
    textToVehicleCategory :: Text -> Maybe DVC.VehicleCategory
    textToVehicleCategory txt
      | txt == "AUTO_CATEGORY" = Just DVC.AUTO_CATEGORY
      | txt == "CAR" = Just DVC.CAR
      | txt == "MOTORCYCLE" = Just DVC.MOTORCYCLE
      | txt == "AMBULANCE" = Just DVC.AMBULANCE
      | txt == "TRUCK" = Just DVC.TRUCK
      | txt == "BOAT" = Just DVC.BOAT
      | txt == "BUS" = Just DVC.BUS
      | txt == "TOTO" = Just DVC.TOTO
      | otherwise = Nothing

-- | Base64URL encode without padding (for PKCE)
base64UrlEncodeNoPadding :: ByteString -> Text
base64UrlEncodeNoPadding bytes =
  let base64Encoded = B64.encode bytes
      base64Text = TE.decodeUtf8 base64Encoded
      -- Convert Base64 to Base64URL: replace + with -, / with _, and remove padding =
      base64UrlText = T.replace "+" "-" $ T.replace "/" "_" $ T.replace "=" "" base64Text
   in base64UrlText

-- | Generate code_challenge from code_verifier using SHA256 and Base64URL encoding
-- Implements: code_challenge = base64_url_encode_without_padding(sha256(code_verifier))
generateCodeChallenge :: Text -> Text
generateCodeChallenge codeVerifier =
  let verifierBytes = TE.encodeUtf8 codeVerifier
      digest = Hash.hashWith Hash.SHA256 verifierBytes
      hashBytes = BA.convert digest :: ByteString
   in base64UrlEncodeNoPadding hashBytes

-- | Construct DigiLocker authorization URL with all required parameters
constructDigiLockerAuthUrl :: DigilockerTypes.DigiLockerCfg -> Text -> Text -> Text
constructDigiLockerAuthUrl config digiLockerState codeChallenge =
  let baseUrl = Kernel.Prelude.showBaseUrl config.url
      authPath = "/public/oauth2/1/authorize"
      params =
        [ ("response_type", "code"),
          ("client_id", config.clientId),
          ("redirect_uri", config.redirectUri),
          ("state", digiLockerState),
          ("code_challenge", codeChallenge),
          ("code_challenge_method", config.codeChallengeMethod),
          ("pla", "Y"),
          ("plsignup", "Y"),
          ("ulsignup", "Y"),
          ("purpose", "verification")
        ]
      queryString = T.intercalate "&" $ map (\(k, v) -> k <> "=" <> encodeURIComponent v) params
   in baseUrl <> authPath <> "?" <> queryString
  where
    -- URL encode text for query parameters
    encodeURIComponent :: Text -> Text
    encodeURIComponent txt =
      TE.decodeUtf8 $ URI.urlEncode True $ TE.encodeUtf8 txt

-- | Get DigiLocker authorization URL from an existing session
-- Returns Nothing if session doesn't exist, is not in PENDING status, or merchantOperatingCityId is missing
getDigiLockerAuthorizationUrl :: Id DP.Person -> Flow (Maybe Text)
getDigiLockerAuthorizationUrl driverId = do
  latestSessions <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  case latestSessions of
    [] -> return Nothing
    (session : _) -> do
      -- Only return URL if session is PENDING (not yet authorized)
      case session.sessionStatus of
        DDV.PENDING -> do
          let merchantOpCityId = session.merchantOperatingCityId
          digiLockerConfig <- getDigiLockerConfig merchantOpCityId
          let codeChallenge = generateCodeChallenge session.codeVerifier
          return $ Just $ constructDigiLockerAuthUrl digiLockerConfig session.stateId codeChallenge
        _ -> return Nothing
