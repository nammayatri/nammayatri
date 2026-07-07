{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Registration where

import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, sort, sortOn)
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Person as DP
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import Domain.Types.Person as DP
import qualified Domain.Types.Person.Type as PT
import qualified Domain.Types.RegistrationToken as DR
import Domain.Types.Role as DRole
import qualified Domain.Types.ServerName as DTServer
import qualified Domain.Types.Transaction as DTransaction
import qualified EulerHS.Language as L
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import qualified SharedLogic.Transaction as STransaction
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Role as QRole
import qualified Storage.Queries.Transaction as QT
import Tools.Auth
import qualified Tools.Auth.Common as Auth
import Tools.Auth.Merchant
import Tools.Error
import qualified Tools.InternalClient as InternalClient
import qualified Tools.Utils as Utils

data LoginReq = LoginReq
  { email :: Maybe Text,
    password :: Text,
    otp :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Enable2FAReq = Enable2FAReq
  { email :: Text,
    password :: Text,
    merchantId :: ShortId DMerchant.Merchant,
    city :: Maybe City.City,
    otp :: Maybe Text -- Existing authenticator TOTP for re-setup (skip phone OTP)
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Initiate2FASetupReq = Initiate2FASetupReq
  { email :: Maybe Text,
    password :: Maybe Text,
    merchantId :: Maybe (ShortId DMerchant.Merchant), -- Optional when token is provided
    city :: Maybe City.City,
    otp :: Maybe Text, -- Existing authenticator TOTP for re-setup (skip phone OTP)
    token :: Maybe Text -- Auth token for logged-in users (e.g., during merchant switch)
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype Enable2FARes = Enable2FARes
  { qrcode :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Initiate2FASetupRes = Initiate2FASetupRes
  { requestId :: Maybe Text, -- Present when phone OTP flow is initiated
    qrcode :: Maybe Text, -- Present when TOTP verified and new QR code generated
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Verify2FASetupReq = Verify2FASetupReq
  { requestId :: Text,
    otp :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Stored in Redis for pending 2FA setup requests
data Pending2FASetupData = Pending2FASetupData
  { personId :: Id DP.Person,
    merchantId :: Id DMerchant.Merchant,
    merchantShortId :: ShortId DMerchant.Merchant,
    city :: City.City,
    otp :: Text,
    email :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data LoginRes = LoginRes
  { authToken :: Text,
    is2faMandatory :: Bool,
    is2faEnabled :: Bool,
    message :: Text,
    city :: City.City,
    merchantId :: ShortId DMerchant.Merchant,
    enforcementDeadline :: Maybe UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data TwoFaStatusRes = TwoFaStatusRes
  { is2faRequired :: Bool,
    is2faEnabled :: Bool,
    enforcementDeadline :: Maybe UTCTime,
    daysRemaining :: Maybe Int,
    mustEnrollNow :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data SwitchMerchantAndCityReq = SwitchMerchantAndCityReq
  { merchantId :: ShortId DMerchant.Merchant,
    city :: City.City,
    otp :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data SwitchMerchantReq = SwitchMerchantReq
  { merchantId :: ShortId DMerchant.Merchant,
    otp :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data FleetRegisterReq = FleetRegisterReq
  { firstName :: Text,
    lastName :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: ShortId DMerchant.Merchant,
    fleetType :: Maybe FleetType,
    city :: Maybe City.City,
    email :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FleetType = RENTAL_FLEET | NORMAL_FLEET | BUSINESS_FLEET | BOAT_FLEET
  deriving (Generic, ToJSON, FromJSON, ToSchema)

login ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["sendEmailRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int],
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    EncFlow m r
  ) =>
  LoginReq ->
  m LoginRes
login LoginReq {..} = do
  sendEmailRateLimitOptions <- asks (.sendEmailRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeEmailHitsCountKey email) sendEmailRateLimitOptions
  email_ <- email & fromMaybeM (InvalidRequest "Email cannot be empty when login type is email")
  person <- QP.findByEmailAndPassword email_ password >>= fromMaybeM (PersonDoesNotExist email_)
  Auth.checkPasswordExpiry person
  merchantAccessList <- B.runInReplica $ QAccess.findAllMerchantAccessByPersonId person.id
  (merchant', city') <- case merchantAccessList of
    [] -> throwError (InvalidRequest "No access to any merchant")
    merchantAccessList' -> do
      let sortedMerchantAccessList = sortOn DAccess.merchantShortId merchantAccessList'
      let groupedByMerchant = groupBy ((==) `on` DAccess.merchantShortId) sortedMerchantAccessList
      let merchantIds = map DAccess.merchantShortId $ map head groupedByMerchant
      merchants <- QMerchant.findAllByShortIds merchantIds
      let enabledMerchants = filter (\merchant -> merchant.enabled == Just True) merchants
      case enabledMerchants of
        [] -> throwError (InvalidRequest "Account deactivated. Please check with Admin")
        _ -> do
          let merchant = head enabledMerchants
              merchantWithCityList = map (.operatingCity) $ filter (\ma -> ma.merchantId == merchant.id) merchantAccessList'
              defaultCityPresent = elem merchant.defaultOperatingCity merchantWithCityList
              city' = if defaultCityPresent then merchant.defaultOperatingCity else head merchantWithCityList
          pure (merchant, city')
  loginRes <- generateLoginRes person merchant' otp city'
  when (not $ T.null loginRes.authToken) $ do
    buildAndCreateAuthTransaction DTransaction.DashboardUserLogin person merchant'
  pure loginRes

makeEmailHitsCountKey :: Maybe Text -> Text
makeEmailHitsCountKey email = "Email:" <> fromMaybe "" email <> ":hitsCount"

-- Merchant / city switching happens post-authentication (the request carries
-- a valid TokenInfo). We do NOT re-check 2FA here — the user already passed
-- the TOTP gate at login. Re-prompting on every switch is friction without
-- adding meaningful security (the token itself is the auth proof).
switchMerchant ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    EncFlow m r
  ) =>
  TokenInfo ->
  SwitchMerchantReq ->
  m LoginRes
switchMerchant authToken req = do
  merchant <- QMerchant.findByShortId req.merchantId >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  person <- QP.findById authToken.personId >>= fromMaybeM (PersonDoesNotExist authToken.personId.getId)
  generateLoginResWithoutOtp person merchant merchant.defaultOperatingCity

switchMerchantAndCity ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    EncFlow m r
  ) =>
  TokenInfo ->
  SwitchMerchantAndCityReq ->
  m LoginRes
switchMerchantAndCity authToken req = do
  merchant <- QMerchant.findByShortId req.merchantId >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  person <- QP.findById authToken.personId >>= fromMaybeM (PersonDoesNotExist authToken.personId.getId)
  generateLoginResWithoutOtp person merchant req.city

generateLoginRes ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    EncFlow m r
  ) =>
  DP.Person ->
  DMerchant.Merchant ->
  Maybe Text ->
  City.City ->
  m LoginRes
generateLoginRes person merchant otp city = do
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id city >>= fromMaybeM AccessDenied
  twoFaRequired <- is2FARequired
  deadline <- asks (.twoFaEnforcementDeadline)
  (isToken, msg) <- check2FA person twoFaRequired otp
  token <-
    if isToken
      then generateToken person.id merchant city
      else pure ""
  pure $ LoginRes token twoFaRequired person.is2faEnabled msg city merchant.shortId deadline

generateLoginResWithoutOtp ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    EncFlow m r
  ) =>
  DP.Person ->
  DMerchant.Merchant ->
  City.City ->
  m LoginRes
generateLoginResWithoutOtp person merchant city = do
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id city >>= fromMaybeM AccessDenied
  token <- generateToken person.id merchant city
  twoFaRequired <- is2FARequired
  deadline <- asks (.twoFaEnforcementDeadline)
  pure $ LoginRes token twoFaRequired person.is2faEnabled "Logged in successfully" city merchant.shortId deadline

is2FARequired ::
  (HasFlowEnv m r '["is2faMandatory" ::: Bool]) =>
  m Bool
is2FARequired = asks (.is2faMandatory)

check2FA ::
  ( EncFlow m r,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int]
  ) =>
  DP.Person ->
  Bool ->
  Maybe Text ->
  m (Bool, Text)
check2FA person twoFaRequired otp =
  case (twoFaRequired, person.is2faEnabled) of
    (True, True) -> handle2FA person.id person.secretKey otp
    (True, False) -> do
      -- Grace period: if the enforcement deadline is still in the future, let
      -- the user log in without 2FA. Frontend will route them into the setup
      -- flow (skippable). After the deadline (or if no deadline is set), block.
      now <- getCurrentTime
      deadline <- asks (.twoFaEnforcementDeadline)
      let inGrace = maybe False (>= now) deadline
      if inGrace
        then pure (True, "Two-factor authentication not yet enabled. Please enrol before the enforcement date.")
        else pure (False, "2 Factor authentication is not enabled, it is mandatory for this deployment")
    _ -> pure (True, "Logged in successfully")

-- 2FA verify rate limit: 5 failed attempts in 5 min, then locked out for 15 min.
twoFaVerifyMaxFailures :: Int
twoFaVerifyMaxFailures = 5

twoFaVerifyFailureWindowSecs :: Int
twoFaVerifyFailureWindowSecs = 300

twoFaVerifyLockoutSecs :: Int
twoFaVerifyLockoutSecs = 900

make2FAVerifyFailKey :: Id DP.Person -> Text
make2FAVerifyFailKey personId = "Dashboard:2FA:Verify:Fail:" <> personId.getId

make2FAVerifyLockKey :: Id DP.Person -> Text
make2FAVerifyLockKey personId = "Dashboard:2FA:Verify:Lock:" <> personId.getId

handle2FA ::
  ( EncFlow m r,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int]
  ) =>
  Id DP.Person ->
  Maybe Text ->
  Maybe Text ->
  m (Bool, Text)
handle2FA personId secretKey otp = do
  stepSize <- asks (.totpStepSize)
  clockSkew <- asks (.totpClockSkew)
  -- Check lockout first
  locked <- Redis.get @Bool (make2FAVerifyLockKey personId)
  case locked of
    Just True -> pure (False, "Too many failed attempts. Try again in a few minutes.")
    _ -> case (secretKey, otp) of
      (Just key, Just userOtp) -> do
        isValid <- Utils.verifyTOTP stepSize clockSkew key userOtp
        if isValid
          then do
            Redis.del (make2FAVerifyFailKey personId)
            pure (True, "Logged in successfully")
          else do
            failCount <- fromMaybe 0 <$> Redis.get @Int (make2FAVerifyFailKey personId)
            let newCount = failCount + 1
            if newCount >= twoFaVerifyMaxFailures
              then do
                Redis.setExp (make2FAVerifyLockKey personId) True twoFaVerifyLockoutSecs
                Redis.del (make2FAVerifyFailKey personId)
                pure (False, "Too many failed attempts. Try again in a few minutes.")
              else do
                Redis.setExp (make2FAVerifyFailKey personId) newCount twoFaVerifyFailureWindowSecs
                pure (False, "Authenticator OTP does not match")
      (_, Nothing) -> pure (False, "Authenticator OTP is required")
      (Nothing, _) -> pure (False, "Secret key not found for 2FA")

-- Redis keys for 2FA setup flow
make2FASetupKey :: Text -> Text
make2FASetupKey requestId = "Dashboard:2FA:Setup:" <> requestId

make2FASetupAttemptsKey :: Text -> Text
make2FASetupAttemptsKey requestId = "Dashboard:2FA:Attempts:" <> requestId

-- | Step 1: Validate credentials, send OTP to registered phone, return requestId.
-- | Deprecated: Use initiate2FaSetup + verify2FaSetup instead.
enable2fa ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["twoFaIssuerName" ::: Text],
    EncFlow m r
  ) =>
  Enable2FAReq ->
  m Enable2FARes
enable2fa Enable2FAReq {..} = do
  person <- QP.findByEmailAndPassword email password >>= fromMaybeM (PersonDoesNotExist email)
  merchant <- QMerchant.findByShortId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  let city' = fromMaybe merchant.defaultOperatingCity city
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id city' >>= fromMaybeM AccessDenied
  key <- L.runIO Utils.generateSecretKey
  QP.updatePerson2FaSecret person.id key
  stepSize <- asks (.totpStepSize)
  issuer <- asks (.twoFaIssuerName)
  let qrCodeUri = Utils.generateAuthenticatorURI stepSize key email issuer
  pure $ Enable2FARes qrCodeUri

-- | Validate credentials and either:
-- 1. If user provides a valid TOTP (has existing authenticator): regenerate secret, return new QR code directly
-- 2. Otherwise: send OTP to registered phone, return requestId for verify2FaSetup
initiate2FASetup ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    Auth.AuthFlow m r,
    HasFlowEnv m r '["sendEmailRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["twoFaIssuerName" ::: Text],
    EncFlow m r
  ) =>
  Initiate2FASetupReq ->
  m Initiate2FASetupRes
initiate2FASetup Initiate2FASetupReq {..} = do
  -- Resolve person + (merchantId, city) hint from token or credentials.
  (person, tokenMerchantId, tokenCity) <- case token of
    Just authToken -> do
      (personId, tokMerchId, tokCity) <- Auth.verifyPerson authToken
      p <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      pure (p, Just tokMerchId, Just tokCity)
    Nothing -> do
      email_ <- email & fromMaybeM (InvalidRequest "Email is required when token is not provided")
      password_ <- password & fromMaybeM (InvalidRequest "Password is required when token is not provided")
      sendEmailRateLimitOptions <- asks (.sendEmailRateLimitOptions)
      checkSlidingWindowLimitWithOptions (makeEmailHitsCountKey (Just email_)) sendEmailRateLimitOptions
      p <- QP.findByEmailAndPassword email_ password_ >>= fromMaybeM (PersonDoesNotExist email_)
      pure (p, Nothing, Nothing)
  personEmail <- case email of
    Just e -> pure e
    Nothing -> do
      encEmail <- person.email & fromMaybeM (InvalidRequest "Person does not have an email set")
      decrypt encEmail
  -- Resolve merchant only for BAP-vs-BPP routing hint on internal email OTP call;
  -- 2FA state itself lives on Person (deployment-wide, not per-merchant).
  merchant <- case merchantId of
    Just msid -> QMerchant.findByShortId msid >>= fromMaybeM (MerchantDoesNotExist msid.getShortId)
    Nothing -> case tokenMerchantId of
      Just mid -> QMerchant.findById mid >>= fromMaybeM (MerchantDoesNotExist mid.getId)
      Nothing -> throwError $ InvalidRequest "merchantId is required when token is not provided"
  let city' = fromMaybe merchant.defaultOperatingCity (maybe tokenCity Just city)
  stepSize <- asks (.totpStepSize)
  clockSkew <- asks (.totpClockSkew)
  -- If user has existing authenticator and provides valid TOTP, re-setup directly
  case (person.secretKey, otp) of
    (Just existingKey, Just userOtp) -> do
      isValid <- Utils.verifyTOTP stepSize clockSkew existingKey userOtp
      if isValid
        then do
          newKey <- L.runIO Utils.generateSecretKey
          QP.updatePerson2FaSecret person.id newKey
          issuer <- asks (.twoFaIssuerName)
          let qrCodeUri = Utils.generateAuthenticatorURI stepSize newKey personEmail issuer
          pure $ Initiate2FASetupRes {requestId = Nothing, qrcode = Just qrCodeUri, message = "2FA re-setup successful"}
        else throwError (InvalidRequest "Invalid authenticator code")
    _ -> do
      -- No existing authenticator or no TOTP provided: email OTP flow
      reqId <- generateGUID
      let emailReq = InternalClient.SendEmailOTPReq {email = personEmail}
      let callInternalSendEmailOTP =
            if DTServer.APP_BACKEND `elem` merchant.serverNames
              then InternalClient.callBAPInternalSendEmailOTP
              else InternalClient.callBPPInternalSendEmailOTP
      emailRes <- callInternalSendEmailOTP (getShortId merchant.shortId) city' emailReq
      otpCode <- emailRes.otp & fromMaybeM (InternalError "OTP not returned from internal email service")
      let pendingData =
            Pending2FASetupData
              { personId = person.id,
                merchantId = merchant.id,
                merchantShortId = merchant.shortId,
                city = city',
                otp = otpCode,
                email = personEmail
              }
      envOtpTTL <- asks (.twoFaOtpTTLInSecs)
      let otpTTL = fromMaybe 900 envOtpTTL
      Redis.setExp (make2FASetupKey reqId) pendingData otpTTL
      logInfo $ "2FA setup email OTP sent for person: " <> person.id.getId
      pure $ Initiate2FASetupRes {requestId = Just reqId, qrcode = Nothing, message = "Reset code sent to registered email"}

-- | Step 2: Verify phone OTP, generate TOTP secret, return QR code.
-- Works for both first-time setup and reset.
verify2FASetup ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["twoFaMaxOtpVerifyAttempts" ::: Maybe Int],
    HasFlowEnv m r '["twoFaIssuerName" ::: Text],
    EncFlow m r
  ) =>
  Verify2FASetupReq ->
  m Enable2FARes
verify2FASetup Verify2FASetupReq {..} = do
  let redisKey = make2FASetupKey requestId
      attemptsKey = make2FASetupAttemptsKey requestId
  pendingData <- Redis.get @Pending2FASetupData redisKey >>= fromMaybeM (InvalidRequest "2FA setup request expired or not found")
  envMaxAttempts <- asks (.twoFaMaxOtpVerifyAttempts)
  envOtpTTL <- asks (.twoFaOtpTTLInSecs)
  stepSize <- asks (.totpStepSize)
  issuer <- asks (.twoFaIssuerName)
  let maxAttempts = fromMaybe 5 envMaxAttempts
      otpTTL = fromMaybe 900 envOtpTTL
  -- Check attempt limit
  attempts <- fromMaybe 0 <$> Redis.get @Int attemptsKey
  when (attempts >= maxAttempts) $ do
    Redis.del redisKey
    Redis.del attemptsKey
    throwError (InvalidRequest "Too many OTP attempts. Please initiate 2FA setup again.")
  -- Verify OTP
  if pendingData.otp == otp
    then do
      -- Clean up Redis
      Redis.del redisKey
      Redis.del attemptsKey
      -- Generate TOTP secret and enable 2FA on the Person row
      key <- L.runIO Utils.generateSecretKey
      QP.updatePerson2FaSecret pendingData.personId key
      let qrCodeUri = Utils.generateAuthenticatorURI stepSize key pendingData.email issuer
      pure $ Enable2FARes qrCodeUri
    else do
      Redis.setExp attemptsKey (attempts + 1) otpTTL
      throwError (InvalidRequest "Invalid OTP")

generateToken ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  DMerchant.Merchant ->
  City.City ->
  m Text
generateToken personId merchant city = do
  case merchant.singleActiveSessionOnly of
    Just True -> generateNewToken personId merchant.id city
    _ -> do
      findPreviousToken <- QR.findByPersonIdAndMerchantIdAndCity personId merchant.id city
      case findPreviousToken of
        Just token -> pure token.token
        Nothing -> generateNewToken personId merchant.id city

generateNewToken ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  City.City ->
  m Text
generateNewToken personId merchantId city = do
  regToken <- buildRegistrationToken personId merchantId city
  Auth.cleanCachedTokensByMerchantIdAndCity personId merchantId city
  QR.deleteAllByPersonIdAndMerchantIdAndCity personId merchantId city
  QR.create regToken
  pure $ regToken.token

logout ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logout tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantDoesNotExist tokenInfo.merchantId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokensByMerchantIdAndCity personId tokenInfo.merchantId tokenInfo.city
  QR.deleteAllByPersonIdAndMerchantIdAndCity person.id tokenInfo.merchantId tokenInfo.city
  buildAndCreateAuthTransaction DTransaction.DashboardUserLogout person merchant
  pure $ LogoutRes "Logged out successfully"

logoutAllMerchants ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logoutAllMerchants tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantDoesNotExist tokenInfo.merchantId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokens personId
  QR.deleteAllByPersonId person.id
  buildAndCreateAuthTransaction DTransaction.DashboardUserLogout person merchant
  pure $ LogoutRes "Logged out successfully from all servers"

buildRegistrationToken :: MonadFlow m => Id DP.Person -> Id DMerchant.Merchant -> City.City -> m DR.RegistrationToken
buildRegistrationToken personId merchantId city = do
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  return $
    DR.RegistrationToken
      { id = Id rtid,
        token = token,
        personId = personId,
        merchantId = merchantId,
        createdAt = now,
        operatingCity = city,
        enabled = True
      }

buildAndCreateAuthTransaction ::
  BeamFlow m r =>
  DTransaction.Endpoint ->
  DP.Person ->
  DMerchant.Merchant ->
  m ()
buildAndCreateAuthTransaction endpoint person merchant = do
  whenJust person.dashboardAccessType $ \dashboardAccessType ->
    when (dashboardAccessType `elem` merchant.trackLoginLogoutForRoles) $ do
      transaction <- STransaction.buildDashboardAuthTransaction endpoint person.id merchant.id
      QT.create transaction

registerFleetOwner ::
  ( BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  FleetRegisterReq ->
  Id DP.Person ->
  m APISuccess
registerFleetOwner req personId = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  let validateFn = if fromMaybe True merchant.isStrongNameCheckRequired then validateFleetOwner else weakValidateFleetOwner
  runRequestValidation validateFn req
  unlessM (isNothing <$> QP.findByMobileNumber req.mobileNumber req.mobileCountryCode) $ throwError (InvalidRequest "Phone already registered")
  fleetOwnerRole <- QRole.findByDashboardAccessType (getFleetRole req.fleetType) >>= fromMaybeM (RoleDoesNotExist (show $ getFleetRole req.fleetType))

  merchantServerAccessCheck merchant
  createFleetOwnerDashboardOnly fleetOwnerRole merchant req personId
  return Success
  where
    getFleetRole mbFleetType = case mbFleetType of
      Just RENTAL_FLEET -> RENTAL_FLEET_OWNER
      Just NORMAL_FLEET -> FLEET_OWNER
      Just BUSINESS_FLEET -> FLEET_OWNER
      Just BOAT_FLEET -> FLEET_OWNER
      Nothing -> FLEET_OWNER

buildFleetOwner :: (EncFlow m r) => FleetRegisterReq -> Id DP.Person -> Id DRole.Role -> DRole.DashboardAccessType -> m PT.Person
buildFleetOwner req pid roleId dashboardAccessType = do
  now <- getCurrentTime
  mobileNumber <- encrypt req.mobileNumber
  email <- forM req.email encrypt
  return
    PT.Person
      { id = pid,
        firstName = req.firstName,
        lastName = req.lastName,
        roleId = roleId,
        email = email,
        mobileNumber = mobileNumber,
        mobileCountryCode = req.mobileCountryCode,
        passwordHash = Nothing,
        dashboardAccessType = Just dashboardAccessType,
        receiveNotification = Nothing,
        createdAt = now,
        updatedAt = now,
        verified = Nothing,
        rejectionReason = Nothing,
        rejectedAt = Nothing,
        dashboardType = DEFAULT_DASHBOARD,
        passwordUpdatedAt = Just now,
        approvedBy = Nothing,
        rejectedBy = Nothing,
        language = Nothing,
        secretKey = Nothing,
        is2faEnabled = False
      }

validateFleetOwner :: Validate FleetRegisterReq
validateFleetOwner FleetRegisterReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "lastName" lastName $ NotEmpty `And` P.name,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

weakValidateFleetOwner :: Validate FleetRegisterReq
weakValidateFleetOwner FleetRegisterReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.nameWithNumber,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

createFleetOwnerDashboardOnly ::
  ( BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  DRole.Role ->
  DMerchant.Merchant ->
  FleetRegisterReq ->
  Id DP.Person ->
  m ()
createFleetOwnerDashboardOnly fleetOwnerRole merchant req personId = do
  fleetOwner <- buildFleetOwner req personId fleetOwnerRole.id fleetOwnerRole.dashboardAccessType
  let city' = fromMaybe merchant.defaultOperatingCity req.city
  merchantAccess <- DP.buildMerchantAccess fleetOwner.id merchant.id merchant.shortId city'
  let mbBoolVerified = Just (not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding) && (merchant.verifyFleetWhileLogin == Just True))
  QP.create fleetOwner{verified = mbBoolVerified}
  QAccess.create merchantAccess

-- 2FA status endpoint - used by frontend banner + post-login modal to show
-- countdown and route to enrollment.

getTwoFaStatus ::
  ( BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime]
  ) =>
  TokenInfo ->
  m TwoFaStatusRes
getTwoFaStatus tokenInfo = do
  person <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  is2faRequired <- asks (.is2faMandatory)
  enforcementDeadline <- asks (.twoFaEnforcementDeadline)
  now <- getCurrentTime
  let daysRemaining =
        enforcementDeadline <&> \deadline ->
          floor (realToFrac (diffUTCTime deadline now) / 86400 :: Double) :: Int
      pastGrace = maybe True (< now) enforcementDeadline
      is2faEnabled = person.is2faEnabled
      mustEnrollNow = is2faRequired && pastGrace && not is2faEnabled
  pure TwoFaStatusRes {..}

-- Admin reset - lets a DASHBOARD_ADMIN invalidate someone else's TOTP so they
-- can re-enroll. Force-logs the target out and audit-logs the action.

data TwoFaAdminResetReq = TwoFaAdminResetReq
  { targetPersonId :: Id DP.Person
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype TwoFaAdminResetRes = TwoFaAdminResetRes
  { message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

adminResetTwoFa ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    EncFlow m r
  ) =>
  TokenInfo ->
  TwoFaAdminResetReq ->
  m TwoFaAdminResetRes
adminResetTwoFa tokenInfo TwoFaAdminResetReq {..} = do
  requestor <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  unless (requestor.dashboardAccessType == Just DRole.DASHBOARD_ADMIN) $
    throwError AccessDenied
  target <- QP.findById targetPersonId >>= fromMaybeM (PersonNotFound targetPersonId.getId)
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantDoesNotExist tokenInfo.merchantId.getId)
  -- Clear TOTP secret and disable 2FA on the Person row
  QP.clearPerson2Fa target.id
  -- Force logout across all sessions
  Auth.cleanCachedTokens target.id
  QR.deleteAllByPersonId target.id
  -- Audit log
  transaction <- STransaction.buildDashboardAuthTransaction DTransaction.DashboardTwoFactorAdminReset requestor.id merchant.id
  QT.create transaction
  logInfo $ "2FA admin reset: requestor=" <> requestor.id.getId <> " target=" <> target.id.getId
  pure $ TwoFaAdminResetRes "2FA reset for target user. They must re-enroll on next login."

-- Deadline notification dispatch. Cron-triggered (external scheduler hits the
-- admin-authenticated endpoint below). Sends email nudges at 30/14/7/2/1 days
-- before the merchant's enforcementDeadline. Idempotent per (person, bucket)
-- via a Redis key that lives until the deadline passes.

data DispatchNotificationsRes = DispatchNotificationsRes
  { emailsSent :: Int,
    skipped :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Buckets we dispatch at (days before deadline). Ordered largest-first so
-- if a user has been skipped, they get the tightest applicable warning today.
twoFaNotificationBuckets :: [Int]
twoFaNotificationBuckets = [30, 14, 7, 2, 1]

make2FANotifyKey :: Id DP.Person -> Int -> Text
make2FANotifyKey personId bucket = "Dashboard:2FA:Notify:" <> personId.getId <> ":" <> show bucket

pickBucket :: Int -> Maybe Int
pickBucket daysRemaining = find (\b -> daysRemaining <= b) (reverse (sort twoFaNotificationBuckets))

dispatchTwoFaDeadlineNotifications ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    EncFlow m r
  ) =>
  TokenInfo ->
  m DispatchNotificationsRes
dispatchTwoFaDeadlineNotifications tokenInfo = do
  requestor <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  unless (requestor.dashboardAccessType == Just DRole.DASHBOARD_ADMIN) $
    throwError AccessDenied
  is2faMandatory <- asks (.is2faMandatory)
  enforcementDeadline <- asks (.twoFaEnforcementDeadline)
  unless is2faMandatory $ pure ()
  case enforcementDeadline of
    Nothing -> pure $ DispatchNotificationsRes {emailsSent = 0, skipped = 0}
    Just deadline -> do
      now <- getCurrentTime
      let daysRemaining = floor (realToFrac (diffUTCTime deadline now) / 86400 :: Double) :: Int
      case pickBucket daysRemaining of
        Nothing -> pure $ DispatchNotificationsRes {emailsSent = 0, skipped = 0}
        Just bucket -> do
          -- Any merchant will do for BAP-vs-BPP routing on the internal SendEmailOTP
          -- call — 2FA state is deployment-wide, but the internal email endpoint
          -- lives on a specific downstream service.
          merchants <- QMerchant.findAllMerchants
          case find (\m -> m.enabled == Just True) merchants of
            Nothing -> pure $ DispatchNotificationsRes {emailsSent = 0, skipped = 0}
            Just anyMerchant -> do
              accessRows <- QAccess.findAllUserAccountForMerchant anyMerchant.id
              foldM
                ( \acc access -> do
                    mbPerson <- QP.findById access.personId
                    case mbPerson of
                      Nothing -> pure acc
                      Just person
                        | person.is2faEnabled -> pure acc
                        | otherwise -> do
                          let notifyKey = make2FANotifyKey person.id bucket
                          alreadyNotified <- Redis.get @Bool notifyKey
                          case alreadyNotified of
                            Just True -> pure acc {skipped = acc.skipped + 1}
                            _ -> case person.email of
                              Nothing -> pure acc {skipped = acc.skipped + 1}
                              Just encEmail -> do
                                email <- decrypt encEmail
                                let emailReq = InternalClient.SendEmailOTPReq {email = email}
                                let callInternalSendEmail =
                                      if DTServer.APP_BACKEND `elem` anyMerchant.serverNames
                                        then InternalClient.callBAPInternalSendEmailOTP
                                        else InternalClient.callBPPInternalSendEmailOTP
                                _ <- callInternalSendEmail (getShortId anyMerchant.shortId) access.operatingCity emailReq
                                Redis.setExp notifyKey True (86400 * 35)
                                logInfo $ "2FA deadline notification sent: person=" <> person.id.getId <> " bucket=" <> show bucket
                                pure acc {emailsSent = acc.emailsSent + 1}
                )
                (DispatchNotificationsRes 0 0)
                accessRows
