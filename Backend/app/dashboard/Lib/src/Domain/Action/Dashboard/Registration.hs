{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=x-partial #-}

module Domain.Action.Dashboard.Registration where

import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Person as DP
import qualified Domain.Types.DashboardActionType as DashAuth
import qualified Domain.Types.Entity as Entity
import qualified Domain.Types.EntityAccess as Fields
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import Domain.Types.Person as DP
import qualified Domain.Types.Person.Type as PT
import qualified Domain.Types.RegistrationToken as DR
import Domain.Types.Role as DRole
import qualified Domain.Types.ServerName as DTServer
import qualified Domain.Types.Transaction as DT
import qualified EulerHS.Language as L
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import qualified Kernel.External.Encryption as Fields
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
import qualified Storage.Queries.Entity as QEntity
import qualified Storage.Queries.EntityAccess as QEntityAccess
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Role as QRole
import qualified Storage.Queries.Transaction as QT
import Tools.Auth.Capability (isSuperAdmin)
import qualified Tools.Auth.Common as Auth
import Tools.Auth.Dashboard
import Tools.Auth.Merchant
import Tools.Error
import qualified Tools.InternalClient as InternalClient
import qualified Tools.Utils as Utils

data LoginReq = LoginReq
  { email :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    password :: Maybe Text,
    tokenNo :: Maybe Text,
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

-- 2FA is always enforced. @twoFaMode@ tells the client which second factor applied:
--   * MOBILE_OTP with @otpSent = True@ and an empty @authToken@: an OTP was texted; resubmit the
--     same login request with the code in @otp@.
--   * AUTHENTICATOR_APP with @requiresTwoFaSetup = True@ and an empty @authToken@: the person has
--     no authenticator enrolled yet; route them through initiate2FaSetup / verify2FaSetup.
--   * a non-empty @authToken@: login fully succeeded.
-- The trailing Maybe fields keep the legacy email+password wire shape additive; tokenNo login
-- populates them for the PT welcome screen. entityName is deprecated in favour of entityShortIds
-- but stays on the wire so a frontend can be released after the backend.
data LoginRes = LoginRes
  { authToken :: Text,
    twoFaMode :: Maybe DRole.TwoFaMode,
    otpSent :: Bool,
    requiresTwoFaSetup :: Bool,
    message :: Text,
    city :: City.City,
    merchantId :: ShortId DMerchant.Merchant,
    role :: Maybe Text,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    entityName :: Maybe Text,
    entityShortIds :: [ShortId Entity.Entity]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data TwoFaStatusRes = TwoFaStatusRes
  { twoFaMode :: DRole.TwoFaMode,
    -- | Only meaningful for AUTHENTICATOR_APP: whether the person has enrolled a TOTP secret.
    is2faEnrolled :: Bool
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
    HasRequestId r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["loginRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["twoFaMaxOtpVerifyAttempts" ::: Maybe Int],
    EncFlow m r
  ) =>
  LoginReq ->
  m LoginRes
login rawReq = do
  let LoginReq {..} = sanitizeLoginReq rawReq
  person <-
    case (mobileNumber, mobileCountryCode, tokenNo, email, password) of
      -- tokenNo branch keeps the error generic so callers can't tell
      -- "phone not registered" from "wrong token".
      (Just mob, Just cc, Just tkn, _, _) -> do
        rateLimitOpts <- asks (.loginRateLimitOptions)
        checkSlidingWindowLimitWithOptions (makeMobileHitsCountKey cc mob) rateLimitOpts
        tokenDbHash <- getDbHash tkn
        mbPerson <- QP.findByMobileNumber mob cc
        case mbPerson of
          -- Defense in depth: require DASHBOARD_USER tier so a mistakenly-set
          -- tokenNo on any admin never becomes a login credential.
          Just p
            | (p.tokenNo <&> (.hash)) == Just tokenDbHash,
              p.dashboardAccessType == Just DRole.DASHBOARD_USER ->
              pure p
          _ -> throwError (InvalidRequest "Invalid mobile number or token")
      (_, _, _, Just em, Just pwd) -> do
        loginRateLimitOptions <- asks (.loginRateLimitOptions)
        checkSlidingWindowLimitWithOptions (DP.makeEmailHitsCountKey (Just em)) loginRateLimitOptions
        p <- QP.findByEmailAndPassword em pwd >>= fromMaybeM (PersonDoesNotExist em)
        Auth.checkForcedPasswordChange p
        Auth.checkPasswordExpiry p
        pure p
      _ ->
        throwError $
          InvalidRequest "Provide email+password, or mobileNumber+mobileCountryCode+tokenNo"
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
    buildAndCreateAuthTransaction DT.DashboardUserLogin person merchant'
  pure loginRes

makeMobileHitsCountKey :: Text -> Text -> Text
makeMobileHitsCountKey cc mobile = "MobileTokenLogin:" <> cc <> ":" <> mobile <> ":hitsCount"

-- Trims every field and collapses Just "" to Nothing so blank submissions
-- don't slip into the lookup branches with empty credentials.
sanitizeLoginReq :: LoginReq -> LoginReq
sanitizeLoginReq r =
  LoginReq
    { email = nonBlank r.email,
      mobileNumber = nonBlank r.mobileNumber,
      mobileCountryCode = nonBlank r.mobileCountryCode,
      password = nonBlank r.password,
      tokenNo = nonBlank r.tokenNo,
      otp = nonBlank r.otp
    }
  where
    nonBlank = (>>= \v -> let s = T.strip v in if T.null s then Nothing else Just s)

-- Merchant / city switching happens post-authentication (the request carries
-- a valid TokenInfo). We do NOT re-check 2FA here — the user already passed
-- the TOTP gate at login. Re-prompting on every switch is friction without
-- adding meaningful security (the token itself is the auth proof).
switchMerchant ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
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
    HasRequestId r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["twoFaMaxOtpVerifyAttempts" ::: Maybe Int],
    EncFlow m r
  ) =>
  DP.Person ->
  DMerchant.Merchant ->
  Maybe Text ->
  City.City ->
  m LoginRes
generateLoginRes person merchant otp city = do
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id city >>= fromMaybeM AccessDenied
  mode <- resolve2FAMode person
  (outcome, msg) <- check2FA person mode otp merchant city
  token <-
    if outcome == Authorized
      then generateToken person.id
      else pure ""
  (mbRoleName, personEntities) <- lookupRoleAndEntities person merchant.id
  pure
    LoginRes
      { authToken = token,
        twoFaMode = Just mode,
        otpSent = outcome == OtpChallengeSent,
        requiresTwoFaSetup = outcome == SetupRequired,
        message = msg,
        city = city,
        merchantId = merchant.shortId,
        role = mbRoleName,
        firstName = Just person.firstName,
        lastName = Just person.lastName,
        entityName = listToMaybe personEntities <&> (.entityName),
        entityShortIds = personEntities <&> (.entityShortId)
      }

generateLoginResWithoutOtp ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    EncFlow m r
  ) =>
  DP.Person ->
  DMerchant.Merchant ->
  City.City ->
  m LoginRes
generateLoginResWithoutOtp person merchant city = do
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id city >>= fromMaybeM AccessDenied
  token <- generateToken person.id
  mode <- resolve2FAMode person
  (mbRoleName, personEntities) <- lookupRoleAndEntities person merchant.id
  pure
    LoginRes
      { authToken = token,
        twoFaMode = Just mode,
        otpSent = False,
        requiresTwoFaSetup = False,
        message = "Logged in successfully",
        city = city,
        merchantId = merchant.shortId,
        role = mbRoleName,
        firstName = Just person.firstName,
        lastName = Just person.lastName,
        entityName = listToMaybe personEntities <&> (.entityName),
        entityShortIds = personEntities <&> (.entityShortId)
      }

-- Best-effort: a missing Role or Entity does not fail the login.
-- Merchant-scoped: a person may hold depots under several merchants, and a session for one
-- must never surface another merchant's grants.
lookupRoleAndEntities :: BeamFlow m r => DP.Person -> Id DMerchant.Merchant -> m (Maybe Text, [Entity.Entity])
lookupRoleAndEntities person merchantId = do
  mbRole <- QRole.findById person.roleId
  entityGrants <- QEntityAccess.findAllByPersonIdsAndMerchantId [person.id] merchantId
  let grantedEntityIds = entityGrants <&> (.entityId)
  personEntities <- QEntity.findAllByIdsOrdered grantedEntityIds
  let missingEntityIds = filter (`notElem` (personEntities <&> (.id))) grantedEntityIds
  unless (null missingEntityIds) $
    logError $
      "Dangling entity grants for person " <> person.id.getId <> ": "
        <> T.intercalate ", " (missingEntityIds <&> (.getId))
  pure (mbRole <&> (.name), filter (not . (.deleted)) personEntities)

-- 2FA is always enforced. 'resolve2FAMode' picks the second factor: MOBILE_OTP (an SMS code) when
-- either the person or their role is set to MOBILE_OTP; otherwise AUTHENTICATOR_APP (TOTP), the
-- default. Both are changed through POST /user/twoFaMode, SUPER_ADMIN only.
resolve2FAMode ::
  BeamFlow m r =>
  DP.Person ->
  m DRole.TwoFaMode
resolve2FAMode person =
  if person.twoFaMode == DRole.MOBILE_OTP
    then pure DRole.MOBILE_OTP
    else do
      mbRole <- QRole.findById person.roleId
      pure $
        if maybe False ((== DRole.MOBILE_OTP) . (.twoFaMode)) mbRole
          then DRole.MOBILE_OTP
          else DRole.AUTHENTICATOR_APP

-- | Outcome of the login-time 2FA check.
data TwoFaOutcome
  = -- | Second factor satisfied; mint a session token.
    Authorized
  | -- | MOBILE_OTP: a code was just texted; the client must resubmit login with it.
    OtpChallengeSent
  | -- | AUTHENTICATOR_APP: no authenticator enrolled yet; the client must run 2FA setup first.
    SetupRequired
  | -- | The supplied second factor was wrong or expired.
    TwoFaRejected
  deriving (Eq)

check2FA ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["twoFaMaxOtpVerifyAttempts" ::: Maybe Int]
  ) =>
  DP.Person ->
  DRole.TwoFaMode ->
  Maybe Text ->
  DMerchant.Merchant ->
  City.City ->
  m (TwoFaOutcome, Text)
check2FA person mode otp merchant city =
  case mode of
    DRole.AUTHENTICATOR_APP ->
      case person.secretKey of
        Just _ -> do
          (ok, msg) <- handle2FA person.id person.secretKey otp
          pure (if ok then Authorized else TwoFaRejected, msg)
        Nothing ->
          pure (SetupRequired, "Two-factor authentication setup required. Please enrol your authenticator app before logging in.")
    DRole.MOBILE_OTP ->
      case otp of
        Nothing -> do
          sendMobileLoginOtp person merchant city
          pure (OtpChallengeSent, "An OTP has been sent to your registered mobile number.")
        Just userOtp -> do
          ok <- verifyMobileLoginOtp person userOtp
          pure $ if ok then (Authorized, "Logged in successfully") else (TwoFaRejected, "Invalid or expired OTP")

-- Redis keys for the MOBILE_OTP login challenge.
make2FALoginOtpKey :: Id DP.Person -> Text
make2FALoginOtpKey personId = "Dashboard:2FA:LoginOtp:" <> personId.getId

make2FALoginOtpAttemptsKey :: Id DP.Person -> Text
make2FALoginOtpAttemptsKey personId = "Dashboard:2FA:LoginOtp:Attempts:" <> personId.getId

make2FALoginOtpSendLockKey :: Id DP.Person -> Text
make2FALoginOtpSendLockKey personId = "Dashboard:2FA:LoginOtp:SendLock:" <> personId.getId

-- Short self-releasing lock so concurrent logins for the same person can't fan out into multiple
-- SMS. A failed send stores no code, so once this window lapses a retry can send again.
twoFaLoginOtpSendLockSecs :: Int
twoFaLoginOtpSendLockSecs = 30

-- | Text an OTP to the person's registered mobile number and cache it for verification.
-- If an unexpired code is already outstanding we do NOT send another, so repeated login attempts
-- cannot be used to flood the number with SMS. The downstream /internal/sendSMS generates the OTP
-- (reusing the SEND_OTP template driver login uses) and returns it; we only persist it.
sendMobileLoginOtp ::
  ( EncFlow m r,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int]
  ) =>
  DP.Person ->
  DMerchant.Merchant ->
  City.City ->
  m ()
sendMobileLoginOtp person merchant city = do
  existing <- Redis.get @Text (make2FALoginOtpKey person.id)
  case existing of
    Just _ -> pure () -- an unexpired code is still valid; don't re-send
    Nothing -> do
      -- Atomically claim a short send slot before contacting the SMS service. Concurrent logins
      -- for the same person lose this race and skip, so a burst can't trigger multiple texts; the
      -- lock's short TTL self-releases, so a failed send (which stores no code) can retry once it
      -- lapses -- no explicit release needed.
      claimed <- Redis.setNxExpire (make2FALoginOtpSendLockKey person.id) twoFaLoginOtpSendLockSecs True
      when claimed $ do
        mobile <- decrypt person.mobileNumber
        let phoneNumber = person.mobileCountryCode <> mobile
            smsReq =
              InternalClient.SendSMSReq
                { phoneNumber = phoneNumber,
                  messageKey = "SEND_OTP",
                  templateVars = Map.fromList [("hash", "")],
                  isOtp = Just True
                }
            -- Route by the merchant short-id suffix, as the email-OTP flow does: `_PARTNER` is the
            -- driver-app's own short id (BPP); everything else is rider-app (BAP).
            callInternalSendSMS =
              if "_PARTNER" `T.isSuffixOf` getShortId merchant.shortId
                then InternalClient.callBPPInternalSendSMS
                else InternalClient.callBAPInternalSendSMS
        smsRes <- callInternalSendSMS (getShortId merchant.shortId) city smsReq
        otpCode <- smsRes.otp & fromMaybeM (InternalError "OTP not returned from internal SMS service")
        otpTTL <- fromMaybe 900 <$> asks (.twoFaOtpTTLInSecs)
        Redis.setExp (make2FALoginOtpKey person.id) otpCode otpTTL
        Redis.del (make2FALoginOtpAttemptsKey person.id)
        logInfo $ "2FA login OTP sent for person: " <> person.id.getId

-- | Verify a submitted mobile OTP against the cached code, enforcing an attempt cap.
verifyMobileLoginOtp ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["twoFaMaxOtpVerifyAttempts" ::: Maybe Int]
  ) =>
  DP.Person ->
  Text ->
  m Bool
verifyMobileLoginOtp person userOtp = do
  maxAttempts <- fromMaybe 5 <$> asks (.twoFaMaxOtpVerifyAttempts)
  -- Atomic increment so concurrent attempts can't under-count against the cap; set the expiry on
  -- the first increment so the counter still lapses with the OTP.
  attempts <- Redis.incr (make2FALoginOtpAttemptsKey person.id)
  when (attempts == 1) $ do
    otpTTL <- fromMaybe 900 <$> asks (.twoFaOtpTTLInSecs)
    Redis.expire (make2FALoginOtpAttemptsKey person.id) otpTTL
  if attempts > fromIntegral maxAttempts
    then do
      Redis.del (make2FALoginOtpKey person.id)
      pure False
    else do
      mbStored <- Redis.get @Text (make2FALoginOtpKey person.id)
      case mbStored of
        Nothing -> pure False
        Just stored
          | stored == T.strip userOtp -> do
            Redis.del (make2FALoginOtpKey person.id)
            Redis.del (make2FALoginOtpAttemptsKey person.id)
            pure True
          | otherwise -> pure False

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
    HasFlowEnv m r '["loginRateLimitOptions" ::: APIRateLimitOptions],
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
  -- Resolve the person from the token (identity-only) or from credentials.
  person <- case token of
    Just authToken -> do
      personId <- Auth.verifyPerson authToken
      QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    Nothing -> do
      email_ <- email & fromMaybeM (InvalidRequest "Email is required when token is not provided")
      password_ <- password & fromMaybeM (InvalidRequest "Password is required when token is not provided")
      loginRateLimitOptions <- asks (.loginRateLimitOptions)
      checkSlidingWindowLimitWithOptions (DP.makeEmailHitsCountKey (Just email_)) loginRateLimitOptions
      QP.findByEmailAndPassword email_ password_ >>= fromMaybeM (PersonDoesNotExist email_)
  personEmail <- case email of
    Just e -> pure e
    Nothing -> do
      encEmail <- person.email & fromMaybeM (InvalidRequest "Person does not have an email set")
      decrypt encEmail
  -- Resolve merchant only for BAP-vs-BPP routing of the internal email OTP call;
  -- 2FA state itself lives on Person (deployment-wide, not per-merchant). The
  -- token no longer carries a merchant, so the request must name one.
  merchant <- case merchantId of
    Just msid -> QMerchant.findByShortId msid >>= fromMaybeM (MerchantDoesNotExist msid.getShortId)
    Nothing -> throwError $ InvalidRequest "merchantId is required"
  let city' = fromMaybe merchant.defaultOperatingCity city
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
      -- Pick the platform from the merchant's own short id, not from
      -- serverNames. Post-unification the BPP row (`X_PARTNER`) advertises
      -- APP_BACKEND too, so a serverNames test always chose BAP and then handed
      -- rider-app a `_PARTNER` short id it has never heard of — surfacing as
      -- "No merchant matches passed data" from one hop downstream.
      --
      -- The suffix is the reliable signal: `X_PARTNER` is the driver-app's own
      -- short id, `X` is the rider-app's. Routing by it means the short id we
      -- forward is always one the target already knows, with no translation.
      let callInternalSendEmailOTP =
            if "_PARTNER" `T.isSuffixOf` getShortId merchant.shortId
              then InternalClient.callBPPInternalSendEmailOTP
              else InternalClient.callBAPInternalSendEmailOTP
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

-- | One token per person, identity-only. Wipes any existing tokens for the
-- person and mints exactly one, so @merchant.singleActiveSessionOnly@ is moot --
-- every login is single-session. The token carries no merchant/city; URL +
-- merchant_access govern access on every request.
generateToken ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  m Text
generateToken personId = do
  regToken <- buildRegistrationToken personId
  Auth.cleanCachedTokens personId
  QR.deleteAllByPersonId personId
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
  -- Identity token: one token per person, so logging out drops that single
  -- session wholesale (there is no per-merchant/city token to target).
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokens personId
  QR.deleteAllByPersonId personId
  -- The token no longer names a merchant; audit the logout against the person's
  -- home merchant when it is set and configured to track login/logout.
  auditPersonHomeMerchant DT.DashboardUserLogout person
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
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokens personId
  QR.deleteAllByPersonId person.id
  auditPersonHomeMerchant DT.DashboardUserLogout person
  pure $ LogoutRes "Logged out successfully from all servers"

-- | Write a login/logout audit transaction against the person's home merchant,
-- when it is set. One token per person means there is no session merchant to
-- attribute to; the home merchant (person.merchantId) is the stable stand-in,
-- and the per-merchant trackLoginLogoutForRoles gate still applies via
-- 'buildAndCreateAuthTransaction'.
auditPersonHomeMerchant ::
  BeamFlow m r =>
  DT.Endpoint DashAuth.DashboardActionType ->
  DP.Person ->
  m ()
auditPersonHomeMerchant endpoint person =
  whenJust person.merchantId $ \mId -> do
    mbMerchant <- QMerchant.findById mId
    whenJust mbMerchant $ \merchant -> buildAndCreateAuthTransaction endpoint person merchant

buildRegistrationToken :: MonadFlow m => Id DP.Person -> m DR.RegistrationToken
buildRegistrationToken personId = do
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  return $
    DR.RegistrationToken
      { id = Id rtid,
        token = token,
        personId = personId,
        createdAt = now,
        enabled = True
      }

buildAndCreateAuthTransaction ::
  BeamFlow m r =>
  DT.Endpoint DashAuth.DashboardActionType ->
  DP.Person ->
  DMerchant.Merchant ->
  m ()
buildAndCreateAuthTransaction endpoint person merchant =
  whenJust person.dashboardAccessType $ \dashboardAccessType ->
    when (dashboardAccessType `elem` merchant.trackLoginLogoutForRoles) $ do
      transaction <- STransaction.buildDashboardAuthTransaction endpoint person.id (Just merchant.id)
      QT.createDashboardTransaction transaction

registerFleetOwner ::
  ( BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  FleetRegisterReq ->
  Id DP.Person ->
  m APISuccess
registerFleetOwner req personId = do
  (merchant, fleetOwnerRole) <- validateFleetOwnerRegistration req
  createFleetOwnerDashboardOnly fleetOwnerRole merchant req personId
  return Success

-- | Every check 'registerFleetOwner' makes, with nothing written.
--
-- Separate so a caller that also writes the fleet owner to an application
-- database can run these first: otherwise a request the dashboard side rejects
-- would leave a fleet owner there with no dashboard login.
validateFleetOwnerRegistration ::
  ( BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  FleetRegisterReq ->
  m (DMerchant.Merchant, DRole.Role)
validateFleetOwnerRegistration req = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  let validateFn = if fromMaybe True merchant.isStrongNameCheckRequired then validateFleetOwner else weakValidateFleetOwner
  runRequestValidation validateFn req
  unlessM (isNothing <$> QP.findByMobileNumber req.mobileNumber req.mobileCountryCode) $ throwError (InvalidRequest "Phone already registered")
  fleetOwnerRole <- QRole.findByDashboardAccessType (getFleetRole req.fleetType) >>= fromMaybeM (RoleDoesNotExist (show $ getFleetRole req.fleetType))
  merchantServerAccessCheck merchant
  pure (merchant, fleetOwnerRole)
  where
    getFleetRole mbFleetType = case mbFleetType of
      Just RENTAL_FLEET -> RENTAL_FLEET_OWNER
      Just NORMAL_FLEET -> FLEET_OWNER
      Just BUSINESS_FLEET -> FLEET_OWNER
      Just BOAT_FLEET -> FLEET_OWNER
      Nothing -> FLEET_OWNER

buildFleetOwner :: (EncFlow m r) => FleetRegisterReq -> Id DP.Person -> Id DRole.Role -> DRole.DashboardAccessType -> Id DMerchant.Merchant -> m PT.Person
buildFleetOwner req pid roleId dashboardAccessType merchantId = do
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
        forcePasswordChange = Nothing,
        merchantId = Just merchantId,
        approvedBy = Nothing,
        rejectedBy = Nothing,
        language = Nothing,
        secretKey = Nothing,
        twoFaMode = DRole.AUTHENTICATOR_APP,
        tokenNo = Nothing,
        vpa = Nothing
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
    [ validateField "firstName" firstName $ MinLength 3 `And` P.nameWithSymbols,
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
  fleetOwner <- buildFleetOwner req personId fleetOwnerRole.id fleetOwnerRole.dashboardAccessType merchant.id
  let city' = fromMaybe merchant.defaultOperatingCity req.city
  merchantAccess <- DP.buildMerchantAccess fleetOwner.id merchant.id merchant.shortId city'
  let mbBoolVerified = Just (not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding) && (merchant.verifyFleetWhileLogin == Just True))
  QP.create fleetOwner{verified = mbBoolVerified}
  QAccess.create merchantAccess

-- | Create the dashboard-side operator.

--
-- @operatorId@ is the id the application server allocated, so the dashboard
-- person and the application person share it.
registerOperatorDashboardOnly ::
  ( BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  City.City ->
  Maybe Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Id DP.Person ->
  DMerchant.Merchant ->
  Maybe Text ->
  m ()
registerOperatorDashboardOnly opCity email mobileNumber mobileCountryCode firstName lastName password operatorId merchant mbRoleId = do
  operatorRole <-
    case mbRoleId of
      Just roleId -> QRole.findById (Id roleId) >>= fromMaybeM (RoleNotFound roleId)
      Nothing -> QRole.findByDashboardAccessType DRole.DASHBOARD_OPERATOR >>= fromMaybeM (RoleNotFound "OPERATOR")
  operator <- buildOperator email mobileNumber mobileCountryCode firstName lastName password operatorId operatorRole merchant.id
  merchantAccess <- DP.buildMerchantAccess operator.id merchant.id merchant.shortId opCity
  QP.create operator
  QAccess.create merchantAccess

buildOperator ::
  (EncFlow m r) =>
  Maybe Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Id DP.Person ->
  DRole.Role ->
  Id DMerchant.Merchant ->
  m PT.Person
buildOperator emailUnencrypted mobileNumberUnencrypted mobileCountryCode firstName lastName password operatorId role merchantId = do
  now <- getCurrentTime
  mobileNumber <- encrypt mobileNumberUnencrypted
  email <- forM emailUnencrypted encrypt
  passwordHash <- getDbHash `mapM` password
  return
    PT.Person
      { id = operatorId,
        firstName = firstName,
        lastName = lastName,
        roleId = role.id,
        email = email,
        mobileNumber,
        mobileCountryCode = mobileCountryCode,
        passwordHash,
        dashboardAccessType = Just role.dashboardAccessType,
        receiveNotification = Nothing,
        createdAt = now,
        updatedAt = now,
        verified = Just True,
        rejectionReason = Nothing,
        rejectedAt = Nothing,
        dashboardType = PT.DEFAULT_DASHBOARD,
        passwordUpdatedAt = Nothing,
        forcePasswordChange = Nothing,
        merchantId = Just merchantId,
        approvedBy = Nothing,
        rejectedBy = Nothing,
        language = Nothing,
        secretKey = Nothing,
        twoFaMode = DRole.AUTHENTICATOR_APP,
        tokenNo = Nothing,
        vpa = Nothing
      }

-- 2FA status endpoint - surfaces the person's second-factor mode and, for the authenticator path,
-- whether they have enrolled a TOTP secret yet.

getTwoFaStatus ::
  BeamFlow m r =>
  TokenInfo ->
  m TwoFaStatusRes
getTwoFaStatus tokenInfo = do
  person <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  twoFaMode <- resolve2FAMode person
  let is2faEnrolled = isJust person.secretKey
  pure TwoFaStatusRes {..}

-- Admin reset - lets a DASHBOARD_ADMIN invalidate someone else's TOTP so they
-- can re-enroll. Force-logs the target out and audit-logs the action.

data TwoFaAdminResetReq = TwoFaAdminResetReq
  { targetPersonId :: Id DP.Person
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | One switch for both levels. Exactly one of email (a person) or roleName (every holder of the
-- role) must be given. Keyed by the natural identifier an operator has to hand, not by id, so no
-- list/search round-trip is needed first.
data TwoFaModeReq = TwoFaModeReq
  { email :: Maybe Text,
    roleName :: Maybe Text,
    mode :: DRole.TwoFaMode
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | What gets written to the transaction audit row. The request body is not stored as-is because
-- it carries the email (PII); the target id plus the mode is enough to tell what changed.
data TwoFaModeAudit = TwoFaModeAudit
  { personId :: Maybe (Id DP.Person),
    roleId :: Maybe (Id DRole.Role),
    mode :: DRole.TwoFaMode
  }
  deriving (Generic, ToJSON)

-- | Set the second-factor mode for a person or for a whole role. SUPER_ADMIN only: switching the
-- second factor is a sensitive security control, so the DASHBOARD_ADMIN tier the route requires is
-- not enough. There is no DashboardAuth tier for SUPER_ADMIN (it is an admin_tier, not a
-- DashboardAccessType), hence the explicit isSuperAdmin check, done before any lookup so a
-- non-super-admin cannot probe which emails or role names exist.
--
-- A mode change must take effect on the next request, not whenever the session lapses: the affected
-- persons' cached and registration tokens are dropped so they re-authenticate with the new factor.
setTwoFaMode ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  TwoFaModeReq ->
  m APISuccess
setTwoFaMode tokenInfo req = do
  unlessM (isSuperAdmin tokenInfo.personId) $
    throwError AccessDenied
  affected <- case (req.email, req.roleName) of
    (Just emailRaw, Nothing) -> do
      runRequestValidation validateTwoFaModeEmail emailRaw
      let email = T.toLower emailRaw
      person <- QP.findByEmail email >>= fromMaybeM (PersonDoesNotExist email)
      QP.updatePersonTwoFaMode person.id req.mode
      recordModeChange TwoFaModeAudit {personId = Just person.id, roleId = Nothing, mode = req.mode}
      pure [person]
    (Nothing, Just roleName) -> do
      role <- QRole.findByName roleName >>= fromMaybeM (RoleNotFound roleName)
      QRole.updateRoleTwoFaMode role.id req.mode
      recordModeChange TwoFaModeAudit {personId = Nothing, roleId = Just role.id, mode = req.mode}
      -- Force re-auth for every holder regardless of dashboard type: QP.findAllByRole is
      -- DEFAULT_DASHBOARD-only, so fetch TICKET_DASHBOARD holders too, else they keep a session
      -- under the old second factor until it lapses.
      defaultHolders <- QP.findAllByRoleWithType @'PT.DefaultDashboard role.id
      ticketHolders <- QP.findAllByRoleWithType @'PT.TicketDashboard role.id
      pure (defaultHolders <> ticketHolders)
    _ -> throwError $ InvalidRequest "Exactly one of email or roleName must be provided"
  forM_ affected $ \person -> do
    Auth.cleanCachedTokens person.id
    QR.deleteAllByPersonId person.id
  pure Success
  where
    recordModeChange audit = do
      -- Identity token: attribute the super-admin's change to their home merchant when set.
      mbRequestor <- QP.findById tokenInfo.personId
      transaction <- STransaction.buildDashboardAuthTransaction (DT.DashboardTwoFactorExemptChange :: DT.Endpoint DashAuth.DashboardActionType) tokenInfo.personId (mbRequestor >>= (.merchantId))
      QT.createDashboardTransaction transaction {DT.request = Just $ encodeToText audit}

validateTwoFaModeEmail :: Validate Text
validateTwoFaModeEmail email = validateField "email" email P.email

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
  -- Clear TOTP secret and disable 2FA on the Person row
  QP.clearPerson2Fa target.id
  -- Force logout across all sessions
  Auth.cleanCachedTokens target.id
  QR.deleteAllByPersonId target.id
  -- Audit log. Identity token carries no merchant; attribute to the admin's home merchant when set.
  STransaction.buildDashboardAuthTransaction (DT.DashboardTwoFactorAdminReset :: DT.Endpoint DashAuth.DashboardActionType) requestor.id requestor.merchantId >>= QT.createDashboardTransaction
  logInfo $ "2FA admin reset: requestor=" <> requestor.id.getId <> " target=" <> target.id.getId
  pure $ TwoFaAdminResetRes "2FA reset for target user. They must re-enroll on next login."
