{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Partner account linking: a partner (e.g. Ouru) gets its own access token for a rider,
-- after the rider enters an OTP on a page we host. Routes: spec/API/PartnerAccountLink.yaml
-- (JSON) and API.UI.PartnerAccountLinkPage (the HTML page).
--
--   1. The partner sends @{partner_code, user_mobile}@ and gets back
--      @{weblink, request_id}@. The OTP is sent to the number now.
--   2. The rider opens the weblink and enters the OTP.
--        * Existing rider: done.
--        * New rider: a signup form follows; submitting it creates the account.
--   3. We POST @{request_id, access_token}@ to the partner's webhook. The token is never
--      returned by any API call.
--
-- The token is an ordinary rider token (same TokenAuth and APIs as the app; bookings
-- show in the app and can be cancelled there) tagged with createdViaPartnerOrgId, which
-- is how the backend tells partner requests from app requests. The rider's person id
-- is never sent to the partner.
--
-- Pending state lives in Redis for the link's lifetime: no table, no migration.
module Domain.Action.UI.PartnerAccountLink
  ( postPartnerOrgAccountLink,
    getPartnerOrgAccountLinkPageInfo,
    postPartnerOrgAccountLinkPageResendOtp,
    postPartnerOrgAccountLinkPageVerifyOtp,
    postPartnerOrgAccountLinkPageSignup,
  )
where

import qualified API.Types.UI.PartnerAccountLink as API
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as A
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Domain.Types.PartnerOrganization
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude (parseBaseUrl)
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import Lib.ConfigPilot.Interface.Types (getConfig)
import Servant hiding (throwError)
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.OTP as SOTP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import Storage.ConfigPilot.Config.MerchantConfig (MerchantConfigDimensions (..))
import qualified Storage.Queries.PartnerOrganizationExtra as QPO
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Auth (authTokenCacheKey, verifyPartnerOrganization)
import Tools.Error

---------------------------------------------------------------------------------------
-- Pending state (Redis)

data LinkState = LinkState
  { requestId :: Text,
    partnerOrgId :: Id PartnerOrganization,
    merchantId :: Id DM.Merchant,
    mobileCountryCode :: Text,
    -- Plain number for sending the OTP; the key lives only as long as the link.
    mobileNumber :: Text,
    step :: API.PageStep,
    otp :: Maybe Text,
    otpSends :: Int,
    wrongAttempts :: Int,
    expiresAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- The page secret and the request id differ on purpose: the request id goes to the
-- partner's webhook and logs, and must not open the page.
linkKey :: Text -> Text
linkKey linkSecret = "BAP:PartnerAccountLink:link:" <> linkSecret

lockKey :: Text -> Text
lockKey linkSecret = "BAP:PartnerAccountLink:lock:" <> linkSecret

---------------------------------------------------------------------------------------
-- Partner call

-- | The partner's request carries the number alone; riders are Indian numbers.
defaultCountryCode :: Text
defaultCountryCode = "+91"

validateInitLinkReq :: Validate API.InitLinkReq
validateInitLinkReq req =
  sequenceA_ [validateField "user_mobile" req.user_mobile P.mobileNumber]

postPartnerOrgAccountLink :: API.InitLinkReq -> Flow API.InitLinkRes
postPartnerOrgAccountLink req = do
  runRequestValidation validateInitLinkReq req
  partnerOrg <- verifyPartnerOrganization req.partner_code
  checkRateLimit partnerOrg.orgId
  cfg <- getAccountLinkConfig partnerOrg.orgId
  now <- getCurrentTime
  requestId <- generateGUID
  linkSecret <- generateGUID
  st <-
    sendOtpFor
      cfg
      LinkState
        { requestId,
          partnerOrgId = partnerOrg.orgId,
          merchantId = partnerOrg.merchantId,
          mobileCountryCode = defaultCountryCode,
          mobileNumber = req.user_mobile,
          step = API.ENTER_OTP,
          otp = Nothing,
          otpSends = 0,
          wrongAttempts = 0,
          expiresAt = addUTCTime (fromIntegral cfg.linkExpiryInSec) now
        }
  Redis.setExp (linkKey linkSecret) st cfg.linkExpiryInSec
  pure API.InitLinkRes {weblink = cfg.pageBaseUrl <> "/partnerOrg/accountLink/page/" <> linkSecret, request_id = requestId}

-- Same per-partner limit the FRFS partner routes use (the partner's RATE_LIMIT config).
checkRateLimit :: Id PartnerOrganization -> Flow ()
checkRateLimit orgId = do
  pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType orgId DPOC.RATE_LIMIT >>= fromMaybeM (PartnerOrgConfigNotFound orgId.getId $ show DPOC.RATE_LIMIT)
  rateLimitPOCfg <- DPOC.getRateLimitConfig pOrgCfg.config
  checkSlidingWindowLimitWithOptions ("BAP:PartnerAccountLink:PartnerOrgId:" <> orgId.getId <> ":hitsCount") rateLimitPOCfg.rateLimitOptions

---------------------------------------------------------------------------------------
-- Hosted page calls (the link secret in the path is the only credential)

getPartnerOrgAccountLinkPageInfo :: Text -> Flow API.LinkPageInfo
getPartnerOrgAccountLinkPageInfo linkSecret = getLiveState linkSecret >>= toPageInfo

postPartnerOrgAccountLinkPageResendOtp :: Text -> Flow APISuccess
postPartnerOrgAccountLinkPageResendOtp linkSecret = withLinkLock linkSecret $ do
  st <- getLiveState linkSecret
  cfg <- getAccountLinkConfig st.partnerOrgId
  unless (st.step == API.ENTER_OTP) $ throwError AccountLinkWrongStep
  sendOtpFor cfg st >>= putState linkSecret
  pure Success

validateVerifyOtpReq :: Validate API.VerifyOtpReq
validateVerifyOtpReq req =
  sequenceA_ [validateField "otp" req.otp $ ExactLength 4 `And` star P.digit]

-- | Existing rider: links now. New rider: moves the page to the signup form.
postPartnerOrgAccountLinkPageVerifyOtp :: Text -> API.VerifyOtpReq -> Flow API.LinkPageInfo
postPartnerOrgAccountLinkPageVerifyOtp linkSecret req = withLinkLock linkSecret $ do
  runRequestValidation validateVerifyOtpReq req
  st <- getLiveState linkSecret
  cfg <- getAccountLinkConfig st.partnerOrgId
  unless (st.step == API.ENTER_OTP) $ throwError AccountLinkWrongStep
  sentOtp <- st.otp & fromMaybeM AccountLinkWrongStep
  unless (sentOtp == req.otp) $ do
    let wrong = st.wrongAttempts + 1
    if wrong >= cfg.maxOtpAttempts
      then do
        -- The link dies so the OTP cannot be guessed; the partner starts a new one.
        void $ Redis.del (linkKey linkSecret)
        throwError AccountLinkOtpAttemptsExceeded
      else do
        putState linkSecret (st :: LinkState) {wrongAttempts = wrong}
        throwError InvalidAuthData
  mbPerson <- findPerson st
  st' <- case mbPerson of
    Just person -> linkPerson cfg st person
    Nothing -> pure (st :: LinkState) {step = API.SIGNUP, otp = Nothing}
  putState linkSecret st'
  toPageInfo st'

validateSignupReq :: Validate API.SignupReq
validateSignupReq req =
  sequenceA_ [validateField "firstName" req.firstName $ MinLength 1 `And` MaxLength 50]

-- | New rider's form, reached only after the OTP proved the number.
postPartnerOrgAccountLinkPageSignup :: Text -> API.SignupReq -> Flow API.LinkPageInfo
postPartnerOrgAccountLinkPageSignup linkSecret req = withLinkLock linkSecret $ do
  runRequestValidation validateSignupReq req
  st <- getLiveState linkSecret
  cfg <- getAccountLinkConfig st.partnerOrgId
  unless (st.step == API.SIGNUP) $ throwError AccountLinkWrongStep
  merchant <- CQM.findById st.merchantId >>= fromMaybeM (MerchantNotFound st.merchantId.getId)
  -- Looked up again: the rider may have signed up in the app meanwhile, and a second
  -- account must never be created.
  person <-
    findPerson st
      >>= maybe (DReg.createPerson (buildAuthReq merchant st req) SP.MOBILENUMBER Nothing Nothing Nothing Nothing Nothing Nothing Nothing merchant (Just st.partnerOrgId) Nothing) pure
  st' <- linkPerson cfg st person
  putState linkSecret st'
  toPageInfo st'

---------------------------------------------------------------------------------------
-- Linking: token + webhook

linkPerson :: DPOC.AccountLinkConfig -> LinkState -> SP.Person -> Flow LinkState
linkPerson cfg st person = do
  when person.blocked $ throwError AccountLinkPersonBlocked
  regToken <- buildPartnerToken st cfg person
  RegistrationToken.create regToken
  -- One live token per rider per partner: a new link replaces the old token.
  oldTokens <- RegistrationToken.findAllByPersonIdAndPartnerOrgId person.id st.partnerOrgId
  for_ (filter ((/= regToken.id) . (.id)) oldTokens) $ \t -> Redis.del (authTokenCacheKey t.token)
  RegistrationToken.deleteByPersonIdAndPartnerOrgIdExceptNew person.id st.partnerOrgId regToken.id
  fork "PartnerAccountLink:webhook" $
    sendWebhook st cfg WebhookReq {request_id = st.requestId, access_token = regToken.token}
  pure (st :: LinkState) {step = API.DONE, otp = Nothing}

buildPartnerToken :: LinkState -> DPOC.AccountLinkConfig -> SP.Person -> Flow SR.RegistrationToken
buildPartnerToken st cfg person = do
  rtId <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  smsCfg <- asks (.smsCfg)
  pure
    SR.RegistrationToken
      { id = Id rtId,
        token,
        attempts = 0,
        authMedium = SR.PARTNER_ORG,
        -- OTP, not DIRECT: the rider proved the number. DIRECT is the API-key-only path.
        authType = SR.OTP,
        authValueHash = "",
        verified = True,
        authExpiry = smsCfg.sessionConfig.authExpiry,
        tokenExpiry = cfg.tokenExpiryInDays,
        entityId = person.id.getId,
        merchantId = person.merchantId.getId,
        merchantOperatingCityId = person.merchantOperatingCityId.getId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        createdViaPartnerOrgId = Just st.partnerOrgId
      }

-- | Body POSTed to the partner's webhook; field names are the ones agreed with the partner.
data WebhookReq = WebhookReq
  { request_id :: Text,
    access_token :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type WebhookAPI =
  Header "x-ny-timestamp" Text
    :> Header "x-ny-signature" Text
    :> ReqBody '[JSON] WebhookReq
    :> Post '[JSON] APISuccess

webhookAPI :: Proxy WebhookAPI
webhookAPI = Proxy

-- | x-ny-signature = hex(HMAC-SHA256(partner_code, timestamp <> "." <> body)), so the
-- partner can reject a delivery that did not come from us. The URL comes from our
-- config, never from a request, so a leaked partner code cannot send a token anywhere
-- else. Three tries; the partner answers 200 {"result":"Success"}.
sendWebhook :: LinkState -> DPOC.AccountLinkConfig -> WebhookReq -> Flow ()
sendWebhook st cfg body = do
  partnerOrg <- B.runInReplica (QPO.findByOrgId st.partnerOrgId) >>= fromMaybeM (InternalError "partner org missing")
  secret <- decrypt partnerOrg.apiKey
  url <- parseBaseUrl cfg.webhookUrl
  now <- getCurrentTime
  let timestamp = show (floor (utcTimeToPOSIXSeconds now) :: Integer)
      signature = signBody secret timestamp (A.encode body)
      eulerClient = ET.client webhookAPI (Just timestamp) (Just signature) body
      attempt :: Int -> Flow ()
      attempt n = do
        res <- callAPI url eulerClient "PartnerAccountLinkWebhook" webhookAPI
        case res of
          Right _ -> logInfo $ "PartnerAccountLink: webhook delivered, requestId:" <> st.requestId
          Left err
            | n < 3 -> do
              logWarning $ "PartnerAccountLink: webhook try " <> show n <> " failed, requestId:" <> st.requestId <> " err:" <> show err
              liftIO $ threadDelay (n * 1000000)
              attempt (n + 1)
            | otherwise -> logError $ "PartnerAccountLink: webhook not delivered after 3 tries, requestId:" <> st.requestId
  attempt 1

signBody :: Text -> Text -> BSL.ByteString -> Text
signBody secret timestamp body =
  let mac = hmac (TE.encodeUtf8 secret) (TE.encodeUtf8 timestamp <> "." <> BSL.toStrict body) :: HMAC SHA256
   in TE.decodeUtf8 (BAE.convertToBase BAE.Base16 (hmacGetDigest mac))

---------------------------------------------------------------------------------------
-- Helpers

sendOtpFor :: DPOC.AccountLinkConfig -> LinkState -> Flow LinkState
sendOtpFor cfg st = do
  when (st.otpSends >= cfg.maxOtpSends) $ throwError AccountLinkOtpSendLimitExceeded
  merchant <- CQM.findById st.merchantId >>= fromMaybeM (MerchantNotFound st.merchantId.getId)
  mocId <- defaultCityId merchant
  -- The app's per-number OTP limits apply here too.
  merchantConfigs <- getConfig (MerchantConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing
  phoneHash <- phoneHashText st.mobileNumber
  mbResetSeconds <- SMC.checkAuthLimitExceededByPhone merchantConfigs phoneHash
  whenJust mbResetSeconds $ \resetSeconds -> throwError (HitsLimitError resetSeconds)
  SMC.updateCustomerAuthCountersByPhone phoneHash merchantConfigs
  smsCfg <- asks (.smsCfg)
  -- A resend repeats the OTP already sent, as the app's resend does.
  otpCode <- maybe (maybe generateOTPCode pure (show <$> useFakeSms smsCfg)) pure st.otp
  -- sendOTP uses the person id only as a log tag; a new rider has no person yet.
  SOTP.sendOTP SOTP.SMS otpCode (Id $ "account-link:" <> st.requestId) merchant.id mocId (Just st.mobileCountryCode) (Just st.mobileNumber) Nothing Nothing Nothing
  pure (st :: LinkState) {otp = Just otpCode, otpSends = st.otpSends + 1}

toPageInfo :: LinkState -> Flow API.LinkPageInfo
toPageInfo st = do
  cfg <- getAccountLinkConfig st.partnerOrgId
  pure API.LinkPageInfo {step = st.step, partnerName = cfg.partnerDisplayName, maskedMobileNumber = maskMobileNumber st.mobileNumber}

getAccountLinkConfig :: Id PartnerOrganization -> Flow DPOC.AccountLinkConfig
getAccountLinkConfig orgId = do
  pOrgCfg <- CQPOC.findByIdAndCfgType orgId DPOC.ACCOUNT_LINK >>= fromMaybeM (PartnerOrgConfigNotFound orgId.getId $ show DPOC.ACCOUNT_LINK)
  DPOC.getAccountLinkConfig pOrgCfg.config

getLiveState :: Text -> Flow LinkState
getLiveState linkSecret = do
  st <- Redis.safeGet (linkKey linkSecret) >>= fromMaybeM AccountLinkNotFound
  now <- getCurrentTime
  when (now >= st.expiresAt) $ throwError AccountLinkNotFound
  pure st

-- | Writes the state back without extending the link's lifetime.
putState :: Text -> LinkState -> Flow ()
putState linkSecret st = do
  now <- getCurrentTime
  let remaining = max 1 (round (diffUTCTime st.expiresAt now) :: Int)
  Redis.setExp (linkKey linkSecret) st remaining

-- | One page call at a time per link, so a double tap cannot mint two tokens or slip
-- a wrong guess past the attempt counter.
withLinkLock :: Text -> Flow a -> Flow a
withLinkLock linkSecret action = do
  acquired <- Redis.tryLockRedis (lockKey linkSecret) 10
  unless acquired $ throwError AccountLinkBusy
  action `finally` Redis.unlockRedis (lockKey linkSecret)

findPerson :: LinkState -> Flow (Maybe SP.Person)
findPerson st = do
  mobileNumberHash <- getDbHash st.mobileNumber
  Person.findByRoleAndMobileNumberAndMerchantId SP.USER st.mobileCountryCode mobileNumberHash st.merchantId

defaultCityId :: DM.Merchant -> Flow (Id DMOC.MerchantOperatingCity)
defaultCityId merchant =
  CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
    >>= fmap (.id) . fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchant.id.getId <> " ,city: " <> show merchant.defaultCity)

-- Same non-PII hex key the app's phone counters use (see Registration.auth).
phoneHashText :: Text -> Flow Text
phoneHashText mobileNumber = do
  h <- getDbHash mobileNumber
  case A.toJSON h of
    A.String t -> pure t
    _ -> throwError (InternalError "Unexpected phone hash encoding")

maskMobileNumber :: Text -> Text
maskMobileNumber n = if T.length n > 4 then T.take 2 n <> "XXXXXX" <> T.takeEnd 2 n else "XXXXXX"

buildAuthReq :: DM.Merchant -> LinkState -> API.SignupReq -> DReg.AuthReq
buildAuthReq merchant st req =
  DReg.AuthReq
    { mobileNumber = Just st.mobileNumber,
      mobileCountryCode = Just st.mobileCountryCode,
      identifierType = Just SP.MOBILENUMBER,
      merchantId = merchant.shortId,
      deviceToken = Nothing,
      notificationToken = Nothing,
      whatsappNotificationEnroll = Nothing,
      firstName = Just req.firstName,
      middleName = Nothing,
      lastName = req.lastName,
      email = Nothing,
      businessEmail = Nothing,
      language = Nothing,
      gender = Nothing,
      otpChannel = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing,
      enableOtpLessRide = Nothing,
      allowBlockedUserLogin = Nothing,
      isOperatorReq = Nothing,
      reuseToken = Nothing,
      operatorBadgeToken = Nothing,
      deviceSerialNumber = Nothing,
      vehicleType = Nothing
    }
