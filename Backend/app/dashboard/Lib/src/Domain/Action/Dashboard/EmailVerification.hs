module Domain.Action.Dashboard.EmailVerification
  ( EmailOtpSendReq (..),
    EmailOtpVerifyReq (..),
    sendEmailVerificationOtp,
    verifyEmailOtp,
    makeEmailOtpKey,
    makeEmailOtpHitsCountKey,
    makeEmailOtpVerifyHitsCountKey,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DTServer
import Kernel.External.Encryption (encrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.City (City)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QP
import Tools.Auth.Dashboard
import qualified Tools.InternalClient as InternalClient

data EmailOtpSendReq = EmailOtpSendReq
  { email :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data EmailOtpVerifyReq = EmailOtpVerifyReq
  { email :: Text,
    otp :: Text,
    requesteeId :: Maybe (Id DP.Person)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

validateEmailOtpSendReq :: Validate EmailOtpSendReq
validateEmailOtpSendReq EmailOtpSendReq {..} =
  sequenceA_
    [validateField "email" email P.email]

validateEmailOtpVerifyReq :: Validate EmailOtpVerifyReq
validateEmailOtpVerifyReq EmailOtpVerifyReq {..} =
  sequenceA_
    [ validateField "email" email P.email,
      validateField "otp" otp $ ExactLength 4 `And` star P.digit
    ]

sendEmailVerificationOtp ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["loginRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int]
  ) =>
  TokenInfo ->
  EmailOtpSendReq ->
  m APISuccess
sendEmailVerificationOtp tokenInfo req = do
  runRequestValidation validateEmailOtpSendReq req
  let email = T.toLower req.email
  loginRateLimitOptions <- asks (.loginRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeEmailOtpHitsCountKey tokenInfo.personId email) loginRateLimitOptions
  (merchant, routingCity) <- resolveRoutingMerchant tokenInfo.personId
  -- Route by the merchant's own short id, not serverNames: post-unification the
  -- BPP row (`X_PARTNER`) advertises APP_BACKEND too, so a serverNames test
  -- always chose BAP and handed rider-app a `_PARTNER` short id it does not
  -- have. The suffix is the reliable signal — `X_PARTNER` is driver-app's own
  -- short id, `X` is rider-app's — so the id we forward is always one the
  -- target knows. Mirrors Domain.Action.Dashboard.Registration.
  let callInternalSendEmailOTP =
        if "_PARTNER" `T.isSuffixOf` getShortId merchant.shortId
          then InternalClient.callBPPInternalSendEmailOTP
          else InternalClient.callBAPInternalSendEmailOTP
  emailRes <- callInternalSendEmailOTP (getShortId merchant.shortId) routingCity (InternalClient.SendEmailOTPReq {email = email})
  otpCode <- emailRes.otp & fromMaybeM (InternalError "OTP not returned from internal email service")
  envOtpTTL <- asks (.twoFaOtpTTLInSecs)
  let otpTTL = fromMaybe 300 envOtpTTL
  Redis.setExp (makeEmailOtpKey tokenInfo.personId email) otpCode otpTTL
  pure Success

verifyEmailOtp ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["twoFaMaxOtpVerifyAttempts" ::: Maybe Int]
  ) =>
  TokenInfo ->
  EmailOtpVerifyReq ->
  m APISuccess
verifyEmailOtp tokenInfo req = do
  runRequestValidation validateEmailOtpVerifyReq req
  (merchant, _routingCity) <- resolveRoutingMerchant tokenInfo.personId
  let requesteeId = fromMaybe tokenInfo.personId req.requesteeId
  envMaxAttempts <- asks (.twoFaMaxOtpVerifyAttempts)
  envOtpTTL <- asks (.twoFaOtpTTLInSecs)
  let maxAttempts = fromMaybe 5 envMaxAttempts
      otpTTL = fromMaybe 300 envOtpTTL
      email = T.toLower req.email
      key = makeEmailOtpKey tokenInfo.personId email
      attemptsKey = makeEmailOtpVerifyHitsCountKey tokenInfo.personId email
  attempts <- fromMaybe 0 <$> Redis.get @Int attemptsKey
  when (attempts >= maxAttempts) $ do
    Redis.del key
    Redis.del attemptsKey
    throwError (InvalidRequest "Too many OTP attempts. Please request a new OTP.")
  mbStored <- Redis.get @Text key
  storedOtp <- mbStored & fromMaybeM (InvalidRequest "OTP expired or not found")
  if storedOtp == req.otp
    then do
      mbExisting <- QP.findByEmail email
      whenJust mbExisting $ \existing ->
        when (existing.id /= requesteeId) $ do
          Redis.del key
          Redis.del attemptsKey
          throwError (InvalidRequest "Email already registered by another user")
      -- Same routing rule as sendEmailVerificationOtp above: the short id we
      -- forward must be one the target platform knows.
      let callInternalVerifyEmailUpdate =
            if "_PARTNER" `T.isSuffixOf` getShortId merchant.shortId
              then InternalClient.callBPPInternalVerifyEmailUpdate
              else InternalClient.callBAPInternalVerifyEmailUpdate
          updateReq = InternalClient.VerifyEmailUpdateReq {email = email, requesteeId = requesteeId.getId, requestorId = tokenInfo.personId.getId}
      void $ callInternalVerifyEmailUpdate (getShortId merchant.shortId) updateReq
      encEmail <- encrypt email
      QP.updatePersonEmail requesteeId encEmail
      Redis.del key
      Redis.del attemptsKey
      pure Success
    else do
      Redis.setExp attemptsKey (attempts + 1) otpTTL
      throwError (InvalidRequest "Invalid OTP")

-- | One token per person: email verification is about the caller's own email,
-- so the merchant used to route the internal OTP (BAP vs BPP) and its city come
-- from the person's home merchant, not a session the token no longer carries.
resolveRoutingMerchant ::
  (BeamFlow m r) =>
  Id DP.Person ->
  m (DMerchant.Merchant, City)
resolveRoutingMerchant personId = do
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  mId <- person.merchantId & fromMaybeM (InvalidRequest "Person has no home merchant to route email verification")
  merchant <- QMerchant.findById mId >>= fromMaybeM (MerchantDoesNotExist mId.getId)
  pure (merchant, merchant.defaultOperatingCity)

makeEmailOtpKey :: Id DP.Person -> Text -> Text
makeEmailOtpKey personId email = "Dashboard:EmailVerification:" <> personId.getId <> ":" <> email

makeEmailOtpHitsCountKey :: Id DP.Person -> Text -> Text
makeEmailOtpHitsCountKey personId email = "Dashboard:EmailVerification:" <> personId.getId <> ":" <> email <> ":hitsCount"

makeEmailOtpVerifyHitsCountKey :: Id DP.Person -> Text -> Text
makeEmailOtpVerifyHitsCountKey personId email = "Dashboard:EmailVerification:" <> personId.getId <> ":" <> email <> ":verifyHitsCount"
