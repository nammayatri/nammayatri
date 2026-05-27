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
import Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DTServer
import Kernel.External.Encryption (encrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess (Success))
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
import Tools.Auth
import qualified Tools.InternalClient as InternalClient

data EmailOtpSendReq = EmailOtpSendReq
  { email :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data EmailOtpVerifyReq = EmailOtpVerifyReq
  { email :: Text,
    otp :: Text
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
    HasFlowEnv m r '["sendEmailRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  TokenInfo ->
  EmailOtpSendReq ->
  m APISuccess
sendEmailVerificationOtp tokenInfo req = do
  runRequestValidation validateEmailOtpSendReq req
  let email = T.toLower req.email
  sendEmailRateLimitOptions <- asks (.sendEmailRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeEmailOtpHitsCountKey tokenInfo.personId email) sendEmailRateLimitOptions
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantDoesNotExist tokenInfo.merchantId.getId)
  let callInternalSendEmailOTP =
        if DTServer.APP_BACKEND `elem` merchant.serverNames
          then InternalClient.callBAPInternalSendEmailOTP
          else InternalClient.callBPPInternalSendEmailOTP
  emailRes <- callInternalSendEmailOTP (getShortId merchant.shortId) tokenInfo.city (InternalClient.SendEmailOTPReq {email = email})
  otpCode <- emailRes.otp & fromMaybeM (InternalError "OTP not returned from internal email service")
  let otpTTL = fromMaybe 300 merchant.emailOtpTTLInSecs
  Redis.setExp (makeEmailOtpKey tokenInfo.personId email) otpCode otpTTL
  pure Success

verifyEmailOtp ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  TokenInfo ->
  EmailOtpVerifyReq ->
  m APISuccess
verifyEmailOtp tokenInfo req = do
  runRequestValidation validateEmailOtpVerifyReq req
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantDoesNotExist tokenInfo.merchantId.getId)
  let maxAttempts = fromMaybe 5 merchant.emailMaxOtpVerifyAttempts
      otpTTL = fromMaybe 300 merchant.emailOtpTTLInSecs
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
      person <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
      mbExisting <- QP.findByEmail email
      whenJust mbExisting $ \existing ->
        when (existing.id /= tokenInfo.personId) $ do
          Redis.del key
          Redis.del attemptsKey
          throwError (InvalidRequest "Email already registered by another user")
      let callInternalVerifyEmailUpdate =
            if DTServer.APP_BACKEND `elem` merchant.serverNames
              then InternalClient.callBAPInternalVerifyEmailUpdate
              else InternalClient.callBPPInternalVerifyEmailUpdate
          updateReq = InternalClient.VerifyEmailUpdateReq {email = email, personId = tokenInfo.personId.getId}
      void $ callInternalVerifyEmailUpdate (getShortId merchant.shortId) updateReq
      encEmail <- encrypt email
      QP.updatePersonEmail person.id encEmail
      Redis.del key
      Redis.del attemptsKey
      pure Success
    else do
      Redis.setExp attemptsKey (attempts + 1) otpTTL
      throwError (InvalidRequest "Invalid OTP")

makeEmailOtpKey :: Id DP.Person -> Text -> Text
makeEmailOtpKey personId email = "Dashboard:EmailVerification:" <> personId.getId <> ":" <> email

makeEmailOtpHitsCountKey :: Id DP.Person -> Text -> Text
makeEmailOtpHitsCountKey personId email = "Dashboard:EmailVerification:" <> personId.getId <> ":" <> email <> ":hitsCount"

makeEmailOtpVerifyHitsCountKey :: Id DP.Person -> Text -> Text
makeEmailOtpVerifyHitsCountKey personId email = "Dashboard:EmailVerification:" <> personId.getId <> ":" <> email <> ":verifyHitsCount"
