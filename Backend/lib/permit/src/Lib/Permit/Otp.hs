-- | One-time passwords and rate limits, kept in Redis.
module Lib.Permit.Otp
  ( PermitHost (..),
    OtpSlot (..),
    issueOtp,
    checkOtp,
    discardOtp,
    limitHits,
    sha256Hex,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common

-- | What the hosting service provides for sending OTPs.
data PermitHost m = PermitHost
  { -- | Mobile country code, mobile number and code.
    sendOtpSms :: Text -> Text -> Text -> m (),
    -- | Set only by local or test configuration. When present no SMS is sent.
    fakeOtp :: Maybe Text
  }

data StoredOtp = StoredOtp
  { codeHash :: Text,
    -- | Ties an OTP to one thing, such as one exact version of an application.
    context :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype OtpSlot = OtpSlot Text

otpTtlSeconds, maxOtpAttempts :: Int
otpTtlSeconds = 300
maxOtpAttempts = 5

otpKey, attemptsKey :: OtpSlot -> Text
otpKey (OtpSlot slot) = "permit:otp:" <> slot
attemptsKey (OtpSlot slot) = "permit:otpAttempts:" <> slot

sha256Hex :: Text -> Text
sha256Hex = show . hashText
  where
    hashText :: Text -> Digest SHA256
    hashText = hash . TE.encodeUtf8

-- | Fails once 'maxHits' calls have been made for the key within the window.
limitHits :: (CacheFlow m r) => Text -> Int -> Int -> m ()
limitHits key maxHits windowSeconds = do
  let redisKey = "permit:hits:" <> key
  hits <- Redis.incr redisKey
  when (hits == 1) $ Redis.expire redisKey windowSeconds
  when (hits > fromIntegral maxHits) $ throwError $ InvalidRequest "Too many attempts. Try again in a few minutes."

issueOtp :: (CacheFlow m r, MonadFlow m) => PermitHost m -> OtpSlot -> Text -> Text -> Text -> m ()
issueOtp host slot otpContext countryCode mobile = do
  code <- maybe generateOTPCode pure host.fakeOtp
  Redis.setExp (otpKey slot) StoredOtp {codeHash = sha256Hex code, context = otpContext} otpTtlSeconds
  Redis.del (attemptsKey slot)
  when (isNothing host.fakeOtp) $ host.sendOtpSms countryCode mobile code

-- | Throws unless the code matches. Every try counts, right or wrong.
checkOtp :: (CacheFlow m r, MonadFlow m) => OtpSlot -> Text -> Text -> m ()
checkOtp slot otpContext code = do
  (stored :: StoredOtp) <- Redis.safeGet (otpKey slot) >>= fromMaybeM (InvalidRequest "OTP has expired. Request a new one.")
  attempts <- Redis.incr (attemptsKey slot)
  when (attempts == 1) $ Redis.expire (attemptsKey slot) otpTtlSeconds
  when (attempts > fromIntegral maxOtpAttempts) $ throwError $ InvalidRequest "Too many wrong OTP entries. Request a new one."
  unless (stored.context == otpContext) $ throwError $ InvalidRequest "The application changed after the OTP was sent. Request a new one."
  unless (stored.codeHash == sha256Hex code) $ throwError $ InvalidRequest "Incorrect OTP."

discardOtp :: (CacheFlow m r) => OtpSlot -> m ()
discardOtp slot = do
  Redis.del (otpKey slot)
  Redis.del (attemptsKey slot)
