-- | Citizen sign-in. Permit sessions live only in Redis under their own keys, so
-- a permit token is never accepted by the host's own token checks.
module Lib.Permit.Auth
  ( PermitSession (..),
    sendLoginOtp,
    verifyLoginOtp,
    authenticate,
    logout,
    isMobileNumber,
  )
where

import qualified Crypto.Random as Random
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Permit.Domain.Types.Common
import Lib.Permit.Domain.Types.PermitApplicant
import Lib.Permit.Otp
import Lib.Permit.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Permit.Storage.Queries.PermitApplicant as QApplicant
import qualified Lib.Permit.Storage.Queries.PermitApplicantExtra as QApplicantExtra

data PermitSession = PermitSession
  { applicantId :: Id PermitApplicant,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity
  }
  deriving (Generic, ToJSON, FromJSON)

sessionTtlSeconds :: Int
sessionTtlSeconds = 24 * 3600

sessionKey :: Text -> Text
sessionKey token = "permit:session:" <> sha256Hex token

isMobileNumber :: Text -> Bool
isMobileNumber mobile = T.length mobile == 10 && T.all Char.isDigit mobile

loginSlot :: Id Merchant -> Text -> Text -> OtpSlot
loginSlot merchant countryCode mobile = OtpSlot ("login:" <> merchant.getId <> ":" <> sha256Hex (countryCode <> mobile))

randomToken :: MonadIO m => m Text
randomToken = do
  bytes <- liftIO (Random.getRandomBytes 32 :: IO BS.ByteString)
  pure $ TE.decodeUtf8 (convertToBase Base64URLUnpadded bytes)

sendLoginOtp :: (CacheFlow m r, MonadFlow m) => PermitHost m -> Id Merchant -> Maybe Text -> Text -> Text -> m ()
sendLoginOtp host merchant mbClientAddress countryCode mobile = do
  unless (isMobileNumber mobile) $ throwError $ InvalidRequest "Enter a 10-digit mobile number."
  limitHits ("loginOtp:" <> merchant.getId <> ":" <> sha256Hex (countryCode <> mobile)) 5 600
  whenJust mbClientAddress $ \address -> limitHits ("loginOtpAddress:" <> sha256Hex address) 50 600
  issueOtp host (loginSlot merchant countryCode mobile) "" countryCode mobile

verifyLoginOtp ::
  (BeamFlow m r, EncFlow m r) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Text ->
  Text ->
  Text ->
  m Text
verifyLoginOtp merchant city countryCode mobile code = do
  let slot = loginSlot merchant countryCode mobile
  checkOtp slot "" code
  discardOtp slot
  mobileHash <- getDbHash mobile
  applicant <-
    QApplicantExtra.findByMobileHashAndMerchant mobileHash merchant >>= \case
      Just existing -> pure existing
      Nothing -> do
        newId <- generateGUID
        now <- getCurrentTime
        encryptedMobile <- encrypt mobile
        let created =
              PermitApplicant
                { id = newId,
                  mobileCountryCode = countryCode,
                  mobileNumber = encryptedMobile,
                  merchantId = merchant,
                  createdAt = now,
                  updatedAt = now
                }
        QApplicant.create created
        pure created
  token <- randomToken
  Redis.setExp (sessionKey token) PermitSession {applicantId = applicant.id, merchantId = merchant, merchantOperatingCityId = city} sessionTtlSeconds
  pure token

authenticate :: (CacheFlow m r, MonadFlow m) => Maybe Text -> m PermitSession
authenticate mbToken = do
  token <- mbToken & fromMaybeM (InvalidToken "missing")
  Redis.safeGet (sessionKey token) >>= fromMaybeM (InvalidToken "expired or unknown")

logout :: (CacheFlow m r, MonadFlow m) => Maybe Text -> m ()
logout = traverse_ (Redis.del . sessionKey)
