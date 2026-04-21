{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Tools.Utils where

import Crypto.Hash.Algorithms (SHA1 (..))
import Crypto.OTP (ClockSkew (..), OTP, OTPDigits (..), TOTPParams, defaultTOTPParams, mkTOTPParams, totp, totpVerify)
import Crypto.Random (getRandomBytes)
import Data.ByteString.Base32 as Base32
import qualified Data.Text as T2
import qualified Data.Text.Encoding as T
import qualified Data.Time.Clock.POSIX as DT
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, logError)

genTOTP :: Text -> IO OTP
genTOTP secretKey = do
  time <- DT.getPOSIXTime
  let secretKeyBytes = either (error . show) identity (Base32.decodeBase32 (T.encodeUtf8 secretKey))
  pure (totp defaultTOTPParams secretKeyBytes (floor time))

-- | Map a small integer to cryptonite's 'ClockSkew' (maxes at 4 steps).
clockSkewFromInt :: Int -> ClockSkew
clockSkewFromInt n
  | n <= 0 = NoSkew
  | n == 1 = OneStep
  | n == 2 = TwoSteps
  | n == 3 = ThreeSteps
  | otherwise = FourSteps

buildTOTPParams :: Maybe Int -> Maybe Int -> TOTPParams SHA1
buildTOTPParams Nothing Nothing = defaultTOTPParams
buildTOTPParams mbStepSize mbSkew =
  let stepSize = maybe 30 fromIntegral mbStepSize
      skew = clockSkewFromInt (fromMaybe 2 mbSkew)
   in case mkTOTPParams SHA1 0 stepSize OTP6 skew of
        Right params -> params
        Left _ -> defaultTOTPParams

verifyTOTP :: MonadFlow m => Maybe Int -> Maybe Int -> Text -> Text -> m Bool
verifyTOTP mbStepSize mbSkew secretKey userOtp = do
  time <- liftIO DT.getPOSIXTime
  case Base32.decodeBase32 (T.encodeUtf8 secretKey) of
    Left err -> do
      logError $ "verifyTOTP: base32 decode failed for stored secret: " <> show err
      pure False
    Right secretKeyBytes -> do
      let params = buildTOTPParams mbStepSize mbSkew
      case readMaybe (T2.unpack (T2.strip userOtp)) :: Maybe OTP of
        Nothing -> pure False
        Just otpValue -> pure $ totpVerify params secretKeyBytes (floor time) otpValue

generateSecretKey :: IO Text
generateSecretKey = do
  randomBytes <- getRandomBytes 20
  pure $ encodeBase32 randomBytes

generateAuthenticatorURI :: Maybe Int -> Text -> Text -> ShortId DMerchant.Merchant -> Text
generateAuthenticatorURI mbStepSize secretKey user issuer =
  "otpauth://totp/" <> issuer.getShortId <> "?secret=" <> secretKey <> "&user=" <> user <> periodParam
  where
    periodParam = case mbStepSize of
      Just n | n > 0 -> "&period=" <> T2.pack (show n)
      _ -> ""
