{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Tools.Utils where

import Crypto.OTP (OTP, defaultTOTPParams, totp)
import Crypto.Random (getRandomBytes)
import Data.ByteString.Base32 as Base32
import qualified Data.Text.Encoding as T
import qualified Data.Time.Clock.POSIX as DT
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Id

genTOTP :: Text -> IO OTP
genTOTP secretKey = do
  time <- DT.getPOSIXTime
  let secretKeyBytes = either (error . show) identity (Base32.decodeBase32 (T.encodeUtf8 secretKey))
  pure (totp defaultTOTPParams secretKeyBytes (floor time))

generateSecretKey :: IO Text
generateSecretKey = do
  randomBytes <- getRandomBytes 20
  pure $ encodeBase32 randomBytes

generateAuthenticatorURI :: Text -> Text -> ShortId DMerchant.Merchant -> Text
generateAuthenticatorURI secretKey user issuer = "otpauth://totp/" <> issuer.getShortId <> "?secret=" <> secretKey <> "&user=" <> user

getMobileNumberOtpKey :: Text -> Text -> Text
getMobileNumberOtpKey mobileCountryCode mobileNumber =
  let phoneNumber = mobileCountryCode <> mobileNumber
   in "UnifiedDashboard:MobileNumberOtp:mobileNumber-" <> phoneNumber
