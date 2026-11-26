{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.OtpCountryRestriction
  ( validateSmsOtpCountryCode,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

-- | Rejects an OTP SMS to a mobile country code that is not in the city's allowlist
-- (@smsOtpAllowedCountryCodes@ on RiderConfig / TransporterConfig). 'Nothing' means no restriction.
-- Only for the SMS channel: WhatsApp and email OTPs are not restricted, so callers must not
-- call this for them. A leading @+@ and surrounding spaces are ignored on both sides.
validateSmsOtpCountryCode :: (MonadThrow m, Log m) => Maybe [Text] -> Text -> m ()
validateSmsOtpCountryCode mbAllowedCountryCodes countryCode =
  whenJust mbAllowedCountryCodes $ \allowedCountryCodes ->
    unless (normalize countryCode `elem` map normalize allowedCountryCodes) $
      throwError $ InvalidRequest "SMS OTP is not supported for this mobile country code"
  where
    normalize = T.dropWhile (== '+') . T.strip
