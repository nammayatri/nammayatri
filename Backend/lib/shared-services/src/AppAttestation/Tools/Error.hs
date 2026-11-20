{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module AppAttestation.Tools.Error where

import AppAttestation.Domain.Types.Common (FallbackMethod)
import Data.Aeson (Value (Null), object, (.=))
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data AttestationError
  = AttestationRequired [FallbackMethod]
  | AttestationFailed
  | AttestationChallengeInvalid
  | AttestationKeyRejected Text
  | AttestationNotConfigured Text
  | AttestationCaptchaInvalid
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AttestationError

instance IsBaseError AttestationError where
  toMessage = \case
    AttestationRequired _ -> Just "This device could not be verified. Continue with one of the fallback methods."
    AttestationFailed -> Just "This device could not be verified."
    AttestationChallengeInvalid -> Just "Attestation challenge is missing, expired or already used."
    AttestationKeyRejected reason -> Just $ "App Attest key rejected: " <> reason
    AttestationNotConfigured merchantOperatingCityId -> Just $ "App attestation is not configured for merchantOperatingCityId: " <> merchantOperatingCityId
    AttestationCaptchaInvalid -> Just "CAPTCHA verification failed."

instance IsHTTPError AttestationError where
  toErrorCode = \case
    AttestationRequired _ -> "ATTESTATION_REQUIRED"
    AttestationFailed -> "ATTESTATION_FAILED"
    AttestationChallengeInvalid -> "ATTESTATION_CHALLENGE_INVALID"
    AttestationKeyRejected _ -> "ATTESTATION_KEY_REJECTED"
    AttestationNotConfigured _ -> "ATTESTATION_NOT_CONFIGURED"
    AttestationCaptchaInvalid -> "ATTESTATION_CAPTCHA_INVALID"
  toHttpCode = \case
    AttestationRequired _ -> E403
    AttestationFailed -> E403
    AttestationChallengeInvalid -> E400
    AttestationKeyRejected _ -> E400
    AttestationNotConfigured _ -> E400
    AttestationCaptchaInvalid -> E403

instance IsAPIError AttestationError where
  toPayload = \case
    AttestationRequired methods -> object ["fallbackMethods" .= methods]
    _ -> Null
