{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}

module Safety.Tools.Error where

import EulerHS.Prelude
import Kernel.Types.Error.BaseError.HTTPError

data SosError
  = SosNotFound Text
  | SosAlreadyResolved Text
  | SosNotAuthorized Text
  | SosSafetySettingsNotFound Text
  | SosNoActionRequired Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SosError

instance IsBaseError SosError where
  toMessage = \case
    SosNotFound sosId -> Just $ "SOS with id " <> sosId <> " not found."
    SosAlreadyResolved sosId -> Just $ "SOS with id " <> sosId <> " is already resolved."
    SosNotAuthorized sosId -> Just $ "SOS with id " <> sosId <> " does not belong to the specified person."
    SosSafetySettingsNotFound personId -> Just $ "SafetySettings not found for personId: " <> personId
    SosNoActionRequired sosId -> Just $ "SOS with id " <> sosId <> " is already in LiveTracking state or request is a no-op."

instance IsHTTPError SosError where
  toErrorCode (SosNotFound _) = "SOS_NOT_FOUND"
  toErrorCode (SosAlreadyResolved _) = "SOS_ALREADY_RESOLVED"
  toErrorCode (SosNotAuthorized _) = "SOS_NOT_AUTHORIZED"
  toErrorCode (SosSafetySettingsNotFound _) = "SOS_SAFETY_SETTINGS_NOT_FOUND"
  toErrorCode (SosNoActionRequired _) = "SOS_NO_ACTION_REQUIRED"
  toHttpCode (SosNotFound _) = E400
  toHttpCode (SosAlreadyResolved _) = E400
  toHttpCode (SosNotAuthorized _) = E403
  toHttpCode (SosSafetySettingsNotFound _) = E400
  toHttpCode (SosNoActionRequired _) = E400

instance IsAPIError SosError
