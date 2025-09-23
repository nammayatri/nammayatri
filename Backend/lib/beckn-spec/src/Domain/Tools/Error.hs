{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Tools.Error where

import EulerHS.Prelude
import Kernel.Types.Error.BaseError.HTTPError

data CancelError = ActiveCancellationOngoing
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CancelError

instance IsBaseError CancelError where
  toMessage ActiveCancellationOngoing = Just "Active cancellation ongoing."

instance IsHTTPError CancelError where
  toErrorCode = \case
    ActiveCancellationOngoing -> "ACTIVE_CANCELLATION_ONGOING"
  toHttpCode = \case
    ActiveCancellationOngoing -> E400

instance IsAPIError CancelError
