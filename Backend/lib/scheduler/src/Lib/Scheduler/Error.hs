{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data JobDecodeError
  = InvalidJobType Text
  | InvalidJobData Text
  deriving (Show, IsBecknAPIError, Typeable)

instanceExceptionWithParent 'HTTPException ''JobDecodeError

instance IsBaseError JobDecodeError where
  toMessage = \case
    InvalidJobType invalidType -> Just $ "Invalid job type: " <> invalidType
    InvalidJobData invalidData -> Just $ "Invalid job data: " <> invalidData

instance IsHTTPError JobDecodeError where
  toErrorCode = \case
    InvalidJobType _ -> "INVALID_JOB_TYPE"
    InvalidJobData _ -> "INVALID_JOB_DATA"
  toHttpCode _ = E500

instance IsAPIError JobDecodeError
