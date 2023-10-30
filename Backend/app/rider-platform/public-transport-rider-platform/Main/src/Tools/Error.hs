{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Tools.Error
  ( module Tools.Error,
    module Reexport,
  )
where

import Kernel.Prelude
import Kernel.Types.Error as Reexport
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data TransportStationError
  = TransportStationNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TransportStationError

instance IsBaseError TransportStationError

instance IsHTTPError TransportStationError where
  toErrorCode TransportStationNotFound = "TRANSPORT_STATION_NOT_FOUND"
  toHttpCode TransportStationNotFound = E500

instance IsAPIError TransportStationError

newtype PaymentDetailsError
  = PaymentDetailsNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PaymentDetailsError

instance IsBaseError PaymentDetailsError where
  toMessage = \case
    PaymentDetailsNotFound bookingId -> Just $ "Payment details with bookingId \"" <> show bookingId <> "\" not found. "

instance IsHTTPError PaymentDetailsError where
  toErrorCode = \case
    PaymentDetailsNotFound _ -> "PAYMENT_DETAILS_NOT_FOUND"
  toHttpCode = \case
    PaymentDetailsNotFound _ -> E500

instance IsAPIError PaymentDetailsError
