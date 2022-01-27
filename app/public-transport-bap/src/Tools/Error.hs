{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Tools.Error
  ( module Tools.Error,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Error as Reexport
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError

data TransportStationError
  = TransportStationNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TransportStationError

instance IsBaseError TransportStationError

instance IsHTTPError TransportStationError where
  toErrorCode TransportStationNotFound = "TRANSPORT_STATION_NOT_FOUND"
  toHttpCode TransportStationNotFound = E500

instance IsAPIError TransportStationError

data BookingError
  = BookingNotFound
  | BookingDoesNotExist
  | BookingFieldNotPresent Text
  | BookingInvalidStatus Text
  | BookingBppOrderIdNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''BookingError

instance IsBaseError BookingError where
  toMessage = \case
    BookingDoesNotExist -> Just "No booking matches passed data."
    BookingFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this booking."
    BookingInvalidStatus msg -> Just $ "Attempted to do some action in wrong booking status. " <> msg
    _ -> Nothing

instance IsHTTPError BookingError where
  toErrorCode = \case
    BookingNotFound -> "BOOKING_NOT_FOUND"
    BookingDoesNotExist -> "BOOKING_DOES_NOT_EXIST"
    BookingFieldNotPresent _ -> "BOOKING_FIELD_NOT_PRESENT"
    BookingInvalidStatus _ -> "BOOKING_INVALID_STATUS"
    BookingBppOrderIdNotFound -> "BOOKING_BPP_ORDER_ID_NOT_FOUND"
  toHttpCode = \case
    BookingNotFound -> E500
    BookingDoesNotExist -> E400
    BookingFieldNotPresent _ -> E500
    BookingInvalidStatus _ -> E400
    BookingBppOrderIdNotFound -> E500

instance IsAPIError BookingError
