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

data SearchError
  = SearchNotFound
  | SearchDoesNotExist
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchError

instance IsBaseError SearchError where
  toMessage = \case
    SearchDoesNotExist -> Just "No search matches passed data."
    _ -> Nothing

instance IsHTTPError SearchError where
  toErrorCode = \case
    SearchNotFound -> "SEARCH_NOT_FOUND"
    SearchDoesNotExist -> "SEARCH_DOES_NOT_EXIST"
  toHttpCode = \case
    SearchNotFound -> E500
    SearchDoesNotExist -> E400

instance IsAPIError SearchError

data BookingError
  = BookingNotFound
  | BookingDoesNotExist
  | BookingFieldNotPresent Text
  | BookingInvalidStatus Text
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
  toHttpCode = \case
    BookingNotFound -> E500
    BookingDoesNotExist -> E400
    BookingFieldNotPresent _ -> E500
    BookingInvalidStatus _ -> E400

instance IsAPIError BookingError

data ParkingLocationError
  = ParkingLocationNotFound
  | ParkingLocationDoesNotExist
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ParkingLocationError

instance IsBaseError ParkingLocationError where
  toMessage = \case
    ParkingLocationDoesNotExist -> Just "No parking location matches passed data."
    _ -> Nothing

instance IsHTTPError ParkingLocationError where
  toErrorCode = \case
    ParkingLocationNotFound -> "PARKING_LOCATION_NOT_FOUND"
    ParkingLocationDoesNotExist -> "PARKING_LOCATION_DOES_NOT_EXIST"
  toHttpCode = \case
    ParkingLocationNotFound -> E500
    ParkingLocationDoesNotExist -> E400

instance IsAPIError ParkingLocationError