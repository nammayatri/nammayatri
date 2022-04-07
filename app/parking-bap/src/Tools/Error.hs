{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

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
  = SearchNotFound Text
  | SearchDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchError

instance IsBaseError SearchError where
  toMessage = \case
    SearchNotFound searchId -> Just $ "Search with searchId \"" <> show searchId <> "\" not found."
    SearchDoesNotExist searchId -> Just $ "No search matches passed data \"" <> show searchId <> "\" not exist."

instance IsHTTPError SearchError where
  toErrorCode = \case
    SearchNotFound _ -> "SEARCH_NOT_FOUND"
    SearchDoesNotExist _ -> "SEARCH_DOES_NOT_EXIST"
  toHttpCode = \case
    SearchNotFound _ -> E500
    SearchDoesNotExist _ -> E400

instance IsAPIError SearchError

data BookingError
  = BookingNotFound Text
  | BookingDoesNotExist Text
  | BookingFieldNotPresent Text
  | BookingInvalidStatus Text
  | BookingBppOrderIdNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''BookingError

instance IsBaseError BookingError where
  toMessage = \case
    BookingNotFound bookingId -> Just $ "Booking with bookingId \"" <> show bookingId <> "\" not found."
    BookingDoesNotExist bookingId -> Just $ "No booking matches passed data \"" <> show bookingId <> "\" not exist."
    BookingFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this booking."
    BookingInvalidStatus msg -> Just $ "Attempted to do some action in wrong booking status. " <> msg
    _ -> Nothing

instance IsHTTPError BookingError where
  toErrorCode = \case
    BookingNotFound _ -> "BOOKING_NOT_FOUND"
    BookingDoesNotExist _ -> "BOOKING_DOES_NOT_EXIST"
    BookingFieldNotPresent _ -> "BOOKING_FIELD_NOT_PRESENT"
    BookingInvalidStatus _ -> "BOOKING_INVALID_STATUS"
    BookingBppOrderIdNotFound -> "BOOKING_BPP_ORDER_ID_NOT_FOUND"
  toHttpCode = \case
    BookingNotFound _ -> E500
    BookingDoesNotExist _ -> E400
    BookingFieldNotPresent _ -> E500
    BookingInvalidStatus _ -> E400
    BookingBppOrderIdNotFound -> E500

instance IsAPIError BookingError

data ParkingLocationError
  = ParkingLocationNotFound Text
  | ParkingLocationDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ParkingLocationError

instance IsBaseError ParkingLocationError where
  toMessage = \case
    ParkingLocationNotFound locationId -> Just $ "ParkingLocation with locationId \"" <> show locationId <> "\" not found."
    ParkingLocationDoesNotExist locationId -> Just $ "No parking location matches passed data \"" <> show locationId <> "\" not exist."

instance IsHTTPError ParkingLocationError where
  toErrorCode = \case
    ParkingLocationNotFound _ -> "PARKING_LOCATION_NOT_FOUND"
    ParkingLocationDoesNotExist _ -> "PARKING_LOCATION_DOES_NOT_EXIST"
  toHttpCode = \case
    ParkingLocationNotFound _ -> E500
    ParkingLocationDoesNotExist _ -> E400

instance IsAPIError ParkingLocationError

data PaymentDetailsError
  = PaymentDetailsNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PaymentDetailsError

instance IsBaseError PaymentDetailsError where
  toMessage = \case
    PaymentDetailsNotFound paymentDetailId -> Just $ "Payment with paymentId \"" <> show paymentDetailId <> "\" not found."

instance IsHTTPError PaymentDetailsError where
  toErrorCode = \case
    PaymentDetailsNotFound _ -> "PAYMENT_DETAILS_NOT_FOUND"
  toHttpCode = \case
    PaymentDetailsNotFound _ -> E400

instance IsAPIError PaymentDetailsError
