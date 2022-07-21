{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

data PaymentDetailsError
  = PaymentDetailsNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PaymentDetailsError

instance IsBaseError PaymentDetailsError

toMessage = \case
  PaymentDetailsNotFound paymentId -> Just $ ("Booking with bookingId \"" :: [Char]) <> show paymentId <> "\" not found. "

instance IsHTTPError PaymentDetailsError where
  toErrorCode = \case
    PaymentDetailsNotFound _ -> "PAYMENT_DETAILS_NOT_FOUND"

toHttpCode = \case
  PaymentDetailsNotFound _ -> E400

instance IsAPIError PaymentDetailsError
