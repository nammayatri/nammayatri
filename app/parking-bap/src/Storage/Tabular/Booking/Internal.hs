{-# LANGUAGE TemplateHaskell #-}

module Storage.Tabular.Booking.Internal where

import Beckn.Prelude
import Database.Persist.TH

data BookingStatus = NEW | AWAITING_PAYMENT | CONFIRMED | CANCELLED
  deriving (Generic, Show, Read)

derivePersistField "BookingStatus"
