{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.BookingStatus where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data BookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | REALLOCATED
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''BookingStatus)
