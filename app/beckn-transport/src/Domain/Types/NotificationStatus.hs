{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.NotificationStatus where

import Beckn.Types.Id
import Data.Time (UTCTime)
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)
import Types.App

data AnswerStatus = NOTIFIED | REJECTED | IGNORED | ACCEPTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data NotificationStatus = NotificationStatus
  { id :: Id NotificationStatus,
    rideBookingId :: Id DRB.RideBooking,
    driverId :: Id Driver,
    status :: AnswerStatus,
    expiresAt :: UTCTime
  }
  deriving (Generic)
