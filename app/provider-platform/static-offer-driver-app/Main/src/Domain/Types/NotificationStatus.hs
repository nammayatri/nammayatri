{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.NotificationStatus where

import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import Domain.Types.Person (Driver)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data AnswerStatus = NOTIFIED | REJECTED | IGNORED | ACCEPTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data NotificationStatus = NotificationStatus
  { id :: Id NotificationStatus,
    bookingId :: Id DRB.Booking,
    driverId :: Id Driver,
    status :: AnswerStatus,
    expiresAt :: UTCTime
  }
  deriving (Generic)
