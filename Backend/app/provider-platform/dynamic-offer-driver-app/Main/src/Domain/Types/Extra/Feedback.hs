{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.Feedback where

import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Types.Id

-- Extra code goes here --

data FeedbackBadge = FeedbackBadge
  { id :: Id FeedbackBadge,
    driverId :: Id Person,
    badge :: Text,
    badgeCount :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
