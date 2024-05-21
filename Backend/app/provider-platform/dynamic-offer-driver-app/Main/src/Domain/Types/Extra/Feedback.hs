{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.Feedback where

import Data.Aeson
import Domain.Types.Person (Person)
import Domain.Types.Ride (Ride)
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
