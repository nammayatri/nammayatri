{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Rating where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Rating = Rating
  { id :: Kernel.Types.Id.Id Domain.Types.Rating.Rating,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    ratingValue :: Kernel.Prelude.Int,
    feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    wasOfferedAssistance :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
