{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Rating where

import Data.Aeson (eitherDecode)
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Rating = Rating
  { createdAt :: Kernel.Prelude.UTCTime,
    feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Rating.Rating,
    ratingValue :: Kernel.Prelude.Int,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime,
    wasOfferedAssistance :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
