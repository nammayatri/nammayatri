{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Rating where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Rating = Rating
  { createdAt :: Kernel.Prelude.UTCTime,
    feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Rating.Rating,
    mediaId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    ratingValue :: Kernel.Prelude.Int,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime,
    wasOfferedAssistance :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
