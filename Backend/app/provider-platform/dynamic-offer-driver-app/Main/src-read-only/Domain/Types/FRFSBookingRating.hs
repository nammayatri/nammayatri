{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSBookingRating where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSBookingRating = FRFSBookingRating
  { bookingId :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    driverRatingValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    feedbackDetails :: Kernel.Prelude.Maybe Data.Text.Text,
    fleetNumber :: Kernel.Prelude.Maybe Data.Text.Text,
    fleetRatingValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    gtfsId :: Kernel.Prelude.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSBookingRating.FRFSBookingRating,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
