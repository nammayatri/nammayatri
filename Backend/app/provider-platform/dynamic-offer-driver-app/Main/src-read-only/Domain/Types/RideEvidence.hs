{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RideEvidence where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RideEvidence = RideEvidence
  { description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    hiddenAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    hiddenBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hiddenReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.RideEvidence.RideEvidence,
    mediaFileId :: Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    rideStatus :: Domain.Types.Ride.RideStatus,
    thumbnailMediaFileId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
