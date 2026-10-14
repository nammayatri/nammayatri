{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RideEvidence where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride
import qualified Domain.Types.RideEvidence
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data AddRideEvidenceReq = AddRideEvidenceReq {description :: Kernel.Prelude.Maybe Kernel.Prelude.Text, fileId :: Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideEvidenceItem = RideEvidenceItem
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fileId :: Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile,
    filePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fileStatus :: Kernel.Prelude.Maybe IssueManagement.Domain.Types.MediaFile.MediaFileUploadStatus,
    id :: Kernel.Types.Id.Id Domain.Types.RideEvidence.RideEvidence,
    imageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideStatus :: Domain.Types.Ride.RideStatus,
    thumbnailUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    urlsExpireAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
