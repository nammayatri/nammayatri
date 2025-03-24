{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ApprovalRequest where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data ApprovalRequest = ApprovalRequest
  { body :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Data.Text.Text,
    entityType :: Domain.Types.ApprovalRequest.EntityType,
    id :: Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requestData :: Domain.Types.ApprovalRequest.ApprovalRequestData,
    requestType :: Domain.Types.ApprovalRequest.RequestType,
    requesteeId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requesteeType :: Domain.Types.ApprovalRequest.RequesteeType,
    requestorId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestorType :: Domain.Types.ApprovalRequest.RequestorType,
    status :: Domain.Types.ApprovalRequest.RequestStatus,
    title :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ApprovalRequestData
  = EndRide Domain.Types.ApprovalRequest.EndRideData
  | OverSpeeding Domain.Types.ApprovalRequest.OverSpeedingData
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data EndRideData = EndRideData
  { driverMobileNumber :: Kernel.Prelude.Maybe Data.Text.Text,
    driverName :: Data.Text.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    tripCode :: Kernel.Prelude.Maybe Data.Text.Text,
    tripTransactionId :: Data.Text.Text,
    vehicleRegistrationNumber :: Data.Text.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data EntityType = TRIP | RIDE deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data OverSpeedingData = OverSpeedingData {location :: Kernel.External.Maps.Types.LatLong, speed :: Kernel.Prelude.Double}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RequestStatus = ACCEPTED | REJECTED | AWAITING_APPROVAL | REVOKED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RequestType = EndRideApproval | OverSpeedingAlert deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RequesteeType = FleetOwner | Driver deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RequestorType = SystemGenerated | DriverGenerated deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''ApprovalRequestData)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''ApprovalRequestData)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestStatus)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestorType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestorType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequesteeType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequesteeType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''EntityType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''EntityType)
