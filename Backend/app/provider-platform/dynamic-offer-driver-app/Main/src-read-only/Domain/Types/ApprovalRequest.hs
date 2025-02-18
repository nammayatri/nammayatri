{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ApprovalRequest where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.EmptyDynamicParam
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data ApprovalRequest = ApprovalRequest
  { body :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requestData :: Domain.Types.ApprovalRequest.ApprovalRequestData,
    requesteeId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestorId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    status :: Domain.Types.ApprovalRequest.RequestStatus,
    title :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ApprovalRequestData
  = EndRide Domain.Types.ApprovalRequest.EndRideData
  | ChangeRoute Domain.Types.EmptyDynamicParam.EmptyDynamicParam
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

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

data RequestStatus = ACCEPTED | REJECTED | AWAITING_APPROVAL | REVOKED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (Kernel.Prelude.ToParamSchema))

data RequestType = END_RIDE | CHANGE_ROUTE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ApprovalRequestData))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RequestType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''RequestStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''RequestStatus))
