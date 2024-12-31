{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ApprovalRequest where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TripTransaction
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ApprovalRequest = ApprovalRequest
  { body :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requestType :: Domain.Types.ApprovalRequest.ApprovalRequestData,
    status :: Domain.Types.ApprovalRequest.RequestStatus,
    title :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ApprovalRequestData = WmbEndTrip Domain.Types.ApprovalRequest.WmbEndTripData deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RequestStatus = ACCEPTED | REJECTED | PENDING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data WmbEndTripData = WmbEndTripData
  { lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    tripTransactionId :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ApprovalRequestData)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RequestStatus)
