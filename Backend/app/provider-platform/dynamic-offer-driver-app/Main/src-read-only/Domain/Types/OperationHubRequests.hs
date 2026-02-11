{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.OperationHubRequests where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.OperationHub
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data OperationHubRequests = OperationHubRequests
  { creatorId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    fulfilledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.OperationHubRequests.OperationHubRequests,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    operationHubId :: Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub,
    operatorId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    registrationNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestStatus :: Domain.Types.OperationHubRequests.RequestStatus,
    requestType :: Domain.Types.OperationHubRequests.RequestType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RequestStatus = PENDING | APPROVED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RequestType
  = ONBOARDING_INSPECTION
  | REGULAR_INSPECTION
  | DRIVER_ONBOARDING_INSPECTION
  | DRIVER_REGULAR_INSPECTION
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RequestStatus)

$(mkHttpInstancesForEnum ''RequestStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RequestType)

$(mkHttpInstancesForEnum ''RequestType)
