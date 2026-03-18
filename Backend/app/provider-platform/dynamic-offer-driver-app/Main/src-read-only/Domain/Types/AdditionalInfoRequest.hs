{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AdditionalInfoRequest where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.OperationHubRequests
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data AdditionalInfoRequest = AdditionalInfoRequest
  { id :: Kernel.Types.Id.Id Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest,
    operationHubRequestId :: Kernel.Types.Id.Id Domain.Types.OperationHubRequests.OperationHubRequests,
    requestedBy :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestedFrom :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestedDocumentTypes :: [DocumentType],
    message :: Kernel.Prelude.Text,
    status :: AdditionalInfoStatus,
    responseRemarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseDocumentIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AdditionalInfoStatus = PENDING | RESPONDED | REVIEWED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DocumentType
  = PAN
  | AADHAAR
  | ADDRESS_PROOF
  | RC_PHOTO
  | DL_PHOTO
  | VEHICLE_PERMIT
  | INSURANCE
  | FITNESS_CERTIFICATE
  | OTHER
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AdditionalInfoStatus)

$(mkHttpInstancesForEnum ''AdditionalInfoStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DocumentType)

$(mkHttpInstancesForEnum ''DocumentType)
