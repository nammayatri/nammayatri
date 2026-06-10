{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ReviewRequest where

import qualified Dashboard.Common
import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data ReviewRequest = ReviewRequest
  { createdAt :: Kernel.Prelude.UTCTime,
    documentDetails :: Kernel.Prelude.Maybe [Domain.Types.ReviewRequest.DocumentDetail],
    entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.ReviewRequest.EntityType,
    id :: Kernel.Types.Id.Id Domain.Types.ReviewRequest.ReviewRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    rcNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestStatus :: Domain.Types.ReviewRequest.RequestStatus,
    requestType :: Domain.Types.ReviewRequest.RequestType,
    reviewerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DocumentDetail = DocumentDetail
  { documentDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    imageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    mediaId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rejectedReason :: Kernel.Prelude.Text,
    remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EntityType = DRIVER | VEHICLE | FLEET_OWNER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RequestStatus = IN_PROGRESS | COMPLETED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RequestType = BOT_REVIEW deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EntityType)

$(mkHttpInstancesForEnum ''EntityType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RequestStatus)

$(mkHttpInstancesForEnum ''RequestStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RequestType)

$(mkHttpInstancesForEnum ''RequestType)
