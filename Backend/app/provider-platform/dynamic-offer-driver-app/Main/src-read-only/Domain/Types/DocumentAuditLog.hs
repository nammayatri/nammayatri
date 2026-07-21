{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DocumentAuditLog where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DocumentAuditLog = DocumentAuditLog
  { action :: Domain.Types.DocumentAuditLog.AuditAction,
    actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorRole :: Kernel.Prelude.Text,
    actorSource :: Domain.Types.DocumentAuditLog.ActorSource,
    createdAt :: Kernel.Prelude.UTCTime,
    details :: Kernel.Prelude.Maybe Data.Aeson.Value,
    documentRefId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentRefType :: Domain.Types.DocumentAuditLog.DocumentRefType,
    documentType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.DocumentAuditLog.AuditEntityType,
    eventId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fieldName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.DocumentAuditLog.DocumentAuditLog,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    newStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    previousStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data ActorSource = DRIVER_APP | DASHBOARD | SCHEDULER | WEBHOOK | SYSTEM deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AuditAction
  = UPLOADED
  | APPROVED
  | REJECTED
  | STATUS_CHANGED
  | DELETED
  | FLAG_CHANGED
  | VERIFICATION_REQUESTED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AuditEntityType = DRIVER | FLEET_OWNER | VEHICLE | OPERATOR deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DocumentRefType
  = IMAGE
  | DRIVER_LICENSE
  | VEHICLE_REGISTRATION_CERTIFICATE
  | DRIVER_PAN_CARD
  | AADHAAR_CARD
  | VEHICLE_INSURANCE
  | VEHICLE_PUC
  | VEHICLE_FITNESS_CERTIFICATE
  | VEHICLE_PERMIT
  | DRIVER_GSTIN
  | VEHICLE_NOC
  | BUSINESS_LICENSE
  | DRIVER_SSN
  | DRIVER_UDYAM
  | COMMON_ONBOARDING_DOCUMENT
  | MEDIA_FILE_DOCUMENT
  | OPERATION_HUB_REQUEST
  | REVIEW_REQUEST
  | DOCUMENT_REMINDER_HISTORY
  | DRIVER_INFORMATION
  | FLEET_OWNER_INFORMATION
  | DRIVER_BANK_ACCOUNT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''AuditAction)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''AuditEntityType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''ActorSource)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''DocumentRefType)
