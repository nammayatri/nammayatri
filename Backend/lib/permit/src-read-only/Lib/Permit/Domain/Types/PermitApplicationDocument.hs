{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Domain.Types.PermitApplicationDocument where

import Data.Aeson
import qualified Data.Time
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Tools.Beam.UtilsTH

data PermitApplicationDocument = PermitApplicationDocument
  { applicationId :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication,
    contentType :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    documentType :: Lib.Permit.Domain.Types.PermitApplicationDocument.PermitDocumentType,
    fileHash :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument,
    lifetime :: Kernel.Prelude.Bool,
    s3Path :: Kernel.Prelude.Text,
    sizeBytes :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    validUntil :: Kernel.Prelude.Maybe Data.Time.Day
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PermitDocumentType
  = RC
  | INSURANCE
  | PUC
  | ROAD_TAX
  | FITNESS
  | VEHICLE_PERMIT
  | SPEED_LIMITER
  | DRIVING_LICENCE
  | CHALLAN_STATUS
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PermitDocumentType))

$(mkHttpInstancesForEnum (''PermitDocumentType))
