{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TDSDistributionPdfFile where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Domain.Types.TDSDistributionBatch
import qualified Domain.Types.TDSDistributionRecord
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TDSDistributionPdfFile = TDSDistributionPdfFile
  { batchId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch),
    createdAt :: Kernel.Prelude.UTCTime,
    fileName :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile,
    issue :: Kernel.Prelude.Maybe Domain.Types.TDSDistributionPdfFile.TDSFileIssue,
    matchedPersonId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    recipientType :: Kernel.Prelude.Maybe Domain.Types.TDSDistributionPdfFile.TDSRecipientType,
    s3FilePath :: Kernel.Prelude.Text,
    sizeBytes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    tdsDistributionRecordId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord),
    updatedAt :: Kernel.Prelude.UTCTime,
    validationStatus :: Kernel.Prelude.Maybe Domain.Types.TDSDistributionPdfFile.TDSFileValidationStatus
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TDSFileIssue
  = INVALID_NAME
  | NOT_PDF
  | WRONG_QUARTER
  | WRONG_FY
  | TOO_LARGE
  | MISSING_UPLOAD
  | DUPLICATE_PAN
  | PAN_NOT_FOUND
  | AMBIGUOUS_PAN
  | ALREADY_SENT
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data TDSFileValidationStatus = PENDING | READY | SKIPPED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data TDSRecipientType = DRIVER | FLEET_OWNER deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''TDSFileValidationStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''TDSFileIssue))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''TDSRecipientType))
