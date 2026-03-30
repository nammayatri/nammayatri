{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TDSDistributionPdfFile where

import Data.Aeson
import qualified Domain.Types.TDSDistributionRecord
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TDSDistributionPdfFile = TDSDistributionPdfFile
  { createdAt :: Kernel.Prelude.UTCTime,
    fileName :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile,
    s3FilePath :: Kernel.Prelude.Text,
    tdsDistributionRecordId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
