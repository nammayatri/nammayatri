{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.TDSDistributionPdfFile where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.TDSDistributionRecord
import qualified Tools.Beam.UtilsTH



data TDSDistributionPdfFile
    = TDSDistributionPdfFile {createdAt :: Kernel.Prelude.UTCTime,
                              fileName :: Kernel.Prelude.Text,
                              id :: Kernel.Types.Id.Id Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile,
                              s3FilePath :: Kernel.Prelude.Text,
                              tdsDistributionRecordId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord),
                              updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



