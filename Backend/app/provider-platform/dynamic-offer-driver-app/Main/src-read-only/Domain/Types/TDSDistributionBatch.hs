{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TDSDistributionBatch where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TDSDistributionBatch = TDSDistributionBatch
  { completedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    confirmedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    confirmedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    confirmedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    financialYear :: Kernel.Prelude.Text,
    folderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    quarter :: Kernel.Prelude.Text,
    status :: Domain.Types.TDSDistributionBatch.TDSDistributionBatchStatus,
    totalFiles :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    uploadedById :: Kernel.Prelude.Text,
    uploadedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TDSDistributionBatchStatus = DRAFT | VALIDATED | SENDING | COMPLETED | CANCELLED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''TDSDistributionBatchStatus))
