{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.KnowledgeCenter where

import qualified AWS.S3.Types
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data KnowledgeCenter = KnowledgeCenter
  { createdAt :: Kernel.Prelude.UTCTime,
    fileType :: AWS.S3.Types.FileType,
    id :: Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    s3Path :: Kernel.Prelude.Text,
    sopType :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
