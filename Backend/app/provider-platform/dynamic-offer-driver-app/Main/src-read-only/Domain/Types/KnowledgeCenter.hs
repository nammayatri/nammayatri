{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.KnowledgeCenter where
import Kernel.Prelude
import Data.Aeson
import qualified AWS.S3.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data KnowledgeCenter
    = KnowledgeCenter {createdAt :: Kernel.Prelude.UTCTime,
                       documentName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       fileType :: AWS.S3.Types.FileType,
                       id :: Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter,
                       merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                       merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                       s3Path :: Kernel.Prelude.Text,
                       sopType :: Kernel.Prelude.Text,
                       updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



