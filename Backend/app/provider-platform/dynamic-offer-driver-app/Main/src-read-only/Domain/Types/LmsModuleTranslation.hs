{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.LmsModuleTranslation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data LmsModuleTranslation
    = LmsModuleTranslation {description :: Kernel.Prelude.Text,
                            language :: Kernel.External.Types.Language,
                            moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
                            name :: Kernel.Prelude.Text,
                            thumbnailImage :: Kernel.Prelude.Text,
                            merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                            merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                            createdAt :: Kernel.Prelude.UTCTime,
                            updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



