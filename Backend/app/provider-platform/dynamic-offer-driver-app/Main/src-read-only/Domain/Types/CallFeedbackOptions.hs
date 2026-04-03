{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.CallFeedbackOptions where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data CallFeedbackOptions
    = CallFeedbackOptions {category :: Kernel.Prelude.Text,
                           id :: Kernel.Types.Id.Id Domain.Types.CallFeedbackOptions.CallFeedbackOptions,
                           messageKey :: Kernel.Prelude.Text,
                           merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                           merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                           createdAt :: Kernel.Prelude.UTCTime,
                           updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



