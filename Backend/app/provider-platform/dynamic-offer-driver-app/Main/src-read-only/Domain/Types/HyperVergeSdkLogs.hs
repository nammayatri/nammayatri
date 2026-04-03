{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.HyperVergeSdkLogs where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data HyperVergeSdkLogs
    = HyperVergeSdkLogs {callbackResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         createdAt :: Kernel.Prelude.UTCTime,
                         docType :: Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentType,
                         driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                         failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         hvFlowId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                         status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         txnId :: Kernel.Prelude.Text,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



