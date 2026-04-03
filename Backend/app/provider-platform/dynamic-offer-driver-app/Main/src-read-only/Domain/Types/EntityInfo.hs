{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.EntityInfo where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.DocumentReminderHistory
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data EntityInfo
    = EntityInfo {answer :: Kernel.Prelude.Text,
                  createdAt :: Kernel.Prelude.UTCTime,
                  entityId :: Kernel.Prelude.Text,
                  entityType :: Domain.Types.DocumentReminderHistory.EntityType,
                  merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                  merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                  question :: Kernel.Prelude.Text,
                  questionId :: Kernel.Prelude.Text,
                  updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



