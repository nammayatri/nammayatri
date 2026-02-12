{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DocumentReminderHistory where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DocumentReminderHistory = DocumentReminderHistory
  { completionDate :: Kernel.Prelude.UTCTime,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.DocumentReminderHistory.EntityType,
    id :: Kernel.Types.Id.Id Domain.Types.DocumentReminderHistory.DocumentReminderHistory,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    rideCountAtCompletion :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EntityType = DRIVER | RC deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''EntityType)
