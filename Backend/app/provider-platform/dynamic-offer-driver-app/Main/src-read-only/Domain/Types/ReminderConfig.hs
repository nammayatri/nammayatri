{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ReminderConfig where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ReminderConfig = ReminderConfig
  { createdAt :: Kernel.Prelude.UTCTime,
    daysThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    enabled :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reminderIntervals :: [Kernel.Prelude.Int],
    ridesThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
