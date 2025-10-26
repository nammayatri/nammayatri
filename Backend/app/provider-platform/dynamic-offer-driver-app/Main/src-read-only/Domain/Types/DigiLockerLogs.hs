{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DigiLockerLogs where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DigiLockerLogs = DigiLockerLogs
  { createdAt :: Kernel.Prelude.UTCTime,
    digiLockerState :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    digiLockerUri :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    docType :: Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentType,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    flowType :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.DigiLockerLogs.DigiLockerLogs,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    requestPayload :: Kernel.Prelude.Text,
    responsePayload :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
