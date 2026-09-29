{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PolicyAndComplianceDocument where

import qualified Dashboard.Common
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PolicyAndComplianceDocument = PolicyAndComplianceDocument
  { createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument,
    isMandatory :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metadata :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyType :: Dashboard.Common.PolicyType,
    updatedAt :: Kernel.Prelude.UTCTime,
    url :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
