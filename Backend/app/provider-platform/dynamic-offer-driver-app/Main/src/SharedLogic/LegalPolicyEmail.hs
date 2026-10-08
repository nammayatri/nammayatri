module SharedLogic.LegalPolicyEmail
  ( LegalPolicyEmailLogicInput (..),
    LegalPolicyEmailLogicOutput (..),
  )
where

import qualified Dashboard.Common as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id

data LegalPolicyEmailLogicInput = LegalPolicyEmailLogicInput
  { policyDocId :: Id DPCD.PolicyAndComplianceDocument,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    policyType :: Common.PolicyType,
    entityType :: Maybe Common.LegalEntityType,
    version :: Text,
    url :: Text,
    isMandatory :: Bool,
    personId :: Id DP.Person,
    language :: Maybe Language
  }
  deriving (Generic, Show, ToJSON)
data LegalPolicyEmailLogicOutput = LegalPolicyEmailLogicOutput
  { subject :: [Text],
    body :: [Text],
    fromEmail :: Maybe Text
  }
  deriving (Generic, Show, FromJSON)
