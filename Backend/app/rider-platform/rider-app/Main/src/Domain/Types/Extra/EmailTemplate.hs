module Domain.Types.Extra.EmailTemplate where

import Dashboard.Common (PolicyType)
import Data.Aeson
import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Lib.Finance.Invoice.Email (InvoiceEmailConfig)

data LegalPolicyEmailConfig = LegalPolicyEmailConfig
  { fromEmail :: Text,
    policyTypeLabels :: Map.Map PolicyType Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data EmailTemplateConfig
  = InvoiceEmailTemplateConfig InvoiceEmailConfig
  | LegalPolicyEmailTemplateConfig LegalPolicyEmailConfig
  deriving (Generic, Show, Eq)

instance FromJSON EmailTemplateConfig where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = UntaggedValue}

instance ToJSON EmailTemplateConfig where
  toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue}
