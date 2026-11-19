{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.EmailTemplate (module Domain.Types.EmailTemplate, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.EmailTemplate as ReExport
import qualified Domain.Types.Extra.EmailTemplate
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data EmailTemplate = EmailTemplate
  { config :: Domain.Types.Extra.EmailTemplate.EmailTemplateConfig,
    createdAt :: Kernel.Prelude.UTCTime,
    htmlTemplate :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.EmailTemplate.EmailTemplate,
    language :: Kernel.External.Types.Language,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    subjectTemplate :: Kernel.Prelude.Text,
    templateKey :: Domain.Types.EmailTemplate.EmailTemplateKey,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data EmailTemplateKey = RIDE_INVOICE | REFUND_INVOICE | RIDE_CANCELLATION_INVOICE | LEGAL_POLICY_UPDATE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, Enum)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EmailTemplateKey)
