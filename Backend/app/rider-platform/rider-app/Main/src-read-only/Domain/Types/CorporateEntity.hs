{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateEntity (module Domain.Types.CorporateEntity, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.CorporateEntity as ReExport
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateEntity = CorporateEntity
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    name :: Kernel.Prelude.Text,
    registeredName :: Kernel.Prelude.Text,
    gstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    industry :: Kernel.Prelude.Text,
    contactPersonName :: Kernel.Prelude.Text,
    contactEmail :: Kernel.Prelude.Text,
    contactPhone :: Kernel.Prelude.Text,
    billingAddress :: Kernel.Prelude.Text,
    billingModel :: Domain.Types.CorporateEntity.CorporateBillingModel,
    billingCycleType :: Domain.Types.CorporateEntity.BillingCycleType,
    creditLimit :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    status :: Domain.Types.CorporateEntity.CorporateEntityStatus,
    contractStartDate :: Kernel.Prelude.UTCTime,
    contractEndDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateBillingModel = PER_TRIP | PER_EMPLOYEE_MONTH | PER_SEAT_KM | FLAT_ROUTE | HYBRID
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data BillingCycleType = WEEKLY | BIWEEKLY | MONTHLY
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data CorporateEntityStatus = ONBOARDING | ACTIVE | SUSPENDED | CHURNED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateBillingModel)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BillingCycleType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateEntityStatus)
