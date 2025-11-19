{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Pass where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassType
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Pass = Pass
  { amount :: Kernel.Types.Common.HighPrecMoney,
    applicableVehicleServiceTiers :: [BecknV2.FRFS.Enums.ServiceTierType],
    autoApply :: Kernel.Prelude.Bool,
    benefit :: Kernel.Prelude.Maybe Domain.Types.Pass.Benefit,
    benefitDescription :: Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentsRequired :: [Domain.Types.Pass.PassDocumentType],
    enable :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    maxAmount :: Kernel.Types.Common.HighPrecMoney,
    maxValidDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minAmount :: Kernel.Types.Common.HighPrecMoney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    passTypeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passTypeId :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    purchaseEligibilityJsonLogic :: [Data.Aeson.Value],
    redeemEligibilityJsonLogic :: [Data.Aeson.Value],
    verificationValidity :: Kernel.Types.Common.Seconds,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Benefit
  = FullSaving
  | FixedSaving Kernel.Types.Common.HighPrecMoney
  | PercentageSaving Kernel.Types.Common.HighPrecMoney
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data PassDocumentType = ProfilePicture | Aadhaar deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PassDocumentType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''PassDocumentType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''Benefit)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''Benefit)
