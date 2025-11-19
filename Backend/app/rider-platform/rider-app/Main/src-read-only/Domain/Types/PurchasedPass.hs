{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PurchasedPass where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PurchasedPass = PurchasedPass
  { applicableVehicleServiceTiers :: [BecknV2.FRFS.Enums.ServiceTierType],
    benefitDescription :: Kernel.Prelude.Text,
    benefitType :: Kernel.Prelude.Maybe Domain.Types.PurchasedPass.BenefitType,
    benefitValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    deviceId :: Kernel.Prelude.Text,
    deviceSwitchCount :: Kernel.Prelude.Int,
    endDate :: Data.Time.Calendar.Day,
    id :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    maxAmount :: Kernel.Types.Common.HighPrecMoney,
    maxValidDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minAmount :: Kernel.Types.Common.HighPrecMoney,
    passAmount :: Kernel.Types.Common.HighPrecMoney,
    passCode :: Kernel.Prelude.Text,
    passDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passNumber :: Kernel.Prelude.Int,
    passTypeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passTypeId :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    startDate :: Data.Time.Calendar.Day,
    status :: Domain.Types.PurchasedPass.StatusType,
    usedTripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    verificationValidity :: Kernel.Types.Common.Seconds,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BenefitType = FullSaving | FixedSaving | PercentageSaving deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data StatusType
  = Pending
  | Active
  | PreBooked
  | Failed
  | Expired
  | RefundPending
  | RefundInitiated
  | Refunded
  | RefundFailed
  | Invalidated
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''StatusType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''StatusType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''BenefitType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''BenefitType)
