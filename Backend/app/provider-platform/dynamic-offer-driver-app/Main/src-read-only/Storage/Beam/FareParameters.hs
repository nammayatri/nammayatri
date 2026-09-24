{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareParameters where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.FareParameters
import qualified Domain.Types.FarePolicy
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data FareParametersT f = FareParametersT
  { airportConvenienceFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    baseFare :: B.C f Kernel.Types.Common.Money,
    baseFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    boothCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    businessDiscount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cancellationFeeTaxExclusive :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cancellationTax :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cardChargeOnFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fixedCardCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cgst :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    commission :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    conditionalCharges :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    congestionCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    congestionChargeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    congestionChargeViaDp :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    customerCancellationDues :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    customerExtraFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    customerExtraFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    customerGateFeeItems :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    discountApplicableRideFareTax :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    discountApplicableRideFareTaxExclusive :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverAllowance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverCancellationNotAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverSelectedFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    driverSelectedFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fareParametersType :: B.C f Domain.Types.Extra.FareParameters.FareParametersType,
    fareSettlementType :: B.C f (Kernel.Prelude.Maybe Lib.Types.SpecialLocation.FareSettlementType),
    govtCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    govtChargesAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    id :: B.C f Kernel.Prelude.Text,
    insuranceCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    isVatTaxType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    luggageCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    negativeFareAdjustment :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    negativeFareAdjustmentAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    negotiatedFareDelta :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    nightShiftCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    nightShiftChargeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    nightShiftRateIfApplies :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    nonDiscountApplicableRideFareTax :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    nonDiscountApplicableRideFareTaxExclusive :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    parkingCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    parkingChargeTax :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    parkingChargeTaxExclusive :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    paymentProcessingFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    paymentProcessingFeeVat :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    personalDiscount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    petCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    platformFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    platformFeeChargesBy :: B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.PlatformFeeMethods),
    priorityCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    returnFeeCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    rideExtraTimeFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    rideExtraTimeFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    schedulingCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    serviceCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    serviceChargeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    sgst :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    shouldApplyBusinessDiscount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    shouldApplyPersonalDiscount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    stopCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tdsAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tdsProcessedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    tdsRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollVat :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollFareTaxExclusive :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    waitingCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    waitingChargeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersT where
  data PrimaryKey FareParametersT f = FareParametersId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareParametersId . id

type FareParameters = FareParametersT Identity

$(enableKVPG ''FareParametersT ['id] [])

$(mkTableInstances ''FareParametersT "fare_parameters")
