{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareParameters (module Domain.Types.FareParameters, module ReExport) where

import Data.Aeson
import qualified Domain.Types.ConditionalCharges
import Domain.Types.Extra.FareParameters as ReExport
import qualified Domain.Types.Extra.FareParameters
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data FareParameters = FareParameters
  { airportConvenienceFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    baseFare :: Kernel.Types.Common.HighPrecMoney,
    boothCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    businessDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cancellationFeeTaxExclusive :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cancellationTax :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cardCharge :: Kernel.Prelude.Maybe Domain.Types.Extra.FareParameters.CardCharge,
    cgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    commission :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    conditionalCharges :: [Domain.Types.ConditionalCharges.ConditionalCharges],
    congestionCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    congestionChargeViaDp :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    customerCancellationDues :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    customerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    customerGateFeeItems :: [Domain.Types.Extra.FareParameters.CustomerGateFeeItem],
    discountApplicableRideFareTax :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    discountApplicableRideFareTaxExclusive :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverAllowance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverCancellationNotAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverSelectedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fareParametersDetails :: Domain.Types.Extra.FareParameters.FareParametersDetails,
    fareSettlementType :: Kernel.Prelude.Maybe Lib.Types.SpecialLocation.FareSettlementType,
    govtCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.FareParameters.FareParameters,
    insuranceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    isVatTaxType :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    luggageCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    negativeFareAdjustment :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    negotiatedFareDelta :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    nightShiftCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    nightShiftRateIfApplies :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    nonDiscountApplicableRideFareTax :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    nonDiscountApplicableRideFareTaxExclusive :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    parkingCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    parkingChargeTax :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    parkingChargeTaxExclusive :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    paymentProcessingFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    paymentProcessingFeeVat :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    personalDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    petCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeChargesBy :: Domain.Types.FarePolicy.PlatformFeeMethods,
    priorityCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    returnFeeCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    rideExtraTimeFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    schedulingCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    serviceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    shouldApplyBusinessDiscount :: Kernel.Prelude.Bool,
    shouldApplyPersonalDiscount :: Kernel.Prelude.Bool,
    stopCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tdsAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tdsProcessedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tdsRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollFareTax :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollFareTaxExclusive :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    waitingCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
