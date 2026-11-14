module BecknV2.OnDemand.Settlement
  ( PaymentChargeDetails (..),
    paymentChargeDetailsTagList,
    paymentChargeDetailsFromTags,
    chargeOn,
    riderGross,
    GrossMultiplier (..),
    grossMultipliers,
    PlatformDeductions (..),
    platformDeductionsAtPayment,
    SettlementTags (..),
    settlementTagGroups,
    settlementFromTags,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils (getTag)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..), PaymentChargeType (..))
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, highPrecMoneyFromText, highPrecMoneyToText)

data PaymentChargeDetails = PaymentChargeDetails
  { chargeType :: PaymentChargeType,
    chargeValue :: HighPrecMoney,
    chargeTax :: Maybe Tax.TaxConfig,
    bearer :: PaymentChargeBearer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

paymentChargeDetailsTagList :: Maybe PaymentChargeDetails -> Tags.TagList
paymentChargeDetailsTagList = maybe [] $ \config ->
  [Tags.PAYMENT_CHARGE_DETAILS Tags.~= TE.decodeUtf8 (BL.toStrict (A.encode config))]

paymentChargeDetailsFromTags :: Maybe [Spec.TagGroup] -> Maybe PaymentChargeDetails
paymentChargeDetailsFromTags tagGroups =
  getTag Tags.PAYMENT_CHARGE_DETAILS tagGroups >>= A.decodeStrict . TE.encodeUtf8

chargeOn :: PaymentChargeDetails -> HighPrecMoney -> Tax.TaxedAmount
chargeOn config amount =
  Tax.TaxedAmount {taxExclusive = charge, tax = Tax.taxTotal (Tax.taxOn (maybe Tax.VAT Tax.chargeRegime config.chargeTax) config.chargeTax (Tax.Exclusive charge))}
  where
    charge = case config.chargeType of
      PERCENTAGE -> max 0 amount * config.chargeValue / 100
      FIXED -> config.chargeValue

riderGross :: Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
riderGross mbConfig amount = case mbConfig of
  Just config | config.bearer == PAYMENT_CUSTOMER -> amount + Tax.taxedTotal (chargeOn config amount)
  _ -> amount

data GrossMultiplier = GrossMultiplier
  { taxExclusive :: HighPrecMoney,
    taxInclusive :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

grossMultipliers :: Maybe PaymentChargeDetails -> Tax.RiderTaxRates -> FB.RideTaxRates GrossMultiplier
grossMultipliers mbConfig rates =
  (\taxConfig -> GrossMultiplier {taxExclusive = (1 + Tax.taxRateOf taxConfig) * (1 + paymentChargeRate), taxInclusive = 1 + paymentChargeRate}) <$> rates.riderPayable
  where
    paymentChargeRate = case mbConfig of
      Just config | config.bearer == PAYMENT_CUSTOMER && config.chargeType == PERCENTAGE -> config.chargeValue / 100 * (1 + Tax.taxRateOf config.chargeTax)
      _ -> 0

data PlatformDeductions = PlatformDeductions
  { commissionDeductedAtPayment :: Bool,
    paymentChargeDeductedAtPayment :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

platformDeductionsAtPayment :: Bool -> Maybe PaymentChargeDetails -> PlatformDeductions
platformDeductionsAtPayment isOnline mbDetails =
  PlatformDeductions
    { commissionDeductedAtPayment = isOnline,
      paymentChargeDeductedAtPayment = isOnline && maybe False ((/= PAYMENT_PLATFORM) . (.bearer)) mbDetails
    }

data SettlementTags = SettlementTags
  { commission :: Maybe HighPrecMoney,
    commissionTaxBreakup :: Maybe Tax.TaxBreakup,
    paymentCharge :: Maybe HighPrecMoney,
    paymentChargeTaxBreakup :: Maybe Tax.TaxBreakup,
    paymentChargeDetails :: Maybe PaymentChargeDetails
  }

data ChargeTags = ChargeTags
  { amountTag :: Tags.BecknTag,
    taxTag :: Tags.BecknTag,
    cgstTag :: Tags.BecknTag,
    sgstTag :: Tags.BecknTag,
    igstTag :: Tags.BecknTag
  }

commissionTags :: ChargeTags
commissionTags = ChargeTags Tags.COMMISSION Tags.COMMISSION_TAX Tags.COMMISSION_CGST Tags.COMMISSION_SGST Tags.COMMISSION_IGST

paymentChargeTags :: ChargeTags
paymentChargeTags = ChargeTags Tags.PAYMENT_CHARGE Tags.PAYMENT_CHARGE_TAX Tags.PAYMENT_CHARGE_CGST Tags.PAYMENT_CHARGE_SGST Tags.PAYMENT_CHARGE_IGST

settlementTagGroups :: SettlementTags -> Maybe [Spec.TagGroup]
settlementTagGroups settlement =
  if null tags then Nothing else Tags.buildTagGroups tags
  where
    tags =
      chargeTagList commissionTags settlement.commission settlement.commissionTaxBreakup
        <> chargeTagList paymentChargeTags settlement.paymentCharge settlement.paymentChargeTaxBreakup
        <> paymentChargeDetailsTagList settlement.paymentChargeDetails
    chargeTagList charge mbAmount mbTaxBreakup =
      catMaybes
        [ (charge.amountTag Tags.~=) . highPrecMoneyToText <$> mbAmount,
          (charge.taxTag Tags.~=) . highPrecMoneyToText . (.total) <$> mbTaxBreakup,
          (charge.cgstTag Tags.~=) . highPrecMoneyToText <$> (mbTaxBreakup >>= (.cgst)),
          (charge.sgstTag Tags.~=) . highPrecMoneyToText <$> (mbTaxBreakup >>= (.sgst)),
          (charge.igstTag Tags.~=) . highPrecMoneyToText <$> (mbTaxBreakup >>= (.igst))
        ]

settlementFromTags :: Maybe [Spec.TagGroup] -> SettlementTags
settlementFromTags tagGroups =
  SettlementTags
    { commission = money commissionTags.amountTag,
      commissionTaxBreakup = taxBreakupOf commissionTags,
      paymentCharge = money paymentChargeTags.amountTag,
      paymentChargeTaxBreakup = taxBreakupOf paymentChargeTags,
      paymentChargeDetails = paymentChargeDetailsFromTags tagGroups
    }
  where
    money tag = getTag tag tagGroups >>= highPrecMoneyFromText
    taxBreakupOf charge = Tax.mkTaxBreakup (money charge.taxTag) (money charge.cgstTag) (money charge.sgstTag) (money charge.igstTag)
