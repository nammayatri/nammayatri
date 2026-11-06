module BecknV2.OnDemand.PaymentCharge
  ( PaymentChargeDetails (..),
    PaymentChargeQuote (..),
    quotePaymentCharge,
    paymentChargeGross,
    paymentChargeOnShare,
    quotePaymentChargeShare,
    customerPaymentChargeGross,
    paymentChargeShareBorneBy,
    paymentChargeRateBorneBy,
    paymentChargeDetailsTagList,
    paymentChargeDetailsFromTags,
    PlatformDeductions (..),
    platformDeductionsAtPayment,
    platformDeductionsTagList,
    platformDeductionsFromTags,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils (getTag)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..), PaymentChargeType (..))
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney, roundAmountByCurrency')

data PaymentChargeDetails = PaymentChargeDetails
  { chargeType :: PaymentChargeType,
    chargeValue :: HighPrecMoney,
    chargeTax :: Maybe Tax.TaxConfig,
    bearer :: PaymentChargeBearer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data PaymentChargeQuote = PaymentChargeQuote
  { net :: HighPrecMoney,
    tax :: HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    igst :: Maybe HighPrecMoney,
    bearer :: PaymentChargeBearer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

quotePaymentCharge :: Currency -> PaymentChargeDetails -> HighPrecMoney -> Maybe PaymentChargeQuote
quotePaymentCharge currency config payable = do
  let net = roundAmountByCurrency' currency $ case config.chargeType of
        PERCENTAGE -> max 0 payable * config.chargeValue / 100
        FIXED -> config.chargeValue
      mbTax = config.chargeTax >>= \taxConfig -> Tax.taxOnExclusive currency (Tax.chargeRegime taxConfig) (Just taxConfig) net
  guard (net > 0)
  pure PaymentChargeQuote {net, tax = Tax.taxTotal mbTax, cgst = mbTax >>= Tax.cgst, sgst = mbTax >>= Tax.sgst, igst = mbTax >>= Tax.igst, bearer = config.bearer}

paymentChargeGross :: PaymentChargeQuote -> HighPrecMoney
paymentChargeGross quote = quote.net + quote.tax

quotePaymentChargeShare :: Currency -> PaymentChargeDetails -> HighPrecMoney -> Maybe PaymentChargeQuote
quotePaymentChargeShare currency config amount = case config.chargeType of
  PERCENTAGE -> quotePaymentCharge currency config amount
  FIXED -> Nothing

paymentChargeOnShare :: Currency -> PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
paymentChargeOnShare currency config amount = maybe 0 paymentChargeGross (quotePaymentChargeShare currency config amount)

customerPaymentChargeGross :: Maybe PaymentChargeQuote -> HighPrecMoney
customerPaymentChargeGross = maybe 0 (\quote -> if quote.bearer == PAYMENT_CUSTOMER then paymentChargeGross quote else 0)

paymentChargeShareBorneBy :: PaymentChargeBearer -> Currency -> Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
paymentChargeShareBorneBy bearer currency mbConfig amount = maybe 0 (\config -> if config.bearer == bearer then paymentChargeOnShare currency config amount else 0) mbConfig

paymentChargeRateBorneBy :: PaymentChargeBearer -> Maybe PaymentChargeDetails -> HighPrecMoney
paymentChargeRateBorneBy bearer = maybe 0 $ \config ->
  if config.bearer == bearer && config.chargeType == PERCENTAGE
    then config.chargeValue / 100 * (1 + maybe 0 (\taxConfig -> Tax.taxRateOf (Tax.chargeRegime taxConfig) (Just taxConfig)) config.chargeTax)
    else 0

paymentChargeDetailsTagList :: Maybe PaymentChargeDetails -> Tags.TagList
paymentChargeDetailsTagList = maybe [] $ \config ->
  [Tags.PAYMENT_CHARGE_DETAILS Tags.~= TE.decodeUtf8 (BL.toStrict (A.encode config))]

paymentChargeDetailsFromTags :: Maybe [Spec.TagGroup] -> Maybe PaymentChargeDetails
paymentChargeDetailsFromTags tagGroups =
  getTag Tags.PAYMENT_CHARGE_DETAILS tagGroups >>= A.decodeStrict . TE.encodeUtf8

data PlatformDeductions = PlatformDeductions
  { commissionDeductedAtPayment :: Bool,
    paymentChargeDeductedAtPayment :: Bool
  }
  deriving (Show, Eq, Generic)

platformDeductionsAtPayment :: Bool -> Maybe PaymentChargeDetails -> PlatformDeductions
platformDeductionsAtPayment isOnline mbDetails =
  PlatformDeductions
    { commissionDeductedAtPayment = isOnline,
      paymentChargeDeductedAtPayment = isOnline && maybe False ((`elem` [PAYMENT_CUSTOMER, PAYMENT_DRIVER]) . (.bearer)) mbDetails
    }

platformDeductionsTagList :: PlatformDeductions -> Tags.TagList
platformDeductionsTagList deductions =
  [ Tags.COMMISSION_DEDUCTED_AT_PAYMENT Tags.~= show deductions.commissionDeductedAtPayment,
    Tags.PAYMENT_CHARGE_DEDUCTED_AT_PAYMENT Tags.~= show deductions.paymentChargeDeductedAtPayment
  ]

platformDeductionsFromTags :: Maybe [Spec.TagGroup] -> Maybe PlatformDeductions
platformDeductionsFromTags tagGroups =
  PlatformDeductions <$> flagOf Tags.COMMISSION_DEDUCTED_AT_PAYMENT <*> flagOf Tags.PAYMENT_CHARGE_DEDUCTED_AT_PAYMENT
  where
    flagOf tag = (== "True") <$> getTag tag tagGroups
