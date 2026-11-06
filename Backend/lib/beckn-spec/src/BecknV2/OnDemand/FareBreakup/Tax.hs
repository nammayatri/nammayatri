module BecknV2.OnDemand.FareBreakup.Tax
  ( Regime (..),
    TaxConfig (..),
    RideTax (..),
    RiderTaxRates (..),
    riderTaxRatesOf,
    riderTaxRatesTagList,
    riderTaxRatesFromTags,
    DriverDeductible (..),
    PlatformDeductible (..),
    TaxBreakups (..),
    TaxBreakup (..),
    FareTaxes,
    TaxedAmount (..),
    AmountAddition (..),
    AmountDeduction (..),
    AmountBreakup (..),
    DeductionRates (..),
    PayableAddition (..),
    PayableBreakup (..),
    mkPayableBreakup,
    payableTotal,
    exclusiveFromPayable,
    taxRateOf,
    taxedExclusive,
    taxedInclusive,
    taxedTotal,
    riderTaxedExclusive,
    riderTaxedInclusive,
    mkAmountBreakup,
    deductionTotal,
    netOfExclusive,
    netOfInclusive,
    grossFromNet,
    mkTaxBreakup,
    taxTotal,
    chargeRegime,
    splitGst,
    splitGrossByTaxPct,
    taxPct,
    taxOnExclusive,
    taxOnInclusive,
    splitTax,
  )
where

import BecknV2.OnDemand.FareBreakup.Core (RideTaxRates)
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils (getTag)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney (..), getAccuracy, roundAmountByCurrency')

data Regime = GST | VAT | NONE
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data TaxConfig = TaxConfig
  { tax :: Double,
    cgst :: Maybe Double,
    sgst :: Maybe Double,
    igst :: Maybe Double
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data RideTax = RideTax
  { regime :: Regime,
    riderPayable :: RideTaxRates (Maybe TaxConfig),
    driverDeductible :: DriverDeductible (Maybe TaxConfig),
    platformDeductible :: PlatformDeductible (Maybe TaxConfig)
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RiderTaxRates = RiderTaxRates
  { regime :: Regime,
    riderPayable :: RideTaxRates (Maybe TaxConfig)
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

riderTaxRatesOf :: RideTax -> RiderTaxRates
riderTaxRatesOf rideTax = RiderTaxRates {regime = rideTax.regime, riderPayable = rideTax.riderPayable}

riderTaxRatesTagList :: Maybe RiderTaxRates -> Tags.TagList
riderTaxRatesTagList = maybe [] $ \rates ->
  [Tags.RIDE_TAX_RATES Tags.~= TE.decodeUtf8 (BL.toStrict (A.encode rates))]

riderTaxRatesFromTags :: Maybe [Spec.TagGroup] -> Maybe RiderTaxRates
riderTaxRatesFromTags tagGroups =
  getTag Tags.RIDE_TAX_RATES tagGroups >>= A.decodeStrict . TE.encodeUtf8

data DriverDeductible a = DriverDeductible
  { commission :: a,
    cancellationCommission :: a,
    paymentCharge :: a
  }
  deriving stock (Show, Read, Eq, Generic)

instance ToJSON a => ToJSON (DriverDeductible a)

instance FromJSON a => FromJSON (DriverDeductible a)

instance (Typeable a, ToSchema a) => ToSchema (DriverDeductible a)

newtype PlatformDeductible a = PlatformDeductible
  { paymentCharge :: a
  }
  deriving stock (Show, Read, Eq, Generic)

instance ToJSON a => ToJSON (PlatformDeductible a)

instance FromJSON a => FromJSON (PlatformDeductible a)

instance (Typeable a, ToSchema a) => ToSchema (PlatformDeductible a)

data TaxBreakup = TaxBreakup
  { total :: HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    igst :: Maybe HighPrecMoney
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

type FareTaxes = RideTaxRates (Maybe TaxBreakup)

data TaxBreakups = TaxBreakups
  { riderPayable :: FareTaxes,
    driverDeductible :: DriverDeductible (Maybe TaxBreakup),
    platformDeductible :: PlatformDeductible (Maybe TaxBreakup)
  }
  deriving (Show, Eq, Generic)

data TaxedAmount = TaxedAmount
  { taxExclusive :: HighPrecMoney,
    tax :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

newtype AmountAddition = AmountAddition
  { tax :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AmountDeduction = AmountDeduction
  { commission :: HighPrecMoney,
    paymentCharge :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AmountBreakup = AmountBreakup
  { addition :: AmountAddition,
    deduction :: AmountDeduction
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DeductionRates = DeductionRates
  { taxRate :: HighPrecMoney,
    commissionRate :: HighPrecMoney,
    paymentChargeRate :: HighPrecMoney
  }
  deriving (Show, Eq, Generic)

data PayableAddition = PayableAddition
  { tax :: HighPrecMoney,
    paymentCharge :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

newtype PayableBreakup = PayableBreakup
  { addition :: PayableAddition
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

mkPayableBreakup :: Currency -> HighPrecMoney -> HighPrecMoney -> PayableBreakup
mkPayableBreakup currency taxAmount paymentChargeAmount = PayableBreakup {addition = PayableAddition {tax = roundAmountByCurrency' currency taxAmount, paymentCharge = roundAmountByCurrency' currency paymentChargeAmount}}

payableTotal :: Currency -> HighPrecMoney -> PayableBreakup -> HighPrecMoney
payableTotal currency amount breakup = roundAmountByCurrency' currency amount + breakup.addition.tax + breakup.addition.paymentCharge

exclusiveFromPayable :: Currency -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney
exclusiveFromPayable currency rideTaxRate paymentChargeRate payable = roundAmountByCurrency' currency (payable / ((1 + rideTaxRate) * (1 + paymentChargeRate)))

taxRateOf :: Regime -> Maybe TaxConfig -> HighPrecMoney
taxRateOf taxRegime mbConfig = maybe 0 pctRatio (taxPct taxRegime mbConfig)

taxedExclusive :: Currency -> Regime -> Maybe TaxConfig -> HighPrecMoney -> TaxedAmount
taxedExclusive currency taxRegime mbConfig base = TaxedAmount {taxExclusive = base, tax = taxTotal (taxOnExclusive currency taxRegime mbConfig base)}

taxedInclusive :: Currency -> Regime -> Maybe TaxConfig -> HighPrecMoney -> TaxedAmount
taxedInclusive currency taxRegime mbConfig gross =
  let taxAmount = taxTotal (taxOnInclusive currency taxRegime mbConfig gross)
   in TaxedAmount {taxExclusive = gross - taxAmount, tax = taxAmount}

taxedTotal :: TaxedAmount -> HighPrecMoney
taxedTotal taxed = taxed.taxExclusive + taxed.tax

riderTaxedExclusive :: Currency -> (RideTaxRates (Maybe TaxConfig) -> Maybe TaxConfig) -> Maybe RiderTaxRates -> HighPrecMoney -> TaxedAmount
riderTaxedExclusive currency rateOf mbRates base = maybe (TaxedAmount base 0) (\rates -> taxedExclusive currency rates.regime (rateOf rates.riderPayable) base) mbRates

riderTaxedInclusive :: Currency -> (RideTaxRates (Maybe TaxConfig) -> Maybe TaxConfig) -> Maybe RiderTaxRates -> HighPrecMoney -> TaxedAmount
riderTaxedInclusive currency rateOf mbRates gross = maybe (TaxedAmount gross 0) (\rates -> taxedInclusive currency rates.regime (rateOf rates.riderPayable) gross) mbRates

mkAmountBreakup :: Currency -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> AmountBreakup
mkAmountBreakup currency taxAmount commissionAmount paymentChargeAmount =
  AmountBreakup
    { addition = AmountAddition {tax = roundAmountByCurrency' currency taxAmount},
      deduction = AmountDeduction {commission = roundAmountByCurrency' currency commissionAmount, paymentCharge = roundAmountByCurrency' currency paymentChargeAmount}
    }

deductionTotal :: AmountBreakup -> HighPrecMoney
deductionTotal breakup = breakup.deduction.commission + breakup.deduction.paymentCharge

netOfExclusive :: Currency -> HighPrecMoney -> AmountBreakup -> HighPrecMoney
netOfExclusive currency amount breakup = roundAmountByCurrency' currency amount + breakup.addition.tax - deductionTotal breakup

netOfInclusive :: Currency -> HighPrecMoney -> AmountBreakup -> HighPrecMoney
netOfInclusive currency amount breakup = roundAmountByCurrency' currency amount - deductionTotal breakup

grossFromNet :: Currency -> DeductionRates -> HighPrecMoney -> HighPrecMoney
grossFromNet currency rates net
  | factor > 0 = roundAmountByCurrency' currency (net / factor)
  | otherwise = net
  where
    factor = (1 + rates.taxRate) * (1 - rates.paymentChargeRate) - rates.commissionRate

mkTaxBreakup :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe TaxBreakup
mkTaxBreakup mbTotal cgstPart sgstPart igstPart = (\totalTax -> TaxBreakup {total = totalTax, cgst = cgstPart, sgst = sgstPart, igst = igstPart}) <$> mbTotal

taxTotal :: Maybe TaxBreakup -> HighPrecMoney
taxTotal = maybe 0 (.total)

largestRemainder :: Currency -> HighPrecMoney -> [Rational] -> [HighPrecMoney]
largestRemainder _ _ [] = []
largestRemainder currency amount weights
  | weightSum <= 0 = fromUnits units : map (const 0) (drop 1 weights)
  | otherwise = map (fromUnits . (* sign)) allocated
  where
    unitScale = 10 ^ getAccuracy currency :: Integer
    fromUnits n = HighPrecMoney (fromInteger n / fromInteger unitScale)
    units = round ((roundAmountByCurrency' currency amount).getHighPrecMoney * fromInteger unitScale) :: Integer
    sign = signum units
    absUnits = abs units
    clipped = map (max 0) weights
    weightSum = sum clipped
    raws = [fromInteger absUnits * w / weightSum | w <- clipped]
    floors = map floor raws :: [Integer]
    leftover = absUnits - sum floors
    fractions = zipWith (\r f -> r - fromInteger f) raws floors
    ranked = map snd $ List.sortOn (\(fr, i) -> (negate fr, i)) (zip fractions [0 :: Int ..])
    bonus = take (fromInteger leftover) ranked
    allocated = [f + (if i `elem` bonus then 1 else 0) | (f, i) <- zip floors [0 :: Int ..]]

splitGst :: Currency -> (Rational, Rational, Rational) -> HighPrecMoney -> (Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney)
splitGst currency (c, s, i) taxAmount = case mfilter (/= 0) . Just <$> largestRemainder currency taxAmount [c, s, i] of
  [cgstPart, sgstPart, igstPart] -> (cgstPart, sgstPart, igstPart)
  _ -> (Nothing, Nothing, Nothing)

splitGrossByTaxPct :: Maybe Double -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitGrossByTaxPct mbPct gross = case mbPct of
  Just pct
    | pct > 0 ->
      let taxAmount = HighPrecMoney (gross.getHighPrecMoney * (toRational pct / toRational (100 + pct)))
       in (gross - taxAmount, taxAmount)
  _ -> (gross, 0)

chargeRegime :: TaxConfig -> Regime
chargeRegime t = if any isJust [t.cgst, t.sgst, t.igst] then GST else VAT

taxPct :: Regime -> Maybe TaxConfig -> Maybe Double
taxPct taxRegime mbConfig = case taxRegime of
  NONE -> Nothing
  _ -> mfilter (> 0) ((.tax) <$> mbConfig)

pctRatio :: Double -> HighPrecMoney
pctRatio pct = HighPrecMoney (toRational pct / 100)

taxOnExclusive :: Currency -> Regime -> Maybe TaxConfig -> HighPrecMoney -> Maybe TaxBreakup
taxOnExclusive currency taxRegime mbConfig base = do
  pct <- taxPct taxRegime mbConfig
  splitTax currency taxRegime mbConfig (roundAmountByCurrency' currency (base * pctRatio pct))

taxOnInclusive :: Currency -> Regime -> Maybe TaxConfig -> HighPrecMoney -> Maybe TaxBreakup
taxOnInclusive currency taxRegime mbConfig gross = do
  pct <- taxPct taxRegime mbConfig
  splitTax currency taxRegime mbConfig (gross - roundAmountByCurrency' currency (gross / (1 + pctRatio pct)))

splitTax :: Currency -> Regime -> Maybe TaxConfig -> HighPrecMoney -> Maybe TaxBreakup
splitTax currency taxRegime mbConfig taxAmount
  | taxAmount == 0 = Nothing
  | otherwise = case taxRegime of
    NONE -> Nothing
    VAT -> Just TaxBreakup {total = taxAmount, cgst = Nothing, sgst = Nothing, igst = Nothing}
    GST ->
      let (cgstPart, sgstPart, igstPart) = splitGst currency weights taxAmount
       in Just TaxBreakup {total = taxAmount, cgst = cgstPart, sgst = sgstPart, igst = igstPart}
  where
    weights = case mbConfig of
      Just t | any isJust [t.cgst, t.sgst, t.igst] -> (share t.cgst, share t.sgst, share t.igst)
      _ -> (1, 1, 0)
    share = maybe 0 toRational
