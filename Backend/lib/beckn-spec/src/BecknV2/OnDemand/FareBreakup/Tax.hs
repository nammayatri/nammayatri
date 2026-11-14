module BecknV2.OnDemand.FareBreakup.Tax
  ( Regime (..),
    TaxConfig (..),
    RiderTaxRates (..),
    DriverTaxRates (..),
    DriverDeductibleTaxRates (..),
    TaxBreakup (..),
    FareTaxes,
    TaxedAmount (..),
    TaxableAmount (..),
    taxRateOf,
    taxOn,
    taxed,
    splitTax,
    taxedTotal,
    taxRetainedBySeller,
    mkTaxBreakup,
    taxTotal,
    addTaxBreakup,
    subtractTaxBreakup,
    chargeRegime,
  )
where

import BecknV2.OnDemand.FareBreakup.Core (RideTaxRates)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))

data Regime = GST | VAT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data TaxConfig = TaxConfig
  { tax :: Double,
    cgst :: Maybe Double,
    sgst :: Maybe Double,
    igst :: Maybe Double
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data RiderTaxRates = RiderTaxRates
  { regime :: Regime,
    riderPayable :: RideTaxRates (Maybe TaxConfig)
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DriverDeductibleTaxRates = DriverDeductibleTaxRates
  { commission :: Maybe TaxConfig,
    paymentCharge :: Maybe TaxConfig
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DriverTaxRates = DriverTaxRates
  { regime :: Regime,
    riderPayable :: RideTaxRates (Maybe TaxConfig),
    driverDeductible :: DriverDeductibleTaxRates
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

data TaxBreakup = TaxBreakup
  { total :: HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    igst :: Maybe HighPrecMoney
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

type FareTaxes = RideTaxRates (Maybe TaxBreakup)

data TaxedAmount = TaxedAmount
  { taxExclusive :: HighPrecMoney,
    tax :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data TaxableAmount = Inclusive HighPrecMoney | Exclusive HighPrecMoney
  deriving (Show, Eq)

taxRateOf :: Maybe TaxConfig -> HighPrecMoney
taxRateOf mbConfig = maybe 0 pctRatio (taxPct mbConfig)

taxOn :: Regime -> Maybe TaxConfig -> TaxableAmount -> Maybe TaxBreakup
taxOn taxRegime mbConfig taxable = do
  pct <- taxPct mbConfig
  splitTax taxRegime mbConfig $ case taxable of
    Exclusive base -> base * pctRatio pct
    Inclusive gross -> gross - gross / (1 + pctRatio pct)

taxed :: Regime -> Maybe TaxConfig -> TaxableAmount -> TaxedAmount
taxed taxRegime mbConfig taxable =
  let taxAmount = taxTotal (taxOn taxRegime mbConfig taxable)
   in case taxable of
        Exclusive base -> TaxedAmount {taxExclusive = base, tax = taxAmount}
        Inclusive gross -> TaxedAmount {taxExclusive = gross - taxAmount, tax = taxAmount}

taxedTotal :: TaxedAmount -> HighPrecMoney
taxedTotal amount = amount.taxExclusive + amount.tax

taxRetainedBySeller :: Maybe Regime -> Bool
taxRetainedBySeller = (/= Just GST)

mkTaxBreakup :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe TaxBreakup
mkTaxBreakup mbTotal cgstPart sgstPart igstPart = (\totalTax -> TaxBreakup {total = totalTax, cgst = cgstPart, sgst = sgstPart, igst = igstPart}) <$> mbTotal

taxTotal :: Maybe TaxBreakup -> HighPrecMoney
taxTotal = maybe 0 (.total)

addTaxBreakup :: Maybe TaxBreakup -> Maybe TaxBreakup -> Maybe TaxBreakup
addTaxBreakup (Just a) (Just b) = Just TaxBreakup {total = a.total + b.total, cgst = addPart a.cgst b.cgst, sgst = addPart a.sgst b.sgst, igst = addPart a.igst b.igst}
  where
    addPart x y = if isNothing x && isNothing y then Nothing else Just (fromMaybe 0 x + fromMaybe 0 y)
addTaxBreakup a b = maybe b Just a

subtractTaxBreakup :: Maybe TaxBreakup -> Maybe TaxBreakup -> Maybe TaxBreakup
subtractTaxBreakup (Just a) (Just b) = Just TaxBreakup {total = max 0 (a.total - b.total), cgst = subPart a.cgst b.cgst, sgst = subPart a.sgst b.sgst, igst = subPart a.igst b.igst}
  where
    subPart x y = (\v -> max 0 (v - fromMaybe 0 y)) <$> x
subtractTaxBreakup a _ = a

chargeRegime :: TaxConfig -> Regime
chargeRegime t = if any isJust [t.cgst, t.sgst, t.igst] then GST else VAT

taxPct :: Maybe TaxConfig -> Maybe Double
taxPct mbConfig = mfilter (> 0) ((.tax) <$> mbConfig)

pctRatio :: Double -> HighPrecMoney
pctRatio pct = HighPrecMoney (toRational pct / 100)

splitTax :: Regime -> Maybe TaxConfig -> HighPrecMoney -> Maybe TaxBreakup
splitTax taxRegime mbConfig taxAmount
  | taxAmount == 0 = Nothing
  | taxRegime == VAT = Just TaxBreakup {total = taxAmount, cgst = Nothing, sgst = Nothing, igst = Nothing}
  | otherwise = Just TaxBreakup {total = taxAmount, cgst = part cgstShare, sgst = part sgstShare, igst = part igstShare}
  where
    (cgstShare, sgstShare, igstShare) = case mbConfig of
      Just t
        | any ((> 0) . share) [t.cgst, t.sgst, t.igst] -> (share t.cgst, share t.sgst, share t.igst)
        | any isJust [t.cgst, t.sgst, t.igst] -> (1, 0, 0)
      _ -> (1, 1, 0)
    share = maybe 0 (max 0 . toRational)
    part s = mfilter (/= 0) (Just (taxAmount * HighPrecMoney (s / (cgstShare + sgstShare + igstShare))))
