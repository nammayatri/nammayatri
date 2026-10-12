module RideFinance.Core.Components
  ( FareComponent (..),
    allComponents,
    AmountPart (..),
    TaxedAmount (..),
    DeductionAmounts (..),
    ComponentSpec (..),
    componentSpec,
    earnedByDriver,
    isExternalComponent,
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import RideFinance.Core.Projection (ProjectFareParamsBreakup (..))

data FareComponent
  = RIDE_FARE
  | TOLL_FARE
  | CANCELLATION_FARE
  | PARKING_CHARGE
  | PAYMENT_CHARGE
  | COMMISSION
  | CANCELLATION_COMMISSION
  | AIRPORT_ENTRY_FEE
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema)

allComponents :: [FareComponent]
allComponents = [minBound .. maxBound]

data AmountPart = TaxExclusivePart | TaxPart | TaxInclusivePart
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data TaxedAmount = TaxedAmount
  { taxExclusive :: HighPrecMoney,
    tax :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DeductionAmounts = DeductionAmounts
  { commission :: TaxedAmount,
    cancellationCommission :: TaxedAmount,
    driverBorneCharge :: TaxedAmount
  }
  deriving (Show, Eq, Generic)

-- | The money semantics of one fare component, the single place that says where its amount comes from
--   and who it belongs to. Every calculation (charges, deductions, driver earnings, ledger legs, invoice
--   flags) is derived from this registry, so a new component is added by extending 'FareComponent' and
--   giving it a spec in 'componentSpec'.
--
--   Display keys (invoice descriptions, wallet ledger references, earnings row codes, ride-end posting
--   references) are deliberately not here: each surface owns its own 'Keys' mapping in its own domain.
data ComponentSpec = ComponentSpec
  { -- | What the customer is charged for this component, read from the projected fare-params breakup as
    --   (tax-exclusive, tax) parts. A component with several parts (e.g. ride fare: discount-applicable /
    --   non-discount-applicable, taxed / untaxed) keeps them separate so each taxed part carries a single
    --   rate. 'Nothing' means the customer is never charged for it.
    chargeFromCustomer :: Maybe (ProjectFareParamsBreakup -> [TaxedAmount]),
    -- | What is taken back from the driver for this component (commission, cancellation commission, the
    --   driver-borne payment charge). 'Nothing' means it is never deducted from the driver.
    --   A component the customer is charged for and that is not deducted is the driver's earning
    --   (see 'earnedByDriver').
    deductedFromDriver :: Maybe (DeductionAmounts -> TaxedAmount),
    -- | An external charge is collected from the customer on behalf of a third party (toll operator,
    --   parking operator) and passed through; it is not part of the ride's own taxable supply. Invoices
    --   flag such lines with @isExternalCharge@ so they are kept out of the taxable value.
    external :: Bool
  }

componentSpec :: FareComponent -> ComponentSpec
componentSpec = \case
  RIDE_FARE ->
    ComponentSpec
      { chargeFromCustomer = Just $ \b ->
          [ TaxedAmount {taxExclusive = b.discountApplicableRideFareTaxExclusive, tax = b.discountApplicableRideFareTax},
            TaxedAmount {taxExclusive = b.discountApplicableRideFareUntaxed, tax = 0},
            TaxedAmount {taxExclusive = b.nonDiscountApplicableRideFareTaxExclusive, tax = b.nonDiscountApplicableRideFareTax},
            TaxedAmount {taxExclusive = b.nonDiscountApplicableRideFareUntaxed, tax = 0}
          ],
        deductedFromDriver = Nothing,
        external = False
      }
  TOLL_FARE ->
    ComponentSpec
      { chargeFromCustomer = Just $ \b -> [TaxedAmount {taxExclusive = b.tollFareTaxExclusive, tax = b.tollFareTax}],
        deductedFromDriver = Nothing,
        external = True
      }
  PARKING_CHARGE ->
    ComponentSpec
      { chargeFromCustomer = Just $ \b -> [TaxedAmount {taxExclusive = b.parkingChargeTaxExclusive, tax = b.parkingChargeTax}],
        deductedFromDriver = Nothing,
        external = True
      }
  CANCELLATION_FARE ->
    ComponentSpec
      { chargeFromCustomer = Just $ \b -> [TaxedAmount {taxExclusive = b.cancellationFeeTaxExclusive, tax = b.cancellationTax}],
        deductedFromDriver = Nothing,
        external = False
      }
  PAYMENT_CHARGE ->
    ComponentSpec
      { chargeFromCustomer = Just $ \b -> [TaxedAmount {taxExclusive = b.paymentChargeTaxExclusive, tax = b.paymentChargeTax}],
        deductedFromDriver = Just (.driverBorneCharge),
        external = False
      }
  COMMISSION ->
    ComponentSpec
      { chargeFromCustomer = Nothing,
        deductedFromDriver = Just (.commission),
        external = False
      }
  CANCELLATION_COMMISSION ->
    ComponentSpec
      { chargeFromCustomer = Nothing,
        deductedFromDriver = Just (.cancellationCommission),
        external = False
      }
  AIRPORT_ENTRY_FEE ->
    ComponentSpec
      { chargeFromCustomer = Nothing,
        deductedFromDriver = Nothing,
        external = False
      }

earnedByDriver :: FareComponent -> Bool
earnedByDriver c = isJust spec.chargeFromCustomer && isNothing spec.deductedFromDriver
  where
    spec = componentSpec c

isExternalComponent :: FareComponent -> Bool
isExternalComponent c = (componentSpec c).external
