-- | Canonical algorithm for applying a BAP customer offer discount to the
--   BPP fare breakup, with VAT recomputation over the post-discount
--   taxable base.
--
--   The BPP emits twelve numeric tags in quotation.breakup. Ten partition the
--   fare into five categories × {tax-exclusive, tax}:
--
--     - RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE / _TAX
--     - RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE / _TAX
--     - TOLL_FARE_TAX_EXCLUSIVE / TOLL_FARE_TAX
--     - CANCELLATION_FEE_TAX_EXCLUSIVE / CANCELLATION_TAX
--     - PARKING_CHARGE_TAX_EXCLUSIVE / PARKING_CHARGE_TAX
--
--   and two carry the payment-gateway charge levied on top of them:
--
--     - PAYMENT_CHARGE_TAX_EXCLUSIVE / PAYMENT_CHARGE_TAX
--
--   Invariant: 'projectFareParamsBreakupTotal' of all twelve equals what the
--   customer actually pays. 'fareOnlyTotal' of the first ten is the base the
--   payment charge is levied on. For a normal completed ride the cancellation
--   pair is zero; for a cancellation event the ride pair is zero and the
--   cancellation pair carries the full amount.
--
--   The customer offer discount applies only to the discount-applicable ride
--   pair. We back-calculate VAT from the post-discount tax-inclusive amount by
--   scaling each component (taxExcl, tax) by ratio = basePostDiscount / base.
--   This correctly handles the tax-inclusive base: solving x + rate*x = basePostDiscount
--   gives x = basePostDiscount / (1 + rate), which is equivalent to taxExcl * ratio.
--
--   Non-discountable ride, toll and parking are carried through untouched. The
--   payment charge is NOT scaled by the discount ratio -- it is re-derived from
--   the post-discount fare total by 'applyPaymentChargeSlots', because it is
--   levied on what the customer ends up paying, not on what they were quoted.
module Domain.SharedLogic.RideDiscount
  ( ProjectFareParamsBreakup (..),
    RideDiscountResult (..),
    PaymentChargeRate (..),
    projectFareParamsBreakupTotal,
    fareOnlyTotal,
    discountApplicableBase,
    clampDiscount,
    applyRideDiscount,
    applyPaymentChargeSlots,
    paymentChargeRateFromBreakup,
    applyDiscountAndRepriceCharge,
    parseProjectFareParamsBreakup,
    parsePaymentChargeRate,
    projectedFareParamTags,
    isProjectedFareParamTag,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

projectedFareParamTags :: [Text]
projectedFareParamTags =
  [ show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE,
    show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX,
    show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE,
    show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX,
    show Enums.TOLL_FARE_TAX_EXCLUSIVE,
    show Enums.TOLL_FARE_TAX,
    show Enums.CANCELLATION_FEE_TAX_EXCLUSIVE,
    show Enums.CANCELLATION_TAX,
    show Enums.PARKING_CHARGE_TAX_EXCLUSIVE,
    show Enums.PARKING_CHARGE_TAX,
    show Enums.PAYMENT_CHARGE_TAX_EXCLUSIVE,
    show Enums.PAYMENT_CHARGE_TAX,
    show Enums.PAYMENT_CHARGE_RATE,
    show Enums.PAYMENT_CHARGE_VAT_PCT
  ]

isProjectedFareParamTag :: Text -> Bool
isProjectedFareParamTag t = t `elem` projectedFareParamTags

data ProjectFareParamsBreakup = ProjectFareParamsBreakup
  { discountApplicableRideFareTaxExclusive :: HighPrecMoney,
    discountApplicableRideFareTax :: HighPrecMoney,
    nonDiscountApplicableRideFareTaxExclusive :: HighPrecMoney,
    nonDiscountApplicableRideFareTax :: HighPrecMoney,
    tollFareTaxExclusive :: HighPrecMoney,
    tollFareTax :: HighPrecMoney,
    cancellationFeeTaxExclusive :: HighPrecMoney,
    cancellationTax :: HighPrecMoney,
    parkingChargeTaxExclusive :: HighPrecMoney,
    parkingChargeTax :: HighPrecMoney,
    paymentChargeTaxExclusive :: HighPrecMoney,
    paymentChargeTax :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RideDiscountResult = RideDiscountResult
  { postDiscountApplicableTaxExclusive :: HighPrecMoney,
    postDiscountApplicableTax :: HighPrecMoney,
    -- | rideTax (pre-discount) − postDiscountApplicableTax. The platform
    --   absorbs this portion of VAT: the customer didn't pay it, so
    --   the government isn't owed it; it becomes a platform expense.
    rideVatAbsorbedOnDiscount :: HighPrecMoney,
    -- | What was actually applied after clamping to [0, discountApplicableBase].
    clampedDiscount :: HighPrecMoney,
    -- | (base − clampedDiscount) / base. Equals 1 when no discount or base = 0.
    discountRatio :: Rational
  }
  deriving (Show, Eq, Generic)

-- | The ten fare slots: the base the payment charge is levied on.
fareOnlyTotal :: ProjectFareParamsBreakup -> HighPrecMoney
fareOnlyTotal b =
  b.discountApplicableRideFareTaxExclusive
    + b.discountApplicableRideFareTax
    + b.nonDiscountApplicableRideFareTaxExclusive
    + b.nonDiscountApplicableRideFareTax
    + b.tollFareTaxExclusive
    + b.tollFareTax
    + b.cancellationFeeTaxExclusive
    + b.cancellationTax
    + b.parkingChargeTaxExclusive
    + b.parkingChargeTax

-- | All twelve slots: what the customer actually pays.
projectFareParamsBreakupTotal :: ProjectFareParamsBreakup -> HighPrecMoney
projectFareParamsBreakupTotal b =
  fareOnlyTotal b + b.paymentChargeTaxExclusive + b.paymentChargeTax

-- | The rate a payment charge was priced at, as emitted by the BPP so the BAP
--   can re-derive the charge itself rather than rescaling a pre-discount figure.
data PaymentChargeRate = PaymentChargeRate
  { ratePct :: HighPrecMoney,
    vatPct :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Re-derive both payment-charge slots from 'fareOnlyTotal'. 'Nothing' (no
--   rate configured, or a bearer other than the rider) zeroes them.
applyPaymentChargeSlots :: Maybe PaymentChargeRate -> ProjectFareParamsBreakup -> ProjectFareParamsBreakup
applyPaymentChargeSlots mbRate b =
  case mbRate of
    Just rate
      | rate.ratePct > 0,
        base > 0 ->
        let charge = base * rate.ratePct / 100
         in b {paymentChargeTaxExclusive = charge, paymentChargeTax = charge * rate.vatPct / 100}
    _ -> b {paymentChargeTaxExclusive = 0, paymentChargeTax = 0}
  where
    base = fareOnlyTotal b

-- | Recover the rate a breakup's payment charge was priced at, from the charge
--   itself. Exact: the emitter computed charge = ratePct% x fareOnlyTotal, so
--   dividing recovers ratePct with no loss. Both the BPP (when emitting
--   PAYMENT_CHARGE_RATE) and the BAP (when re-pricing on the post-discount base)
--   go through this, so neither can drift from the other.
--
--   'Nothing' when there is no charge to re-price -- no rate configured, or a
--   bearer other than the rider -- in which case the slots stay as sent.
paymentChargeRateFromBreakup :: ProjectFareParamsBreakup -> Maybe PaymentChargeRate
paymentChargeRateFromBreakup b
  | b.paymentChargeTaxExclusive <= 0 || base <= 0 = Nothing
  | otherwise =
    Just
      PaymentChargeRate
        { ratePct = b.paymentChargeTaxExclusive / base * 100,
          vatPct = b.paymentChargeTax / b.paymentChargeTaxExclusive * 100
        }
  where
    base = fareOnlyTotal b

-- | The canonical post-offer breakup: discount the fare slots, then re-price the
--   payment charge on what is left. Both the BAP (for the quoted post-offer
--   amount) and the BPP (for the fare it actually captures) must go through
--   this, or the quote and the capture will not reconcile.
applyDiscountAndRepriceCharge ::
  Maybe PaymentChargeRate ->
  ProjectFareParamsBreakup ->
  HighPrecMoney ->
  (ProjectFareParamsBreakup, RideDiscountResult)
applyDiscountAndRepriceCharge mbRate b rawDiscount =
  let r = applyRideDiscount b rawDiscount
      discounted =
        b
          { discountApplicableRideFareTaxExclusive = r.postDiscountApplicableTaxExclusive,
            discountApplicableRideFareTax = r.postDiscountApplicableTax
          }
   in (applyPaymentChargeSlots mbRate discounted, r)

discountApplicableBase :: ProjectFareParamsBreakup -> HighPrecMoney
discountApplicableBase b = b.discountApplicableRideFareTaxExclusive + b.discountApplicableRideFareTax

-- | Clamp a raw discount to [0, discountApplicableBase]. Non-discountable
--   ride and toll are excluded by construction (they're not part of the base).
clampDiscount :: ProjectFareParamsBreakup -> HighPrecMoney -> HighPrecMoney
clampDiscount b raw = max 0 (min raw (discountApplicableBase b))

-- | Apply the customer discount to the discount-applicable pair and
--   recompute ride VAT over the post-discount inclusive amount.
--
--   Algorithm:
--     base             = discountApplicableRideFareTaxExclusive + discountApplicableRideFareTax
--     basePostDiscount = base − clampedDiscount
--     ratio            = basePostDiscount / base
--     postExcl         = taxExclPreDiscount × ratio   -- back-calculate from tax-inclusive total
--     postTax          = taxPreDiscount × ratio
--     absorbedVat      = preTax − postTax             -- VAT lost to the discount (platform covers)
--
--   Both components scale by the same ratio because basePostDiscount is tax-inclusive.
--   Equivalently: postExcl = basePostDiscount / (1 + taxRate), postTax = basePostDiscount − postExcl.
--
--   Invariant: postExcl + postTax = basePostDiscount = base − clampedDiscount.
--   absorbedVat is a separate platform expense, not part of the fare partition.
applyRideDiscount :: ProjectFareParamsBreakup -> HighPrecMoney -> RideDiscountResult
applyRideDiscount b rawDiscount
  | base <= 0 || clamped <= 0 = noOpResult
  | otherwise =
    RideDiscountResult
      { postDiscountApplicableTaxExclusive = postExcl,
        postDiscountApplicableTax = postTax,
        rideVatAbsorbedOnDiscount = taxPreDiscount - postTax,
        clampedDiscount = clamped,
        discountRatio = ratio
      }
  where
    clamped = clampDiscount b rawDiscount
    base = discountApplicableBase b
    taxExclPreDiscount = b.discountApplicableRideFareTaxExclusive
    taxPreDiscount = b.discountApplicableRideFareTax
    basePostDiscount = base - clamped
    ratio = toRational basePostDiscount / toRational base
    postTax = fromRational (toRational taxPreDiscount * ratio)
    postExcl = fromRational (toRational taxExclPreDiscount * ratio)
    noOpResult =
      RideDiscountResult
        { postDiscountApplicableTaxExclusive = taxExclPreDiscount,
          postDiscountApplicableTax = taxPreDiscount,
          rideVatAbsorbedOnDiscount = 0,
          clampedDiscount = 0,
          discountRatio = 1
        }

-- | Parse a breakup list into the canonical summary. Returns 'Nothing' when
--   none of the ride/cancellation/parking category tags are present — caller
--   should @logError@ and fall back to estimatedFare-based math (the
--   VAT-recompute algorithm can't run on a non-compliant BPP).
--
--   The payment-charge tags are deliberately NOT category sentinels, for the
--   same reason toll is not: a payload carrying only a charge would otherwise
--   parse as a breakup of zeroes and price offers against nothing.
parseProjectFareParamsBreakup :: [(Text, HighPrecMoney)] -> Maybe ProjectFareParamsBreakup
parseProjectFareParamsBreakup pairs =
  let m = Map.fromList pairs
      get k = Map.findWithDefault 0 k m
      hasCategoryTag =
        Map.member "RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE" m
          || Map.member "RIDE_FARE_DISCOUNT_APPLICABLE_TAX" m
          || Map.member "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE" m
          || Map.member "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX" m
          || Map.member "CANCELLATION_FEE_TAX_EXCLUSIVE" m
          || Map.member "CANCELLATION_TAX" m
          || Map.member "PARKING_CHARGE_TAX_EXCLUSIVE" m
          || Map.member "PARKING_CHARGE_TAX" m
   in if hasCategoryTag
        then
          Just
            ProjectFareParamsBreakup
              { discountApplicableRideFareTaxExclusive = get "RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                discountApplicableRideFareTax = get "RIDE_FARE_DISCOUNT_APPLICABLE_TAX",
                nonDiscountApplicableRideFareTaxExclusive = get "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                nonDiscountApplicableRideFareTax = get "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX",
                tollFareTaxExclusive = get "TOLL_FARE_TAX_EXCLUSIVE",
                tollFareTax = get "TOLL_FARE_TAX",
                cancellationFeeTaxExclusive = get "CANCELLATION_FEE_TAX_EXCLUSIVE",
                cancellationTax = get "CANCELLATION_TAX",
                parkingChargeTaxExclusive = get "PARKING_CHARGE_TAX_EXCLUSIVE",
                parkingChargeTax = get "PARKING_CHARGE_TAX",
                paymentChargeTaxExclusive = get "PAYMENT_CHARGE_TAX_EXCLUSIVE",
                paymentChargeTax = get "PAYMENT_CHARGE_TAX"
              }
        else Nothing

-- | The rate the BPP priced the payment charge at. 'Nothing' when the BPP did
--   not emit it (older deploy, or a bearer other than the rider), in which case
--   the charge slots must be taken as-sent rather than re-derived.
parsePaymentChargeRate :: [(Text, HighPrecMoney)] -> Maybe PaymentChargeRate
parsePaymentChargeRate pairs =
  let m = Map.fromList pairs
      rateTag = show Enums.PAYMENT_CHARGE_RATE
   in if Map.member rateTag m
        then
          Just
            PaymentChargeRate
              { ratePct = Map.findWithDefault 0 rateTag m,
                vatPct = Map.findWithDefault 0 (show Enums.PAYMENT_CHARGE_VAT_PCT) m
              }
        else Nothing
