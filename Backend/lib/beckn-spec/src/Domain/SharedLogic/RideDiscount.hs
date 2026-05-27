-- | Canonical algorithm for applying a BAP customer offer discount to the
--   BPP fare breakup, with VAT recomputation over the post-discount
--   taxable base.
--
--   The BPP emits eight numeric tags in quotation.breakup that partition
--   the entire fare sum into four categories × {tax-exclusive, tax}:
--
--     - RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE / _TAX
--     - RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE / _TAX
--     - TOLL_FARE_TAX_EXCLUSIVE / TOLL_FARE_TAX
--     - CANCELLATION_FEE_TAX_EXCLUSIVE / CANCELLATION_TAX
--
--   Invariant: 'projectFareParamsBreakupTotal' of the eight numbers
--   equals the BPP fare sum. For a normal completed ride the
--   cancellation pair is zero; for a cancellation event the ride pair
--   is zero and the cancellation pair carries the full amount.
--
--   The customer offer discount applies only to the discount-applicable ride
--   pair. We back-calculate VAT from the post-discount tax-inclusive amount by
--   scaling each component (taxExcl, tax) by ratio = basePostDiscount / base.
--   This correctly handles the tax-inclusive base: solving x + rate*x = basePostDiscount
--   gives x = basePostDiscount / (1 + rate), which is equivalent to taxExcl * ratio.
--
--   Non-discountable ride and toll are carried through untouched.
module Domain.SharedLogic.RideDiscount
  ( ProjectFareParamsBreakup (..),
    RideDiscountResult (..),
    projectFareParamsBreakupTotal,
    discountApplicableBase,
    clampDiscount,
    applyRideDiscount,
    parseProjectFareParamsBreakup,
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
    show Enums.PARKING_CHARGE_TAX
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
    parkingChargeTax :: HighPrecMoney
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

projectFareParamsBreakupTotal :: ProjectFareParamsBreakup -> HighPrecMoney
projectFareParamsBreakupTotal b =
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

-- | Parse a breakup list into the canonical eight-tag summary. Returns
--   'Nothing' when none of the ride/cancellation category tags are
--   present — caller should @logError@ and fall back to estimatedFare-
--   based math (the VAT-recompute algorithm can't run on a
--   non-compliant BPP).
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
                parkingChargeTax = get "PARKING_CHARGE_TAX"
              }
        else Nothing
