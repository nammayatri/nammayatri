{-# LANGUAGE DerivingStrategies #-}

-- | The pure core of tax and commission derivation.
--
--   'expandPosting' is total and does no IO: given the treatment that applies
--   and the pair a call site named, it returns the full list of ledger legs.
--   That is what makes the migration testable offline — a corpus of real
--   @fare_parameters@ rows can be replayed through the old hand-written leg
--   lists and this expander, and the two compared, without a database.
--
--   Reference types are 'Text' here: the kernel does not own the vocabulary.
--   The names of derived legs come from the caller via 'DerivedRefs'.
module Lib.Finance.Posting
  ( PaymentMode (..),
    LegFlow (..),
    TaxKind (..),
    DerivedRefs (..),
    noDerivedRefs,
    Env (..),
    Posting (..),
    Leg (..),
    expandPosting,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
import Lib.Finance.Domain.Types.DirectTaxTransaction (TdsRateReason)
import Lib.Finance.Domain.Types.FinanceRefTypeConfig (FinanceRefTypeConfig)
import Lib.Finance.Types.AccountRole
import Lib.Finance.Types.ChargeValue
import Lib.Finance.Types.RefTypeConfig

data PaymentMode = Online | Cash
  deriving stock (Eq, Show, Generic)

-- | Whether money is flowing toward the payee (a charge) or back out to the
--   buyer (a refund). Refunds mirror the direction rules: the government leg
--   becomes a source rather than a destination.
data LegFlow = ChargeLeg | RefundLeg
  deriving stock (Eq, Show, Generic)

data TaxKind = IndirectTax | DirectTax
  deriving stock (Eq, Show, Generic)

-- | The ref types a derived leg posts under, supplied by the caller because
--   the kernel does not own the vocabulary.
data DerivedRefs = DerivedRefs
  { indirectTaxRef :: Maybe Text,
    directTaxRef :: Maybe Text,
    commissionRef :: Maybe Text,
    -- | Ref types reached by recursion, e.g. the commission's own tax leg.
    commissionDerived :: Maybe DerivedRefs,
    flow :: LegFlow,
    -- | An uncollected funding leg posted before everything else, for the
    --   2-leg online pass-through (@BuyerAsset -> payer@).
    fundingLeg :: Maybe AccountRole
  }

noDerivedRefs :: DerivedRefs
noDerivedRefs =
  DerivedRefs
    { indirectTaxRef = Nothing,
      directTaxRef = Nothing,
      commissionRef = Nothing,
      commissionDerived = Nothing,
      flow = ChargeLeg,
      fundingLeg = Nothing
    }

-- | Per-transaction facts the profile cannot carry, because they are about the
--   counterparty rather than the reference type.
data Env = Env
  { envMode :: PaymentMode,
    envTdsRateReason :: Maybe TdsRateReason,
    envTdsRateOverride :: Maybe ChargeValue,
    envCumulativeEarnings :: Maybe HighPrecMoney
  }

-- | What a call site says: two accounts, one amount, one reference type.
data Posting = Posting
  { refType :: Text,
    -- | @P@ — where the money comes from.
    payer :: AccountRole,
    -- | @R@ — who earns it.
    payee :: AccountRole,
    -- | Gross by default; the taxable base when the treatment is exclusive.
    amount :: HighPrecMoney
  }

data Leg = Leg
  { from :: AccountRole,
    to :: AccountRole,
    amount :: HighPrecMoney,
    refType :: Text,
    -- | 'False' posts through the non-collecting writer, so the leg never
    --   reaches an invoice.
    collect :: Bool,
    isDerivedTax :: Maybe TaxKind
  }

-- | The government-facing account for a derived leg.
--
--   In cash mode the net leg runs @BuyerControl -> OwnerControl@: tracking
--   accounts that exist so cash rides net to zero, because the rider paid the
--   driver directly and the money never entered our books. Any leg that
--   *leaves* that pair for a real counterparty must therefore be sourced from
--   'OwnerLiability', the driver's real liability account — otherwise a
--   government liability would be funded from a tracking account.
realPayee :: Env -> AccountRole -> AccountRole
realPayee env r = case env.envMode of
  Cash -> OwnerLiability
  Online -> r

-- | Rule 2 — where the indirect-tax leg lands.
taxLegs :: Env -> LegFlow -> IndirectTaxRemittanceDirection -> AccountRole -> AccountRole -> HighPrecMoney -> Text -> [Leg]
taxLegs env flow' direction p r amt ref = case (flow', direction) of
  (ChargeLeg, Owner) -> [leg p r True]
  (ChargeLeg, CompanyDirect) -> [leg (govtSource p) GovtIndirect True]
  (ChargeLeg, CompanyIndirect) -> [leg p r True, leg (realPayee env r) GovtIndirect False]
  (RefundLeg, Owner) -> [leg p r True]
  (RefundLeg, CompanyDirect) -> [leg GovtIndirect r True]
  (RefundLeg, CompanyIndirect) -> [leg GovtIndirect p False, leg p r True]
  where
    govtSource payer' = case env.envMode of
      Cash -> OwnerLiability
      Online -> payer'
    leg f t c = Leg {from = f, to = t, amount = amt, refType = ref, collect = c, isDerivedTax = Just IndirectTax}

-- | Total. No IO, no failure, no config lookup.
expandPosting :: HM.HashMap Text FinanceRefTypeConfig -> Env -> DerivedRefs -> Posting -> [Leg]
expandPosting profile env refs posting =
  case HM.lookup posting.refType profile of
    -- No treatment: exactly what a plain transfer does today.
    Nothing -> [netLeg posting.amount]
    Just t ->
      let (net, tax) = split t posting.amount
       in funding (net + tax)
            <> netOrRoutedLeg t net
            <> indirectLegs t tax
            <> directLegs t net
            <> commissionLegs t net
  where
    p = posting.payer
    r = posting.payee

    netLeg amt = Leg {from = p, to = r, amount = amt, refType = posting.refType, collect = True, isDerivedTax = Nothing}

    split t amt = case t.taxRate of
      Nothing -> (amt, 0)
      Just rate
        | t.isTaxExclusive -> (amt, applyRate rate amt)
        | otherwise -> let tx = extractFromGross rate amt in (amt - tx, tx)

    -- The 2-leg online pass-through the domain writes by hand today.
    funding total = case refs.fundingLeg of
      Nothing -> []
      Just src ->
        [Leg {from = src, to = p, amount = total, refType = posting.refType, collect = False, isDerivedTax = Nothing}]

    -- When a ref type carries a direction but no rate, the caller has supplied
    -- the tax amount itself and this posting *is* the tax leg — so it takes the
    -- routed pair rather than the caller's.
    netOrRoutedLeg t net = case (t.taxRate, t.indirectTaxDirection) of
      (Nothing, Just direction) ->
        map (\l -> l {refType = posting.refType, isDerivedTax = Just IndirectTax}) $
          taxLegs env refs.flow direction p r net posting.refType
      _ -> [netLeg net]

    indirectLegs t tax
      | tax <= 0 = []
      | otherwise = case (t.indirectTaxDirection, refs.indirectTaxRef) of
        (Just direction, Just ref) -> taxLegs env refs.flow direction p r tax ref
        _ -> []

    directLegs t net = fromMaybe [] $ do
      ref <- refs.directTaxRef
      amt <- getDirectTaxForTreatment t
      guard (amt > 0)
      pure [Leg {from = realPayee env r, to = GovtDirect, amount = amt, refType = ref, collect = True, isDerivedTax = Just DirectTax}]
      where
        getDirectTaxForTreatment tt = do
          table <- tt.directTaxRates
          rate <- env.envTdsRateOverride <|> (env.envTdsRateReason >>= \reason -> lookup reason table.rates)
          let amount = applyRate rate net
          pure $ case (table.threshold, env.envCumulativeEarnings) of
            (Just limit, Just earned) | earned < limit -> 0
            _ -> amount

    -- Commission is a rewrite of the same pair, expanded recursively so its own
    -- tax falls out of the same rules. Depth is 1: a tax ref type never carries
    -- a rate, which the seeding invariant enforces.
    commissionLegs t net = fromMaybe [] $ do
      rate <- t.commissionValue
      ref <- refs.commissionRef
      let amt = applyRate rate net
      guard (amt > 0)
      let inner = fromMaybe noDerivedRefs {flow = refs.flow} refs.commissionDerived
      pure $ expandPosting profile env inner (Posting {refType = ref, payer = realPayee env r, payee = SellerRevenue, amount = amt})
