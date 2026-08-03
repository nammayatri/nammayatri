-- | Reading a catalogue row.
--
--   There is no separate treatment or profile type: the unit of treatment is
--   'FinanceRefTypeConfig' itself, and the treatment in force for a transaction
--   is a @HashMap Text FinanceRefTypeConfig@ keyed by reference type. A row read
--   from the catalogue and a row reconstructed from a ledger entry's stamp are
--   then the same type, so nothing has to be kept in step with the table.
module Lib.Finance.Types.RefTypeConfig
  ( module Lib.Finance.Types.TaxRate,
    splitByTreatment,
    getIndirectTaxFor,
    getCommissionFor,
    getDirectTaxFor,
    getIndirectTaxDirectionFor,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
import Lib.Finance.Domain.Types.DirectTaxTransaction (TdsRateReason)
import Lib.Finance.Domain.Types.FinanceRefTypeConfig (FinanceRefTypeConfig)
import Lib.Finance.Types.ChargeValue
import Lib.Finance.Types.TaxRate

-- | @(net, tax)@ for an amount under a reference type's row.
--
--   'Nothing' means the ref type has no row — distinguishable from a rate
--   configured as zero, so a caller can fall back to fare policy.
getIndirectTaxFor :: HM.HashMap Text FinanceRefTypeConfig -> Text -> HighPrecMoney -> Maybe (HighPrecMoney, HighPrecMoney)
getIndirectTaxFor profile refType amount = do
  t <- HM.lookup refType profile
  pure (splitByTreatment t amount)

-- | Split an amount per a row's rate and basis. Total silently returns
--   @(amount, 0)@ when no rate is configured, which is today's behaviour.
splitByTreatment :: FinanceRefTypeConfig -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitByTreatment t amount = case t.taxRate of
  Nothing -> (amount, 0)
  Just rate
    | t.isTaxExclusive -> (amount, applyRate rate amount)
    | otherwise -> let tax = extractFromGross rate amount in (amount - tax, tax)

getCommissionFor :: HM.HashMap Text FinanceRefTypeConfig -> Text -> HighPrecMoney -> Maybe HighPrecMoney
getCommissionFor profile refType net = do
  t <- HM.lookup refType profile
  rate <- t.commissionValue
  pure (applyRate rate net)

-- | TDS for a cohort, gated by cumulative earnings against the threshold.
--   @override@ is the per-driver materialised rate, which wins when present.
getDirectTaxFor ::
  HM.HashMap Text FinanceRefTypeConfig ->
  Text ->
  Maybe TdsRateReason ->
  Maybe ChargeValue ->
  Maybe HighPrecMoney ->
  HighPrecMoney ->
  Maybe HighPrecMoney
getDirectTaxFor profile refType mbReason mbOverride mbCumulative net = do
  t <- HM.lookup refType profile
  table <- t.directTaxRates
  rate <- mbOverride <|> (mbReason >>= \r -> lookup r table.rates)
  let amount = applyRate rate net
  pure $ case (table.threshold, mbCumulative) of
    (Just limit, Just earned) | earned < limit -> 0
    _ -> amount

getIndirectTaxDirectionFor :: HM.HashMap Text FinanceRefTypeConfig -> Text -> Maybe IndirectTaxRemittanceDirection
getIndirectTaxDirectionFor profile refType =
  HM.lookup refType profile >>= (.indirectTaxDirection)
