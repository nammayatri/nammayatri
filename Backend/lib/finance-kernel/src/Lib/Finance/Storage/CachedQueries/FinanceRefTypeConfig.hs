-- | Reading the ref-type catalogue, and resolving the treatment that governs a
--   transaction.
--
--   Two sources, in order:
--
--   1. The treatment stamped on the transaction's earlier ledger legs. A refund
--      months later then expands exactly as the charge it reverses did, even if
--      the catalogue was edited in between.
--   2. Failing that — this is the transaction's first leg — the current
--      catalogue for the city.
--
--   Both produce the same thing: catalogue rows keyed by reference type.
module Lib.Finance.Storage.CachedQueries.FinanceRefTypeConfig
  ( profileFromCatalogue,
    profileFromEntries,
    treatmentOfEntry,
  )
where

import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id (Id (..))
import qualified Lib.Finance.Domain.Types.FinanceRefTypeConfig as DRC
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.FinanceRefTypeConfig as QRC

-- | The treatment for a transaction's first leg: every enabled catalogue row
--   for the city, keyed by reference type.
--
--   The table is registered with ConfigPilot (see each app's
--   @Storage.ConfigPilot.Config.FinanceRefTypeConfig@), which is where the
--   rollout and update APIs come from. The read itself is the plain query: the
--   @ConfigDimensions@ instance cannot live in this library, because
--   @getConfigList@ fixes its constraints and the table's @HasSchemaName@ is
--   still a constraint here rather than a concrete instance.
--
--   So the caching is done here instead, with the same 'withInMemCache' the
--   framework uses. Catalogue rows are ops-edited and rarely change, and this
--   read sits on the EndRide path, so a DB round trip per transaction is not
--   affordable. The empty map is cached too: an unconfigured city costs one
--   query an hour and nothing after.
profileFromCatalogue :: (BeamFlow.BeamFlow m r) => Text -> m (HM.HashMap Text DRC.FinanceRefTypeConfig)
profileFromCatalogue mocId =
  IM.withInMemCache ["FinanceRefTypeConfig", mocId] 3600 $ do
    rows <- QRC.findAllByOpCity mocId
    pure $ HM.fromList [(r.referenceType, r) | r <- rows, r.enabled]

-- | The row a ledger entry was posted under, reconstructed from its stamp.
--
--   The identity fields are taken from the entry rather than the catalogue: the
--   original row may since have been edited or deleted, and what governs this
--   transaction is what was applied, not what the table says now.
treatmentOfEntry :: LE.LedgerEntry -> Maybe DRC.FinanceRefTypeConfig
treatmentOfEntry e =
  case (e.appliedTaxRate, e.appliedIndirectTaxDirection, e.appliedCommissionValue, e.appliedDirectTaxRates) of
    (Nothing, Nothing, Nothing, Nothing) -> Nothing
    _ ->
      Just
        DRC.FinanceRefTypeConfig
          { id = Id e.id.getId,
            merchantId = e.merchantId,
            merchantOperatingCityId = e.merchantOperatingCityId,
            referenceType = e.referenceType,
            taxRate = e.appliedTaxRate,
            isTaxExclusive = fromMaybe False e.appliedTaxExclusive,
            indirectTaxDirection = e.appliedIndirectTaxDirection,
            commissionValue = e.appliedCommissionValue,
            directTaxRates = e.appliedDirectTaxRates,
            enabled = True,
            createdAt = e.createdAt,
            updatedAt = e.updatedAt
          }

-- | Rebuild the treatment from what a transaction already posted. Empty when no
--   prior leg carries a stamp, which is the signal to fall back to the
--   catalogue.
profileFromEntries :: [LE.LedgerEntry] -> HM.HashMap Text DRC.FinanceRefTypeConfig
profileFromEntries entries =
  HM.fromList [(e.referenceType, t) | e <- entries, Just t <- [treatmentOfEntry e]]
