{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- NOTE (reviewer, remove before merge): NEW FILE (main has no bulk payout code). The read-only "who gets paid" pass of the
--   bulk lifecycle: it runs before the batch is opened, and Claim.hs runs the same classifyBulkCandidate again under the
--   wallet lock. Callers: ScheduledBatchPayout (inside `when (payoutServiceFlow == Payout.BulkFlow)` only), AdhocPayout
--   (bulk-only: lookup screen and initiate), Claim.hs, and the bulk instant payout (usableBankDetails only).
--   Juspay/Stripe impact: bulk-only. The Juspay/Stripe sweep forks WalletPayout.runWalletPayout per person and a
--   Juspay/Stripe city's instant payout runs the usual wallet payout; neither calls this module.

-- | Who gets paid. Read-only: one reader for everything a payout decision rests on, and one pure
-- function that decides. Nothing here writes a row or takes a lock.
module SharedLogic.Payout.Bulk.Eligibility
  ( BulkCandidate,
    PayoutSnapshot (..),
    checkBulkPayoutEligibility,
    readPayoutSnapshot,
    classifyBulkCandidate,
    usableBankDetails,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverBankAccount as DDBA
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Payout.Bulk.Types as Bulk
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import SharedLogic.Finance.Wallet
import qualified Storage.Queries.DriverBankAccount as QDBA

-- | A driver or fleet owner that passed the read-only eligibility pass.
type BulkCandidate = Bulk.BulkCandidate DP.Person

-- | Everything one payout decision rests on, read in a single place so the eligibility pass and
--   the claim that follows it cannot drift apart.
data PayoutSnapshot = PayoutSnapshot
  { payoutableBalance :: HighPrecMoney,
    redeemableEntryIds :: [Text],
    merchantTransferAmount :: HighPrecMoney,
    cutoff :: UTCTime,
    mbBankAccount :: Maybe DDBA.DriverBankAccount
  }

-- NOTE (reviewer, remove before merge): the payable amount comes from computePayoutableBalance, the same function
--   WalletPayout.findWalletPayoutAmount uses for Juspay/Stripe instant and sweep payouts.
--   As for Juspay/Stripe, there is no "payout already in flight" check: a person with an open payout can get a new payout
--   for money earned since. Paying the SAME money twice is still blocked: the hold moves the amount out of the wallet when
--   the payout is claimed, and the claim runs under the wallet lock -- the same protection Juspay/Stripe rely on.
--   Juspay/Stripe impact: bulk-only (callers: Claim.hs and checkBulkPayoutEligibility below).
readPayoutSnapshot ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r
  ) =>
  DTConf.TransporterConfig ->
  DP.Person ->
  m PayoutSnapshot
readPayoutSnapshot transporterConfig person = do
  now <- getCurrentTime
  let personId = person.id
      counterparty = counterpartyFromRole person.role
  mbBankAccount <- QDBA.findByPrimaryKey personId
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
      cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  -- The same payable amount instant payout uses: the redeemable balance, less ride and offer holds.
  -- The redeemable balance already excludes what is parked in OwnerPayoutLiability for payouts
  -- still in flight, because the hold debits the wallet at claim time -- so a second sweep cannot
  -- see the same money as payable. As on Juspay/Stripe, an earlier payout still in flight does not
  -- stop a new one for money earned since.
  pb <- computePayoutableBalance counterparty personId.getId cutoff now
  let eligibility = pb.eligibility
  pure
    PayoutSnapshot
      { payoutableBalance = pb.payoutableBalance,
        redeemableEntryIds = map (.getId) eligibility.redeemableEntryIds,
        merchantTransferAmount = eligibility.merchantTransferAmount,
        cutoff = cutoff,
        mbBankAccount = mbBankAccount
      }

-- NOTE (reviewer, remove before merge): bank-detail checks are PRESENCE-ONLY. A blank account number, IFSC or name at bank
--   makes the person EXCLUDED with that reason (shown on GET /payout/excluded). There are no length checks: HDFC validates
--   the values, and a bad value comes back as a rejected item whose hold is given back. The instant payout in a bulk city
--   (Bulk.Driver.runInstantBulkPayout) also calls it, before the wallet lock: there the 'Just' message is returned to the
--   driver as a 400 instead of being recorded as an EXCLUDED request. Same strings either way.
--   Juspay/Stripe impact: bulk-only. Juspay pays to a VPA and Stripe to a connected account; this function is only used on
--   bulk paths (eligibility pass, Claim.hs, adhoc lookup screen, bulk instant payout).

-- | Everything HDFC needs on the wire, checked before a payout_order exists. 'Nothing' is usable;
--   the 'Just' is written verbatim to payout_request.failureReason, which is what
--   @GET \/payout\/excluded@ shows an operator -- so these strings are the operator-facing messages.
--
--   Blank counts as absent: the line item falls back to @""@ for a missing IFSC or name, so without
--   this the bank is sent an empty field and rejects the item. Only presence is checked: the bank
--   validates the values themselves. Verification status is deliberately not consulted: HDFC pays to
--   an account number and IFSC and never sees it, so what matters is whether the details are there.
usableBankDetails :: DDBA.DriverBankAccount -> Maybe Text
usableBankDetails acc
  | T.null (T.strip acc.accountId) = Just "Bank account number missing"
  | blank acc.ifscCode = Just "IFSC code missing"
  | blank acc.nameAtBank = Just "Account holder name missing"
  | otherwise = Nothing
  where
    blank = maybe True (T.null . T.strip)

-- NOTE (reviewer, remove before merge): below minimumPayoutAmount -> skipped and only logged; otherwise a batch member,
--   EXCLUDED when the bank details are missing. There is no "payout in flight" guard (see the NOTE on readPayoutSnapshot).
--   One function decides for the sweep, the adhoc initiate, the adhoc lookup screen and the re-check in Claim.hs, so they
--   cannot disagree. Juspay/Stripe impact: bulk-only.

-- | Decide what happens to one beneficiary. 'Left' is "not in this batch at all, and nothing to
--   record"; 'Right' is a batch member, payable when 'exclusionReason' is empty and otherwise an
--   EXCLUDED request carrying the reason.
classifyBulkCandidate :: DSPC.ScheduledPayoutConfig -> DP.Person -> PayoutSnapshot -> Either Text BulkCandidate
classifyBulkCandidate config person standing
  | standing.payoutableBalance < config.minimumPayoutAmount =
    Left ("payoutableBalance=" <> show standing.payoutableBalance <> " below minimum=" <> show config.minimumPayoutAmount)
  | otherwise = Right candidate {Bulk.exclusionReason = mbExclusion}
  where
    -- The excluded list is a worklist an admin acts on, so it holds exactly the
    -- beneficiaries a human can fix -- and says which field to fix.
    mbExclusion = case standing.mbBankAccount of
      Nothing -> Just "Bank account not added"
      Just bankAccount -> usableBankDetails bankAccount
    candidate =
      Bulk.BulkCandidate
        { beneficiary = person,
          amount = standing.payoutableBalance,
          exclusionReason = Nothing
        }

-- | Read-only eligibility pass for one beneficiary: every check, no writes and no
--   lock. Runs before any batch exists so the batch can be opened around its members.
checkBulkPayoutEligibility ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  DTConf.TransporterConfig ->
  -- | Loaded by the caller, a page at a time, rather than one lookup per beneficiary here.
  DP.Person ->
  -- | 'Left' is why this beneficiary is not in the batch -- the scheduled sweep only logs it, the
  --   adhoc flow reports it back to the admin who asked for the payout.
  m (Either Text BulkCandidate)
checkBulkPayoutEligibility config transporterConfig person = do
  let personId = person.id
  result <- try $ do
    standing <- readPayoutSnapshot transporterConfig person
    pure $ classifyBulkCandidate config person standing
  case result of
    Left (e :: SomeException) -> do
      logError $ "BulkPayoutEligibility error for " <> personId.getId <> ": " <> show e
      pure $ Left ("Could not read payout standing: " <> show e)
    Right (Left reason) -> do
      -- The common case (below the minimum) happens on every sweep and is not actionable by anyone
      -- -- log only, persist nothing. Anything a human could fix is an exclusion instead, and
      -- reaches the worklist.
      logDebug $ "BulkPayoutEligibility: skipping " <> personId.getId <> " -- " <> reason
      pure $ Left reason
    Right (Right candidate) -> pure (Right candidate)
