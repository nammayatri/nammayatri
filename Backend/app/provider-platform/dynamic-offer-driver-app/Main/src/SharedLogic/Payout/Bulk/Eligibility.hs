{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Who gets paid. Read-only: one reader for everything a payout decision rests on, and one pure
-- function that decides. Nothing here writes a row or takes a lock.
module SharedLogic.Payout.Bulk.Eligibility
  ( checkBulkPayoutEligibility,
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
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPRE
import SharedLogic.Finance.Wallet
import SharedLogic.Payout.Bulk.Types
import qualified Storage.Queries.DriverBankAccount as QDBA

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
  mbAccount <- getWalletAccountByOwner counterparty personId.getId
  walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty personId.getId
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
      cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  -- Main's wallet view over the shared calculation. Its redeemableBalance already excludes what is
  -- parked in OwnerPayoutLiability for payouts still in flight, because the hold debits the wallet
  -- at claim time -- so a second sweep cannot see the same money as payable. That is the structural
  -- half of the double-pay fix; the in-flight check below is the belt to its braces.
  eligibility <- case (.id) <$> mbAccount of
    Nothing -> pure emptyWalletPayoutEligibility
    Just accountId -> getPayoutEligibilityData counterparty personId.getId accountId walletBalance cutoff now
  -- Kept deliberately even though the hold now makes double payment impossible by construction:
  -- one payout at a time per beneficiary is the behaviour the sweep wants, and it keeps a
  -- beneficiary out of a second batch while the first is still unresolved with HDFC.
  activePayouts <- QPRE.findByBeneficiaryWithFilters personId.getId Nothing Nothing [PR.INITIATED, PR.PROCESSING] (Just 1) (Just 0)
  pure
    PayoutSnapshot
      { payoutableBalance = eligibility.redeemableBalance,
        redeemableEntryIds = map (.getId) eligibility.redeemableEntryIds,
        merchantTransferAmount = eligibility.merchantTransferAmount,
        cutoff = cutoff,
        mbBankAccount = mbBankAccount,
        hasPayoutInFlight = not (null activePayouts)
      }

-- | Everything HDFC needs on the wire, checked before a payout_order exists. 'Nothing' is usable;
--   the 'Just' is written verbatim to payout_request.failureReason, which is what
--   @GET \/payout\/excluded@ shows an operator -- so these strings are the operator-facing messages.
--
--   Blank counts as absent: the line item falls back to @""@ for a missing IFSC or name, so without
--   this the bank is sent an empty field and rejects the item. Verification status is deliberately
--   not consulted: HDFC pays to an account number and IFSC and never sees it, so what matters is
--   whether the details we hold are usable.
usableBankDetails :: DDBA.DriverBankAccount -> Maybe Text
usableBankDetails acc
  | T.null (T.strip acc.accountId) = Just "Bank account number missing"
  | T.length (T.strip acc.accountId) > 25 = Just "Bank account number is longer than 25 characters"
  | blank acc.ifscCode = Just "IFSC code missing"
  | not (validIfsc acc.ifscCode) = Just "IFSC code is invalid (expected 11 characters)"
  | blank acc.nameAtBank = Just "Account holder name missing"
  | otherwise = Nothing
  where
    blank = maybe True (T.null . T.strip)
    -- Length only. The bank validates the rest, and a stricter pattern here would refuse accounts
    -- HDFC would have accepted.
    validIfsc = maybe False ((== 11) . T.length . T.strip)

-- | Decide what happens to one beneficiary. 'Left' is "not in this batch at all, and nothing to
--   record"; 'Right' is a batch member, payable when 'exclusionReason' is empty and otherwise an
--   EXCLUDED request carrying the reason.
classifyBulkCandidate :: DSPC.ScheduledPayoutConfig -> DP.Person -> PayoutSnapshot -> Either Text BulkCandidate
classifyBulkCandidate config person standing
  | standing.hasPayoutInFlight = Left "an in-flight payout request already exists"
  | standing.payoutableBalance < config.minimumPayoutAmount =
    Left ("payoutableBalance=" <> show standing.payoutableBalance <> " below minimum=" <> show config.minimumPayoutAmount)
  | otherwise = Right candidate {exclusionReason = mbExclusion}
  where
    -- The excluded list is a worklist an admin acts on (doc p.39), so it holds exactly the
    -- beneficiaries a human can fix -- and now says which field to fix.
    mbExclusion = case standing.mbBankAccount of
      Nothing -> Just "Bank account not added"
      Just bankAccount -> usableBankDetails bankAccount
    candidate =
      BulkCandidate
        { person = person,
          amount = standing.payoutableBalance,
          exclusionReason = Nothing
        }

-- | Read-only eligibility pass for one beneficiary: every check the doc asks for, no writes and no
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
      -- The common cases (below the minimum, a payout already in flight) happen on every sweep and
      -- are not actionable by anyone -- log only, persist nothing. Anything a human could fix is an
      -- exclusion instead, and reaches the worklist.
      logDebug $ "BulkPayoutEligibility: skipping " <> personId.getId <> " -- " <> reason
      pure $ Left reason
    Right (Right candidate) -> pure (Right candidate)
