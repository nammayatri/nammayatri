{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Who gets a bulk payout: one snapshot reader and one pure decision. No writes, no locks.
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

-- | Everything one payout decision rests on, read in one place so eligibility and claim cannot drift apart.
data PayoutSnapshot = PayoutSnapshot
  { payoutableBalance :: HighPrecMoney,
    redeemableEntryIds :: [Text],
    merchantTransferAmount :: HighPrecMoney,
    cutoff :: UTCTime,
    mbBankAccount :: Maybe DDBA.DriverBankAccount
  }

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
  -- Same amount instant payout uses. Held money has already left the wallet, so a second sweep cannot
  -- pay it again; a payout still in flight does not block money earned since.
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

-- | 'Nothing' when HDFC has every field it needs; otherwise the reason operators see on the excluded
--   list. Blank counts as missing. Verification status is ignored, since HDFC never sees it.
usableBankDetails :: DDBA.DriverBankAccount -> Maybe Text
usableBankDetails acc
  | T.null (T.strip acc.accountId) = Just "Bank account number missing"
  | blank acc.ifscCode = Just "IFSC code missing"
  | blank acc.nameAtBank = Just "Account holder name missing"
  | otherwise = Nothing
  where
    blank = maybe True (T.null . T.strip)

-- | 'Left': not in this batch, nothing recorded. 'Right': a batch member, payable unless
--   'exclusionReason' is set, in which case it becomes an EXCLUDED request.
classifyBulkCandidate :: DSPC.ScheduledPayoutConfig -> DP.Person -> PayoutSnapshot -> Either Text BulkCandidate
classifyBulkCandidate config person standing
  | standing.payoutableBalance < config.minimumPayoutAmount =
    Left ("payoutableBalance=" <> show standing.payoutableBalance <> " below minimum=" <> show config.minimumPayoutAmount)
  | otherwise = Right candidate {Bulk.exclusionReason = mbExclusion}
  where
    -- The excluded list is an admin worklist: only what a human can fix, naming the field.
    mbExclusion = case standing.mbBankAccount of
      Nothing -> Just "Bank account not added"
      Just bankAccount -> usableBankDetails bankAccount
    candidate =
      Bulk.BulkCandidate
        { beneficiary = person,
          amount = standing.payoutableBalance,
          exclusionReason = Nothing
        }

-- | Read-only eligibility check for one beneficiary. Runs before any batch exists, so the batch
--   can be opened around its members.
checkBulkPayoutEligibility ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  DTConf.TransporterConfig ->
  -- | Loaded by the caller a page at a time, not looked up here.
  DP.Person ->
  -- | 'Left' is why they are left out: the sweep logs it, adhoc reports it to the admin.
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
      -- Mostly below-minimum, seen every sweep and not actionable, so log only.
      logDebug $ "BulkPayoutEligibility: skipping " <> personId.getId <> " -- " <> reason
      pure $ Left reason
    Right (Right candidate) -> pure (Right candidate)
