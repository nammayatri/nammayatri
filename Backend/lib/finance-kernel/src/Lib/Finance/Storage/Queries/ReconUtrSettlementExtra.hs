{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.ReconUtrSettlementExtra
  ( upsertByUtr,
    updateBankVerifiedAmount,
    findByIdAndMerchant,
    findByIds,
    findByIdsAndMerchant,
    listUtrSummariesForDashboard,
  )
where

import qualified Data.List
import qualified Data.Ord
import qualified Data.Text as T
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, getCurrentTime, logError, withTryCatch)
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconUtrSettlement as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.ReconUtrSettlement ()
import qualified Sequelize as Se

upsertByUtr ::
  (BeamFlow m r) =>
  Domain.ReconUtrSettlement ->
  m (Id Domain.ReconUtrSettlement)
upsertByUtr settlement = do
  existing <- findOneWithKV [Se.Is Beam.utr $ Se.Eq settlement.utr]
  case (existing :: Maybe Domain.ReconUtrSettlement) of
    Just prev -> updateAccumulated prev >> pure prev.id
    Nothing -> do
      -- createWithKV, not the generated `create`, is used here deliberately:
      -- the generated ReconUtrSettlement module (src-read-only) already
      -- imports this Extra module to re-export its hand-written functions,
      -- so importing it back here to reach `create` (itself just an alias
      -- for createWithKV) would form a module import cycle.
      res <- withTryCatch "upsertByUtr:create" (createWithKV settlement $> settlement.id)
      case res of
        Right newId -> pure newId
        Left err
          -- The KV connector flattens every DB failure into an untyped
          -- exception (no typed unique-violation constructor to match on),
          -- so a real unique-constraint race and a genuinely unrelated
          -- insert failure are indistinguishable except by inspecting the
          -- error text. Only the recon_utr_settlement_utr_uniq violation
          -- (sqlState 23505, this exact constraint name) is a benign race
          -- safe to recover from by reading back and merging; anything
          -- else must not be silently swallowed as "just a race".
          | isUniqueUtrViolation err -> do
            prev <- findOneWithKV [Se.Is Beam.utr $ Se.Eq settlement.utr] >>= fromMaybeM (InternalError "RSF UTR upsert race: insert failed on a unique-violation but no row found on retry")
            updateAccumulated prev
            pure prev.id
          | otherwise -> do
            logError $ "RSF UTR upsert: create failed for a reason other than the utr unique constraint, not treating as a race: " <> T.pack (show err)
            throwM err
  where
    isUniqueUtrViolation err =
      let msg = T.pack (show err)
       in "23505" `T.isInfixOf` msg || "recon_utr_settlement_utr_uniq" `T.isInfixOf` msg
    updateAccumulated prev = do
      now <- getCurrentTime
      updateWithKV
        [ Se.Set Beam.claimedTotalAmount (prev.claimedTotalAmount + settlement.claimedTotalAmount),
          Se.Set Beam.totalOrders (prev.totalOrders + settlement.totalOrders),
          Se.Set Beam.updatedAt now
        ]
        [Se.Is Beam.utr $ Se.Eq settlement.utr]

updateBankVerifiedAmount ::
  (BeamFlow m r) =>
  Id Domain.ReconUtrSettlement ->
  HighPrecMoney ->
  m ()
updateBankVerifiedAmount utrId bankVerifiedAmount = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bankVerifiedAmount (Just bankVerifiedAmount),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId utrId)]

findByIdAndMerchant ::
  (BeamFlow m r) =>
  Text ->
  Id Domain.ReconUtrSettlement ->
  m (Maybe Domain.ReconUtrSettlement)
findByIdAndMerchant merchantId utrId =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.id $ Se.Eq (getId utrId)
        ]
    ]

findByIds ::
  (BeamFlow m r) =>
  [Id Domain.ReconUtrSettlement] ->
  m [Domain.ReconUtrSettlement]
findByIds [] = pure []
findByIds ids =
  findAllWithKV [Se.Is Beam.id $ Se.In (map getId ids)]

findByIdsAndMerchant ::
  (BeamFlow m r) =>
  Text ->
  [Id Domain.ReconUtrSettlement] ->
  m [Domain.ReconUtrSettlement]
findByIdsAndMerchant _ [] = pure []
findByIdsAndMerchant merchantId ids =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.id $ Se.In (map getId ids)
        ]
    ]

listUtrSummariesForDashboard ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Maybe Text -> -- optional bapId
  Maybe Bool -> -- optional isVerified
  UTCTime ->
  UTCTime -> -- createdAt range
  Int ->
  Int ->
  m (Int, [Domain.ReconUtrSettlement]) -- (totalItems, paginated rows)
listUtrSummariesForDashboard merchantId mbBapId mbVerified from to limit offset = do
  let clauses =
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.createdAt $ Se.LessThan to
        ]
          <> [Se.Is Beam.bapId $ Se.Eq b | Just b <- [mbBapId]]
  -- KV layer has no countKV; pull the filtered set into Haskell and slice.
  -- Acceptable at RSF's operational volume (finance-ops, not a hot path).
  allRows <- findAllWithKV [Se.And clauses]
  let verifiedFiltered = case mbVerified of
        Nothing -> allRows
        Just True -> filter (isJust . (.bankVerifiedAmount)) allRows
        Just False -> filter (isNothing . (.bankVerifiedAmount)) allRows
      sorted = Data.List.sortOn (\r -> Data.Ord.Down r.createdAt) verifiedFiltered
      total = length sorted
      paginated = take limit (drop offset sorted)
  pure (total, paginated)
