{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.ReconUtrSettlementExtra
  ( upsertByUtr,
    updateBankVerifiedAmount,
    findByIds,
    listUtrSummariesForDashboard,
  )
where

import qualified Data.List
import qualified Data.Ord
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconUtrSettlement as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.ReconUtrSettlement ()
import qualified Sequelize as Se

upsertByUtr ::
  (BeamFlow m r) =>
  Domain.ReconUtrSettlement ->
  m ()
upsertByUtr settlement = do
  existing <- findOneWithKV [Se.Is Beam.utr $ Se.Eq settlement.utr]
  case (existing :: Maybe Domain.ReconUtrSettlement) of
    Nothing -> createWithKV settlement
    Just prev -> do
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

findByIds ::
  (BeamFlow m r) =>
  [Id Domain.ReconUtrSettlement] ->
  m [Domain.ReconUtrSettlement]
findByIds [] = pure []
findByIds ids =
  findAllWithKV [Se.Is Beam.id $ Se.In (map getId ids)]

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
