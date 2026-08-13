{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.ReconSettlementOrderExtra
  ( messageIdExists,
    findByMerchantAndReceivedAtRange,
    findByMerchantIdAndReceivedAtRange,
    findByMerchantIdSourceTypeAndReceivedAtRange,
    findByIds,
    findByOrderIds,
    findByOrderIdsAndMerchant,
    findBySettlementId,
    findBySettlementIdAndMerchant,
    findByUtrSettlementIds,
    findByUtrSettlementIdsAndMerchant,
    findByRideIds,
    updateReconciliationStatus,
    updateRsfReconResult,
    updateReconVerdict,
    updateManualConfirmation,
    listBatchSummariesForDashboard,
    markSentWithVerdict,
    markSentPreservingVerdict,
  )
where

import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconSettlementOrder as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.ReconSettlementOrder ()
import qualified Sequelize as Se

messageIdExists ::
  (BeamFlow m r) =>
  Text ->
  m Bool
messageIdExists msgId = do
  rows <- findAllWithOptionsKV [Se.Is Beam.messageId $ Se.Eq msgId] (Se.Asc Beam.createdAt) (Just 1) Nothing
  pure $ not (null (rows :: [Domain.ReconSettlementOrder]))

findByMerchantAndReceivedAtRange ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  m [Domain.ReconSettlementOrder]
findByMerchantAndReceivedAtRange merchantId merchantOpCityId from to =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId),
          Se.Is Beam.receivedAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.receivedAt $ Se.LessThan to
        ]
    ]

findByIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByIds [] = pure []
findByIds ids =
  findAllWithKV [Se.Is Beam.id $ Se.In ids]

findByRideIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByRideIds [] = pure []
findByRideIds rideIds =
  findAllWithKV [Se.Is Beam.rideId $ Se.In (map Just rideIds)]

updateReconciliationStatus ::
  (BeamFlow m r) =>
  Id Domain.ReconSettlementOrder ->
  Text ->
  m ()
updateReconciliationStatus rsoId status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.reconciliationStatus (Just status),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId rsoId)]

findByMerchantIdAndReceivedAtRange ::
  (BeamFlow m r) =>
  Text ->
  UTCTime ->
  UTCTime ->
  m [Domain.ReconSettlementOrder]
findByMerchantIdAndReceivedAtRange merchantId from to =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.receivedAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.receivedAt $ Se.LessThan to
        ]
    ]

findByMerchantIdSourceTypeAndReceivedAtRange ::
  (BeamFlow m r) =>
  Text ->
  [Domain.ReconSourceType] ->
  UTCTime ->
  UTCTime ->
  m [Domain.ReconSettlementOrder]
findByMerchantIdSourceTypeAndReceivedAtRange merchantId sourceTypes from to =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.sourceType $ Se.In (map Just sourceTypes),
          Se.Is Beam.receivedAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.receivedAt $ Se.LessThan to
        ]
    ]

findByOrderIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByOrderIds [] = pure []
findByOrderIds orderIds =
  findAllWithKV [Se.Is Beam.orderId $ Se.In orderIds]

findByOrderIdsAndMerchant ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByOrderIdsAndMerchant _ [] = pure []
findByOrderIdsAndMerchant merchantId orderIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.sourceType $ Se.Eq (Just Domain.BAP_CLAIMED),
          Se.Is Beam.orderId $ Se.In orderIds
        ]
    ]

findBySettlementId ::
  (BeamFlow m r) =>
  Text ->
  m [Domain.ReconSettlementOrder]
findBySettlementId settlementId =
  findAllWithKV [Se.Is Beam.settlementId $ Se.Eq settlementId]

findBySettlementIdAndMerchant ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m [Domain.ReconSettlementOrder]
findBySettlementIdAndMerchant merchantId settlementId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.sourceType $ Se.Eq (Just Domain.BAP_CLAIMED),
          Se.Is Beam.settlementId $ Se.Eq settlementId
        ]
    ]

findByUtrSettlementIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByUtrSettlementIds [] = pure []
findByUtrSettlementIds utrIds =
  findAllWithKV [Se.Is Beam.utrSettlementId $ Se.In (map Just utrIds)]

findByUtrSettlementIdsAndMerchant ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByUtrSettlementIdsAndMerchant _ [] = pure []
findByUtrSettlementIdsAndMerchant merchantId utrIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.sourceType $ Se.Eq (Just Domain.BAP_CLAIMED),
          Se.Is Beam.utrSettlementId $ Se.In (map Just utrIds)
        ]
    ]

-- | Order-level write: verdict/diff are the *same* value for every row
-- sharing an order (fare vs. the order's total claimed amount, nothing
-- individually attributed per UTR -- nobody downstream needs per-UTR
-- status, only the order-level one, confirmed against the actual MSIL
-- admin spec). rideId/driverId/platformGross/platformNet are genuine
-- ride-level facts with no per-UTR ambiguity either. Matches every row for
-- the order in one statement, excluding locked ones.
updateRsfReconResult ::
  (BeamFlow m r) =>
  Text ->
  Domain.OrderReconVerdict ->
  Maybe HighPrecMoney ->
  Maybe Text ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe UTCTime ->
  m ()
updateRsfReconResult orderId verdict diffAmt rideId driverId platformGross platformNet platformOrderTimestamp = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.ourReconStatus verdict,
      Se.Set Beam.diffAmount diffAmt,
      Se.Set Beam.rideId rideId,
      Se.Set Beam.driverId driverId,
      Se.Set Beam.platformGrossFare platformGross,
      Se.Set Beam.platformNetReceivable platformNet,
      Se.Set Beam.platformOrderTimestamp platformOrderTimestamp,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.orderId $ Se.Eq orderId,
          Se.Or
            [ Se.Is Beam.reconciliationStatus $ Se.Eq Nothing,
              Se.Is Beam.reconciliationStatus $ Se.Not $ Se.Eq (Just "SENT")
            ],
          Se.Is Beam.manuallyConfirmedAt $ Se.Eq Nothing
        ]
    ]

updateReconVerdict ::
  (BeamFlow m r) =>
  Id Domain.ReconSettlementOrder ->
  Domain.OrderReconVerdict ->
  HighPrecMoney ->
  Maybe HighPrecMoney ->
  m ()
updateReconVerdict rsoId verdict claimedSettlementAmount diffAmt = do
  now <- getCurrentTime
  let allocatedBankCash = claimedSettlementAmount - min (fromMaybe 0 diffAmt) claimedSettlementAmount
  updateWithKV
    [ Se.Set Beam.ourReconStatus verdict,
      Se.Set Beam.diffAmount diffAmt,
      Se.Set Beam.allocatedBankCash (Just allocatedBankCash),
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (getId rsoId),
          -- Defense in depth: syncUtrStatus only ever passes open-row ids in,
          -- but the query layer enforces the same locked-row invariant too.
          Se.Or
            [ Se.Is Beam.reconciliationStatus $ Se.Eq Nothing,
              Se.Is Beam.reconciliationStatus $ Se.Not $ Se.Eq (Just "SENT")
            ],
          Se.Is Beam.manuallyConfirmedAt $ Se.Eq Nothing
        ]
    ]

updateManualConfirmation ::
  (BeamFlow m r) =>
  Id Domain.ReconSettlementOrder ->
  UTCTime ->
  Text ->
  Text ->
  HighPrecMoney ->
  Domain.OrderReconVerdict ->
  Maybe HighPrecMoney ->
  m ()
updateManualConfirmation rsoId confirmedAt confirmedBy reason confirmedAmount verdict diffAmt = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.manuallyConfirmedAt (Just confirmedAt),
      Se.Set Beam.manuallyConfirmedBy (Just confirmedBy),
      Se.Set Beam.manualConfirmationReason (Just reason),
      Se.Set Beam.allocatedBankCash (Just confirmedAmount),
      Se.Set Beam.ourReconStatus verdict,
      Se.Set Beam.diffAmount diffAmt,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId rsoId)]

listBatchSummariesForDashboard ::
  (BeamFlow m r) =>
  Text ->
  Maybe Text ->
  UTCTime ->
  UTCTime ->
  Int ->
  Int ->
  m (Int, [(Text, Int, Int, Int, UTCTime)])
listBatchSummariesForDashboard merchantId _mbBapId from to limit offset = do
  let clauses =
        [ Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.sourceType $ Se.Eq (Just Domain.BAP_CLAIMED),
          Se.Is Beam.receivedAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.receivedAt $ Se.LessThan to,
          Se.Is Beam.settlementId $ Se.Not $ Se.Eq ""
        ]
  rows <- findAllWithKV [Se.And clauses]
  let rowsMap = Map.fromListWith (<>) [(r.settlementId, [r]) | r <- rows]
      batches =
        map
          ( \(sid, rs) ->
              ( sid,
                length (nub (mapMaybe (.utrSettlementId) rs)),
                length (nub (map (.orderId) rs)),
                length (nub (map (.orderId) (filter (\r -> r.reconciliationStatus /= Just "SENT") rs))),
                minimum (map (.receivedAt) rs)
              )
          )
          (Map.toList rowsMap)
      -- Drop fully-sent batches -- they don't need ops attention any more.
      unsentBatches = filter (\(_, _, _, unsentOrderCount, _) -> unsentOrderCount > 0) batches
      sorted = sortOn (\(_, _, _, _, earliest) -> Down earliest) unsentBatches
      total = length sorted
      paginated = take limit (drop offset sorted)
  pure (total, paginated)

markSentWithVerdict ::
  (BeamFlow m r) =>
  Id Domain.ReconSettlementOrder ->
  Domain.OrderReconVerdict ->
  Maybe HighPrecMoney ->
  m ()
markSentWithVerdict rsoId verdict diffAmt = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.reconciliationStatus (Just "SENT"),
      Se.Set Beam.ourReconStatus verdict,
      Se.Set Beam.diffAmount diffAmt,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId rsoId)]

markSentPreservingVerdict ::
  (BeamFlow m r) =>
  Id Domain.ReconSettlementOrder ->
  m ()
markSentPreservingVerdict rsoId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.reconciliationStatus (Just "SENT"),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId rsoId)]
