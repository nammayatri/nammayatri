{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.ReconSettlementOrderExtra
  ( messageIdExists,
    findByMerchantAndReceivedAtRange,
    findByMerchantIdAndReceivedAtRange,
    findByIds,
    findByOrderIds,
    findBySettlementId,
    findByUtrSettlementIds,
    findByRideIds,
    updateReconciliationStatus,
    updateRsfReconResult,
    updateReconVerdict,
    updateManualConfirmation,
  )
where

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

findByOrderIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByOrderIds [] = pure []
findByOrderIds orderIds =
  findAllWithKV [Se.Is Beam.orderId $ Se.In orderIds]

findBySettlementId ::
  (BeamFlow m r) =>
  Text ->
  m [Domain.ReconSettlementOrder]
findBySettlementId settlementId =
  findAllWithKV [Se.Is Beam.settlementId $ Se.Eq settlementId]

findByUtrSettlementIds ::
  (BeamFlow m r) =>
  [Text] ->
  m [Domain.ReconSettlementOrder]
findByUtrSettlementIds [] = pure []
findByUtrSettlementIds utrIds =
  findAllWithKV [Se.Is Beam.utrSettlementId $ Se.In (map Just utrIds)]

updateRsfReconResult ::
  (BeamFlow m r) =>
  Text ->
  Domain.OrderReconVerdict ->
  Maybe HighPrecMoney ->
  Maybe Text ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  m ()
updateRsfReconResult orderId verdict diffAmt rideId driverId platformGross platformNet = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.ourReconStatus verdict,
      Se.Set Beam.diffAmount diffAmt,
      Se.Set Beam.rideId rideId,
      Se.Set Beam.driverId driverId,
      Se.Set Beam.platformGrossFare platformGross,
      Se.Set Beam.platformNetReceivable platformNet,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.orderId $ Se.Eq orderId,
          -- Locked rows (already SENT or manually confirmed) are excluded --
          -- the NULL-safe form matters here: reconciliationStatus defaults to
          -- Nothing for every open row, and a plain `Se.Not (Se.Eq (Just
          -- "SENT"))` would silently drop those NULL rows too via SQL's
          -- three-valued logic (NULL <> 'SENT' = NULL, not TRUE).
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
  Maybe HighPrecMoney ->
  m ()
updateReconVerdict rsoId verdict diffAmt = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.ourReconStatus verdict,
      Se.Set Beam.diffAmount diffAmt,
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
  m ()
updateManualConfirmation rsoId confirmedAt confirmedBy reason = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.manuallyConfirmedAt (Just confirmedAt),
      Se.Set Beam.manuallyConfirmedBy (Just confirmedBy),
      Se.Set Beam.manualConfirmationReason (Just reason),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId rsoId)]
