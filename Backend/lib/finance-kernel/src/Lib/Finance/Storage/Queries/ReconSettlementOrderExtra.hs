{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ReconSettlementOrderExtra
  ( messageIdExists,
    findByMerchantAndReceivedAtRange,
    findByMerchantIdAndReceivedAtRange,
    findByIds,
    findByOrderIds,
    findByRideIds,
    updateReconciliationStatus,
    updateRsfReconResult,
  )
where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
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
  mbRow <- findOneWithKV [Se.Is Beam.messageId $ Se.Eq msgId]
  pure $ isJust (mbRow :: Maybe Domain.ReconSettlementOrder)

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
    [Se.Is Beam.orderId $ Se.Eq orderId]
