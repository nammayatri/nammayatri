{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfOrderStateExtra
  ( findAllByMerchantAndOrderIds,
    upsertReported,
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.Finance.Domain.Types.RsfOrderState as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.RsfOrderState as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.RsfOrderState ()
import qualified Sequelize as Se

findAllByMerchantAndOrderIds ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Domain.RsfOrderState]
findAllByMerchantAndOrderIds _ [] = pure []
findAllByMerchantAndOrderIds merchantId orderIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.orderId $ Se.In orderIds
        ]
    ]

-- Phase 4 (Mapping C): stamped once per send, only after the collector ACKs.
upsertReported ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  Text ->
  HighPrecMoney ->
  Maybe Text ->
  Text ->
  UTCTime ->
  m ()
upsertReported merchantId orderId reportedStatus reportedDiff reportedCode messageId reportedAt = do
  let key =
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq merchantId,
              Se.Is Beam.orderId $ Se.Eq orderId
            ]
        ]
  existing <- findOneWithKV key
  case (existing :: Maybe Domain.RsfOrderState) of
    Just _ ->
      updateWithKV
        [ Se.Set Beam.reportedStatus (Just reportedStatus),
          Se.Set Beam.reportedDiff (Just reportedDiff),
          Se.Set Beam.reportedCode reportedCode,
          Se.Set Beam.lastReportedMessageId (Just messageId),
          Se.Set Beam.lastReportedAt (Just reportedAt),
          Se.Set Beam.updatedAt reportedAt
        ]
        key
    Nothing ->
      createWithKV
        Domain.RsfOrderState
          { merchantId = merchantId,
            orderId = orderId,
            reportedStatus = Just reportedStatus,
            reportedDiff = Just reportedDiff,
            reportedCode = reportedCode,
            lastReportedMessageId = Just messageId,
            lastReportedAt = Just reportedAt,
            createdAt = reportedAt,
            updatedAt = reportedAt
          }
