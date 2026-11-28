{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutLifecycleEventExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutLifecycleEvent as DPLE
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Beam.PayoutLifecycleEvent as Beam
import Lib.Payment.Storage.Queries.OrphanInstances.PayoutLifecycleEvent
import qualified Sequelize as Se

findByMocIdAndEventTypeAndCreatedAtRange ::
  (BeamFlow m r) =>
  Text ->
  DPLE.PayoutLifecycleEventType ->
  UTCTime ->
  UTCTime ->
  m [DPLE.PayoutLifecycleEvent]
findByMocIdAndEventTypeAndCreatedAtRange mocId eventType fromTime toTime =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq mocId,
          Se.Is Beam.eventType $ Se.Eq eventType,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromTime,
          Se.Is Beam.createdAt $ Se.LessThan toTime
        ]
    ]
