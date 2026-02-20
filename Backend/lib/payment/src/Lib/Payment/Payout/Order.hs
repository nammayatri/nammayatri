{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Order
  ( findLatestPayoutOrderByEntity,
    findLatestPayoutOrderByEntityId,
  )
where

import Control.Lens ((^?), _head)
import Kernel.Prelude
import Lib.Payment.Domain.Types.Common (EntityName)
import Lib.Payment.Domain.Types.PayoutOrder (PayoutOrder)
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutOrderExtra as QPOE

findLatestPayoutOrderByEntity ::
  (BeamFlow m r) =>
  EntityName ->
  [Text] ->
  m (Maybe PayoutOrder)
findLatestPayoutOrderByEntity entityName entityIds = do
  orders <- QPOE.findAllByEntityNameAndEntityIds (Just 1) (Just 0) (Just entityName) (map Just entityIds)
  pure $ orders ^? _head

findLatestPayoutOrderByEntityId ::
  (BeamFlow m r) =>
  EntityName ->
  Text ->
  m (Maybe PayoutOrder)
findLatestPayoutOrderByEntityId entityName entityId =
  findLatestPayoutOrderByEntity entityName [entityId]
