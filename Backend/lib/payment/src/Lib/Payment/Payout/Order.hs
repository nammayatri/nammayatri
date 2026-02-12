{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Order
  ( findLatestPayoutOrderByEntity,
    findLatestPayoutOrderByEntityId,
  )
where

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
  pure $ listToMaybe orders

findLatestPayoutOrderByEntityId ::
  (BeamFlow m r) =>
  EntityName ->
  Text ->
  m (Maybe PayoutOrder)
findLatestPayoutOrderByEntityId entityName entityId =
  findLatestPayoutOrderByEntity entityName [entityId]
