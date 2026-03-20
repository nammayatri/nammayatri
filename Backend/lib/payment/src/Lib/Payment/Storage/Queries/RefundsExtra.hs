module Lib.Payment.Storage.Queries.RefundsExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.Refunds as BeamR
import Lib.Payment.Storage.Queries.OrphanInstances.Refunds ()
import qualified Sequelize as Se

-- Extra code goes here --

findAllByOrderIds ::
  BeamFlow m r =>
  [ShortId DPaymentOrder.PaymentOrder] ->
  m [DRefunds.Refunds]
findAllByOrderIds shortIds = findAllWithKV [Se.Is BeamR.orderId $ Se.In ids | not (null ids)]
  where
    ids = map getShortId shortIds

findLatestByOrderId ::
  BeamFlow m r =>
  ShortId DPaymentOrder.PaymentOrder ->
  m (Maybe DRefunds.Refunds)
findLatestByOrderId orderId =
  findAllWithOptionsKV
    [Se.Is BeamR.orderId $ Se.Eq (getShortId orderId)]
    (Se.Desc BeamR.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe
