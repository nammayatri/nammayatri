module Lib.Payment.Storage.Queries.RefundsExtra where

import Kernel.Beam.Functions
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.Refunds as BeamR
import Lib.Payment.Storage.Queries.OrphanInstances.Refunds ()
import qualified Sequelize as Se

-- Extra code goes here --

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

updateStatus ::
  BeamFlow m r =>
  Payment.RefundStatus ->
  Id DRefunds.Refunds ->
  m ()
updateStatus newStatus rid = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set BeamR.status newStatus, Se.Set BeamR.updatedAt _now]
    [Se.Is BeamR.id $ Se.Eq (getId rid)]
