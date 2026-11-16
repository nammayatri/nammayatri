{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WalletTransactionExtra where

import qualified Domain.Types.WalletTransaction as Domain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.WalletTransaction as Beam
import Storage.Queries.OrphanInstances.WalletTransaction

findAllByPaymentOrderIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder] ->
  m [Domain.WalletTransaction]
findAllByPaymentOrderIds [] = pure []
findAllByPaymentOrderIds orderIds =
  findAllWithKV
    [Se.Is Beam.paymentOrderId $ Se.In (map Kernel.Types.Id.getId orderIds)]

-- | Status of every row of a payment order. Rows written before 'mirrorWalletTransaction' matched by
--   order can hold two rows for one order, so this must not be the generated updateOneWithKV
--   ('updateStatus'), which fails on more than one match and would stop the wallet credit after it.
updateStatusByPaymentOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Domain.WalletTransactionStatus ->
  Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder ->
  m ()
updateStatusByPaymentOrderId status paymentOrderId = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status status, Se.Set Beam.updatedAt now]
    [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]
