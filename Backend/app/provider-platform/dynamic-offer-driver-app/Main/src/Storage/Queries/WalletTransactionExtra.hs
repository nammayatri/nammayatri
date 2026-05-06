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
