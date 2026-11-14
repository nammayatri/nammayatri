{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantPaymentMethod (module Storage.Queries.MerchantPaymentMethod, module ReExport) where

import qualified BecknV2.OnDemand.FareBreakup.Tax
import qualified Domain.Types.MerchantPaymentMethod
import qualified Domain.Types.PaymentChargeBearer
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantPaymentMethod as Beam
import Storage.Queries.MerchantPaymentMethodExtra as ReExport
import Storage.Queries.Transformers.MerchantPaymentMethod

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod] -> m ())
createMany = traverse_ create

updatePaymentCharge ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.PaymentChargeBearer.PaymentChargeType -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe BecknV2.OnDemand.FareBreakup.Tax.TaxConfig -> Kernel.Prelude.Maybe Domain.Types.PaymentChargeBearer.PaymentChargeBearer -> Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod -> m ())
updatePaymentCharge paymentChargeType paymentCharge paymentChargeTax paymentChargeBearer id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.paymentChargeType paymentChargeType,
      Se.Set Beam.paymentCharge paymentCharge,
      Se.Set Beam.paymentChargeTax (toJSON <$> paymentChargeTax),
      Se.Set Beam.paymentChargeBearer paymentChargeBearer,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
