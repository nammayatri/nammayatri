{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantPaymentMethod (module Storage.Queries.MerchantPaymentMethod, module ReExport) where

import qualified Domain.Types.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
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

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod -> m (Maybe Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod -> m ())
updateByPrimaryKey (Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.collectedBy collectedBy,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paymentInstrument paymentInstrument,
      Se.Set Beam.paymentType paymentType,
      Se.Set Beam.priority priority,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
