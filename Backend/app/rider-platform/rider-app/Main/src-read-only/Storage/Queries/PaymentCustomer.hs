{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.PaymentCustomer where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.PaymentCustomer
import qualified Storage.Beam.PaymentCustomer as Beam
import qualified Kernel.Prelude
import qualified Kernel.External.Payment.Interface.Types
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentCustomer.PaymentCustomer -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PaymentCustomer.PaymentCustomer] -> m ())
createMany = traverse_ create
findByCustomerIdAndPaymentMode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                  (Kernel.External.Payment.Interface.Types.CustomerId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByCustomerIdAndPaymentMode customerId paymentMode = do findOneWithKV [Se.And [Se.Is Beam.customerId $ Se.Eq customerId, Se.Is Beam.paymentMode $ Se.Eq paymentMode]]
updateCATAndExipry :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                      (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.External.Payment.Interface.Types.CustomerId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m ())
updateCATAndExipry clientAuthToken clientAuthTokenExpiry customerId paymentMode = do {_now <- getCurrentTime;
                                                                                      updateWithKV [Se.Set Beam.clientAuthToken clientAuthToken, Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.customerId $ Se.Eq customerId,
                                                                                                                                                                                                                                               Se.Is Beam.paymentMode $ Se.Eq paymentMode]]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.External.Payment.Interface.Types.CustomerId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByPrimaryKey customerId paymentMode = do findOneWithKV [Se.And [Se.Is Beam.customerId $ Se.Eq customerId, Se.Is Beam.paymentMode $ Se.Eq paymentMode]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentCustomer.PaymentCustomer -> m ())
updateByPrimaryKey (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do {_now <- getCurrentTime;
                                                                             updateWithKV [Se.Set Beam.clientAuthToken clientAuthToken, Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.customerId $ Se.Eq customerId,
                                                                                                                                                                                                                                      Se.Is Beam.paymentMode $ Se.Eq paymentMode]]}



instance FromTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer
    where fromTType' (Beam.PaymentCustomerT {..}) = do pure $ Just Domain.Types.PaymentCustomer.PaymentCustomer{clientAuthToken = clientAuthToken,
                                                                                                                clientAuthTokenExpiry = clientAuthTokenExpiry,
                                                                                                                customerId = customerId,
                                                                                                                paymentMode = paymentMode,
                                                                                                                createdAt = createdAt,
                                                                                                                updatedAt = updatedAt}
instance ToTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer
    where toTType' (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do Beam.PaymentCustomerT{Beam.clientAuthToken = clientAuthToken,
                                                                                                  Beam.clientAuthTokenExpiry = clientAuthTokenExpiry,
                                                                                                  Beam.customerId = customerId,
                                                                                                  Beam.paymentMode = paymentMode,
                                                                                                  Beam.createdAt = createdAt,
                                                                                                  Beam.updatedAt = updatedAt}



