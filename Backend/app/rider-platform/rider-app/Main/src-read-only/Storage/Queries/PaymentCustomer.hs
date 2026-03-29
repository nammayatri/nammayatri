{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PaymentCustomer where

import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.PaymentCustomer
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PaymentCustomer as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentCustomer.PaymentCustomer -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PaymentCustomer.PaymentCustomer] -> m ())
createMany = traverse_ create

findByCustomerIdAndPaymentMode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.External.Payment.Interface.Types.CustomerId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByCustomerIdAndPaymentMode customerId paymentMode = do findOneWithKV [Se.And [Se.Is Beam.customerId $ Se.Eq customerId, Se.Is Beam.paymentMode $ Se.Eq paymentMode]]

findByPersonIdAndPaymentMode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByPersonIdAndPaymentMode personId paymentMode = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId <$> personId), Se.Is Beam.paymentMode $ Se.Eq paymentMode]]

updateCATAndExipry ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.External.Payment.Interface.Types.CustomerId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m ())
updateCATAndExipry clientAuthToken clientAuthTokenExpiry customerId paymentMode = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.clientAuthToken clientAuthToken, Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.customerId $ Se.Eq customerId,
          Se.Is Beam.paymentMode $ Se.Eq paymentMode
        ]
    ]

updateDefaultPaymentMethodId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m ())
updateDefaultPaymentMethodId defaultPaymentMethodId personId paymentMode = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.defaultPaymentMethodId defaultPaymentMethodId, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId <$> personId),
          Se.Is Beam.paymentMode $ Se.Eq paymentMode
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.External.Payment.Interface.Types.CustomerId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByPrimaryKey customerId paymentMode = do findOneWithKV [Se.And [Se.Is Beam.customerId $ Se.Eq customerId, Se.Is Beam.paymentMode $ Se.Eq paymentMode]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentCustomer.PaymentCustomer -> m ())
updateByPrimaryKey (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientAuthToken clientAuthToken,
      Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry,
      Se.Set Beam.defaultPaymentMethodId defaultPaymentMethodId,
      Se.Set Beam.personId (Kernel.Types.Id.getId <$> personId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.customerId $ Se.Eq customerId, Se.Is Beam.paymentMode $ Se.Eq paymentMode]]

instance FromTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer where
  fromTType' (Beam.PaymentCustomerT {..}) = do
    pure $
      Just
        Domain.Types.PaymentCustomer.PaymentCustomer
          { clientAuthToken = clientAuthToken,
            clientAuthTokenExpiry = clientAuthTokenExpiry,
            customerId = customerId,
            defaultPaymentMethodId = defaultPaymentMethodId,
            paymentMode = paymentMode,
            personId = Kernel.Types.Id.Id <$> personId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer where
  toTType' (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do
    Beam.PaymentCustomerT
      { Beam.clientAuthToken = clientAuthToken,
        Beam.clientAuthTokenExpiry = clientAuthTokenExpiry,
        Beam.customerId = customerId,
        Beam.defaultPaymentMethodId = defaultPaymentMethodId,
        Beam.paymentMode = paymentMode,
        Beam.personId = Kernel.Types.Id.getId <$> personId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
