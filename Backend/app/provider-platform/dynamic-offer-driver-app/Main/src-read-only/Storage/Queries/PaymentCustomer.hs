{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PaymentCustomer where

import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.PaymentCustomer
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
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

findByDriverIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByDriverIdAndServiceName driverId serviceName = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.serviceName $ Se.Eq serviceName]]

updateClientAuthToken ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Text -> Domain.Types.MerchantServiceConfig.ServiceName -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m ())
updateClientAuthToken clientAuthToken clientAuthTokenExpiry customerId paymentServiceName driverId serviceName = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientAuthToken clientAuthToken,
      Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry,
      Se.Set Beam.customerId customerId,
      Se.Set Beam.paymentServiceName paymentServiceName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.serviceName $ Se.Eq serviceName]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByPrimaryKey driverId serviceName = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.serviceName $ Se.Eq serviceName]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentCustomer.PaymentCustomer -> m ())
updateByPrimaryKey (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientAuthToken clientAuthToken,
      Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry,
      Se.Set Beam.customerId customerId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paymentServiceName paymentServiceName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.serviceName $ Se.Eq serviceName]]

instance FromTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer where
  fromTType' (Beam.PaymentCustomerT {..}) = do
    pure $
      Just
        Domain.Types.PaymentCustomer.PaymentCustomer
          { clientAuthToken = clientAuthToken,
            clientAuthTokenExpiry = clientAuthTokenExpiry,
            createdAt = createdAt,
            customerId = customerId,
            driverId = Kernel.Types.Id.Id driverId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paymentServiceName = paymentServiceName,
            serviceName = serviceName,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer where
  toTType' (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do
    Beam.PaymentCustomerT
      { Beam.clientAuthToken = clientAuthToken,
        Beam.clientAuthTokenExpiry = clientAuthTokenExpiry,
        Beam.createdAt = createdAt,
        Beam.customerId = customerId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentServiceName = paymentServiceName,
        Beam.serviceName = serviceName,
        Beam.updatedAt = updatedAt
      }
