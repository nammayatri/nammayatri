{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PaymentCustomer where

import qualified Domain.Types.PaymentCustomer
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PaymentCustomer as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentCustomer.PaymentCustomer -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PaymentCustomer.PaymentCustomer] -> m ())
createMany = traverse_ create

findByCustomerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.External.Payment.Interface.Types.CustomerId -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByCustomerId customerId = do findOneWithKV [Se.Is Beam.customerId $ Se.Eq customerId]

updateCATAndExipry ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.External.Payment.Interface.Types.CustomerId -> m ())
updateCATAndExipry clientAuthToken clientAuthTokenExpiry customerId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.clientAuthToken clientAuthToken, Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry, Se.Set Beam.updatedAt _now] [Se.Is Beam.customerId $ Se.Eq customerId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.External.Payment.Interface.Types.CustomerId -> m (Maybe Domain.Types.PaymentCustomer.PaymentCustomer))
findByPrimaryKey customerId = do findOneWithKV [Se.And [Se.Is Beam.customerId $ Se.Eq customerId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentCustomer.PaymentCustomer -> m ())
updateByPrimaryKey (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.clientAuthToken clientAuthToken, Se.Set Beam.clientAuthTokenExpiry clientAuthTokenExpiry, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.customerId $ Se.Eq customerId]]

instance FromTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer where
  fromTType' (Beam.PaymentCustomerT {..}) = do
    pure $
      Just
        Domain.Types.PaymentCustomer.PaymentCustomer
          { clientAuthToken = clientAuthToken,
            clientAuthTokenExpiry = clientAuthTokenExpiry,
            customerId = customerId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PaymentCustomer Domain.Types.PaymentCustomer.PaymentCustomer where
  toTType' (Domain.Types.PaymentCustomer.PaymentCustomer {..}) = do
    Beam.PaymentCustomerT
      { Beam.clientAuthToken = clientAuthToken,
        Beam.clientAuthTokenExpiry = clientAuthTokenExpiry,
        Beam.customerId = customerId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
