{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallbackRequest where

import qualified Domain.Types.CallbackRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CallbackRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CallbackRequest.CallbackRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CallbackRequest.CallbackRequest] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CallbackRequest.CallbackRequest -> m (Maybe Domain.Types.CallbackRequest.CallbackRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CallbackRequest.CallbackRequest -> m ())
updateByPrimaryKey (Domain.Types.CallbackRequest.CallbackRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.customerMobileCountryCode customerMobileCountryCode,
      Se.Set Beam.customerName customerName,
      Se.Set Beam.customerPhoneEncrypted (customerPhone & unEncrypted . encrypted),
      Se.Set Beam.customerPhoneHash (customerPhone & hash),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CallbackRequest Domain.Types.CallbackRequest.CallbackRequest where
  fromTType' (Beam.CallbackRequestT {..}) = do
    pure $
      Just
        Domain.Types.CallbackRequest.CallbackRequest
          { createdAt = createdAt,
            customerMobileCountryCode = customerMobileCountryCode,
            customerName = customerName,
            customerPhone = EncryptedHashed (Encrypted customerPhoneEncrypted) customerPhoneHash,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CallbackRequest Domain.Types.CallbackRequest.CallbackRequest where
  toTType' (Domain.Types.CallbackRequest.CallbackRequest {..}) = do
    Beam.CallbackRequestT
      { Beam.createdAt = createdAt,
        Beam.customerMobileCountryCode = customerMobileCountryCode,
        Beam.customerName = customerName,
        Beam.customerPhoneEncrypted = customerPhone & unEncrypted . encrypted,
        Beam.customerPhoneHash = customerPhone & hash,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
