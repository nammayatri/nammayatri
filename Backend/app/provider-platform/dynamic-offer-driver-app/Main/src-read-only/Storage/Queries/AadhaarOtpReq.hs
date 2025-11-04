{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AadhaarOtpReq where

import qualified Domain.Types.AadhaarOtpReq
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AadhaarOtpReq as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AadhaarOtpReq.AadhaarOtpReq] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m (Maybe Domain.Types.AadhaarOtpReq.AadhaarOtpReq))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m ())
updateByPrimaryKey (Domain.Types.AadhaarOtpReq.AadhaarOtpReq {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.requestId requestId,
      Se.Set Beam.requestMessage requestMessage,
      Se.Set Beam.statusCode statusCode,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AadhaarOtpReq Domain.Types.AadhaarOtpReq.AadhaarOtpReq where
  fromTType' (Beam.AadhaarOtpReqT {..}) = do
    pure $
      Just
        Domain.Types.AadhaarOtpReq.AadhaarOtpReq
          { driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            requestId = requestId,
            requestMessage = requestMessage,
            statusCode = statusCode,
            transactionId = transactionId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AadhaarOtpReq Domain.Types.AadhaarOtpReq.AadhaarOtpReq where
  toTType' (Domain.Types.AadhaarOtpReq.AadhaarOtpReq {..}) = do
    Beam.AadhaarOtpReqT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.requestId = requestId,
        Beam.requestMessage = requestMessage,
        Beam.statusCode = statusCode,
        Beam.transactionId = transactionId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
