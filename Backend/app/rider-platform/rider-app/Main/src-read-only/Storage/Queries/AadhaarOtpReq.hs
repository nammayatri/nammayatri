{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AadhaarOtpReq where

import qualified Domain.Types.AadhaarOtpReq
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AadhaarOtpReq as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AadhaarOtpReq.AadhaarOtpReq] -> m ())
createMany = traverse_ create

deleteByPersonIdForGenerate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonIdForGenerate personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m (Maybe Domain.Types.AadhaarOtpReq.AadhaarOtpReq))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m ())
updateByPrimaryKey (Domain.Types.AadhaarOtpReq.AadhaarOtpReq {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.requestId requestId,
      Se.Set Beam.requestMessage requestMessage,
      Se.Set Beam.statusCode statusCode,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AadhaarOtpReq Domain.Types.AadhaarOtpReq.AadhaarOtpReq where
  fromTType' (Beam.AadhaarOtpReqT {..}) = do
    pure $
      Just
        Domain.Types.AadhaarOtpReq.AadhaarOtpReq
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            personId = Kernel.Types.Id.Id personId,
            requestId = requestId,
            requestMessage = requestMessage,
            statusCode = statusCode,
            transactionId = transactionId,
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt
          }

instance ToTType' Beam.AadhaarOtpReq Domain.Types.AadhaarOtpReq.AadhaarOtpReq where
  toTType' (Domain.Types.AadhaarOtpReq.AadhaarOtpReq {..}) = do
    Beam.AadhaarOtpReqT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.requestId = requestId,
        Beam.requestMessage = requestMessage,
        Beam.statusCode = statusCode,
        Beam.transactionId = transactionId,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
