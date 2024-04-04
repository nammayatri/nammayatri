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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AadhaarOtpReq as Beam

create :: KvDbFlow m r => (Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.AadhaarOtpReq.AadhaarOtpReq] -> m ())
createMany = traverse_ create

deleteByPersonId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m (Maybe Domain.Types.AadhaarOtpReq.AadhaarOtpReq))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.AadhaarOtpReq.AadhaarOtpReq -> m ())
updateByPrimaryKey (Domain.Types.AadhaarOtpReq.AadhaarOtpReq {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.requestId requestId,
      Se.Set Beam.requestMessage requestMessage,
      Se.Set Beam.statusCode statusCode,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.createdAt createdAt,
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
