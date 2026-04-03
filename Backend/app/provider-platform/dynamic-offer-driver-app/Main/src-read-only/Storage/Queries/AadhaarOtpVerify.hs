{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.AadhaarOtpVerify where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.AadhaarOtpVerify
import qualified Storage.Beam.AadhaarOtpVerify as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify] -> m ())
createMany = traverse_ create
deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify -> m (Maybe Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify -> m ())
updateByPrimaryKey (Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify {..}) = do {_now <- getCurrentTime;
                                                                               updateWithKV [Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
                                                                                             Se.Set Beam.requestId requestId,
                                                                                             Se.Set Beam.requestMessage requestMessage,
                                                                                             Se.Set Beam.statusCode statusCode,
                                                                                             Se.Set Beam.transactionId transactionId,
                                                                                             Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.AadhaarOtpVerify Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify
    where fromTType' (Beam.AadhaarOtpVerifyT {..}) = do pure $ Just Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify{driverId = Kernel.Types.Id.Id driverId,
                                                                                                                   id = Kernel.Types.Id.Id id,
                                                                                                                   requestId = requestId,
                                                                                                                   requestMessage = requestMessage,
                                                                                                                   statusCode = statusCode,
                                                                                                                   transactionId = transactionId,
                                                                                                                   createdAt = createdAt,
                                                                                                                   updatedAt = updatedAt}
instance ToTType' Beam.AadhaarOtpVerify Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify
    where toTType' (Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify {..}) = do Beam.AadhaarOtpVerifyT{Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                     Beam.id = Kernel.Types.Id.getId id,
                                                                                                     Beam.requestId = requestId,
                                                                                                     Beam.requestMessage = requestMessage,
                                                                                                     Beam.statusCode = statusCode,
                                                                                                     Beam.transactionId = transactionId,
                                                                                                     Beam.createdAt = createdAt,
                                                                                                     Beam.updatedAt = updatedAt}



