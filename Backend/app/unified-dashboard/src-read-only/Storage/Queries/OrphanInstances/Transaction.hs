{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Transaction where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Transaction
import qualified Storage.Beam.Transaction as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.Transaction Domain.Types.Transaction.Transaction
    where fromTType' (Beam.TransactionT {..}) = do pure $ Just Domain.Types.Transaction.Transaction{commonDriverId = commonDriverId,
                                                                                                    commonRideId = commonRideId,
                                                                                                    createdAt = createdAt,
                                                                                                    endpoint = endpoint,
                                                                                                    id = Kernel.Types.Id.Id id,
                                                                                                    merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                    request = request,
                                                                                                    requestorId = Kernel.Types.Id.Id <$> requestorId,
                                                                                                    response = response,
                                                                                                    responseError = responseError,
                                                                                                    serverName = serverName,
                                                                                                    updatedAt = updatedAt}
instance ToTType' Beam.Transaction Domain.Types.Transaction.Transaction
    where toTType' (Domain.Types.Transaction.Transaction {..}) = do Beam.TransactionT{Beam.commonDriverId = commonDriverId,
                                                                                      Beam.commonRideId = commonRideId,
                                                                                      Beam.createdAt = createdAt,
                                                                                      Beam.endpoint = endpoint,
                                                                                      Beam.id = Kernel.Types.Id.getId id,
                                                                                      Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                      Beam.request = request,
                                                                                      Beam.requestorId = Kernel.Types.Id.getId <$> requestorId,
                                                                                      Beam.response = response,
                                                                                      Beam.responseError = responseError,
                                                                                      Beam.serverName = serverName,
                                                                                      Beam.updatedAt = updatedAt}



