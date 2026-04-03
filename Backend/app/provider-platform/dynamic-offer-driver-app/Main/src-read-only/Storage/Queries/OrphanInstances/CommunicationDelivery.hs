{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.CommunicationDelivery where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.CommunicationDelivery
import qualified Storage.Beam.CommunicationDelivery as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.CommunicationDelivery Domain.Types.CommunicationDelivery.CommunicationDelivery
    where fromTType' (Beam.CommunicationDeliveryT {..}) = do pure $ Just Domain.Types.CommunicationDelivery.CommunicationDelivery{channel = channel,
                                                                                                                                  communicationId = Kernel.Types.Id.Id communicationId,
                                                                                                                                  createdAt = createdAt,
                                                                                                                                  deliveredAt = deliveredAt,
                                                                                                                                  failureReason = failureReason,
                                                                                                                                  fleetOwnerId = fleetOwnerId,
                                                                                                                                  id = Kernel.Types.Id.Id id,
                                                                                                                                  merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                  merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                  operatorId = operatorId,
                                                                                                                                  readAt = readAt,
                                                                                                                                  recipientId = Kernel.Types.Id.Id recipientId,
                                                                                                                                  recipientRole = recipientRole,
                                                                                                                                  status = status,
                                                                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.CommunicationDelivery Domain.Types.CommunicationDelivery.CommunicationDelivery
    where toTType' (Domain.Types.CommunicationDelivery.CommunicationDelivery {..}) = do Beam.CommunicationDeliveryT{Beam.channel = channel,
                                                                                                                    Beam.communicationId = Kernel.Types.Id.getId communicationId,
                                                                                                                    Beam.createdAt = createdAt,
                                                                                                                    Beam.deliveredAt = deliveredAt,
                                                                                                                    Beam.failureReason = failureReason,
                                                                                                                    Beam.fleetOwnerId = fleetOwnerId,
                                                                                                                    Beam.id = Kernel.Types.Id.getId id,
                                                                                                                    Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                    Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                    Beam.operatorId = operatorId,
                                                                                                                    Beam.readAt = readAt,
                                                                                                                    Beam.recipientId = Kernel.Types.Id.getId recipientId,
                                                                                                                    Beam.recipientRole = recipientRole,
                                                                                                                    Beam.status = status,
                                                                                                                    Beam.updatedAt = updatedAt}



