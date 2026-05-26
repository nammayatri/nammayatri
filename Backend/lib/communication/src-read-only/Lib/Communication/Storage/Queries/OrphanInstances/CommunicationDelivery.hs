{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Communication.Storage.Queries.OrphanInstances.CommunicationDelivery where

import qualified Lib.Communication.Domain.Types.CommunicationDelivery
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Communication.Storage.Beam.CommunicationDelivery as Beam

instance FromTType' Beam.CommunicationDelivery Lib.Communication.Domain.Types.CommunicationDelivery.CommunicationDelivery where
  fromTType' (Beam.CommunicationDeliveryT {..}) = do
    pure $
      Just
        Lib.Communication.Domain.Types.CommunicationDelivery.CommunicationDelivery
          { channel = channel,
            communicationId = Kernel.Types.Id.Id communicationId,
            createdAt = createdAt,
            deliveredAt = deliveredAt,
            failureReason = failureReason,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            operatorId = operatorId,
            readAt = readAt,
            recipientId = recipientId,
            recipientRole = recipientRole,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CommunicationDelivery Lib.Communication.Domain.Types.CommunicationDelivery.CommunicationDelivery where
  toTType' (Lib.Communication.Domain.Types.CommunicationDelivery.CommunicationDelivery {..}) = do
    Beam.CommunicationDeliveryT
      { Beam.channel = channel,
        Beam.communicationId = Kernel.Types.Id.getId communicationId,
        Beam.createdAt = createdAt,
        Beam.deliveredAt = deliveredAt,
        Beam.failureReason = failureReason,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.operatorId = operatorId,
        Beam.readAt = readAt,
        Beam.recipientId = recipientId,
        Beam.recipientRole = recipientRole,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
