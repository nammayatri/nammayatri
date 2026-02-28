{-
  Processor for fleet-communication-dispatch Kafka topic.
  Decodes CommunicationDeliveryDispatchPayload and calls driver app's processFleetCommunicationDeliveryPayload.
-}

module Consumer.FleetCommunication.Processor
  ( processFleetCommunicationDelivery,
  )
where

import "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.Communication
  ( CommunicationDeliveryDispatchPayload,
    processFleetCommunicationDeliveryPayload,
  )
import Environment
import EulerHS.Prelude
import Kernel.Utils.Common (withLogTag)

processFleetCommunicationDelivery :: CommunicationDeliveryDispatchPayload -> Flow ()
processFleetCommunicationDelivery payload =
  withLogTag ("deliveryId:" <> payload.deliveryId) $
    processFleetCommunicationDeliveryPayload payload
