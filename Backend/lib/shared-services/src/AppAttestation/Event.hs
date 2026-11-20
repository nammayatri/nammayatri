{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Event
  ( AttestationEvent (..),
    publishAttestationEvent,
  )
where

import AppAttestation.Domain.Types.Common (AttestationMode, FallbackMethod)
import AppAttestation.Types
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common

data AttestationEvent = AttestationEvent
  { eventId :: Text,
    timestamp :: UTCTime,
    service :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    endpoint :: Endpoint,
    mode :: AttestationMode,
    platform :: Maybe Platform,
    outcome :: Outcome,
    reasons :: [Reason],
    decision :: Text,
    enforcedDecision :: Text,
    fallbackMethod :: Maybe FallbackMethod,
    userKind :: UserKind,
    verdictSummary :: Maybe Text,
    clientVersion :: Maybe Text,
    device :: Maybe Text,
    clientIp :: Maybe Text,
    identifierHash :: Maybe Text,
    latencyMs :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

attestationEventTopic :: Text
attestationEventTopic = "app-attestation-events"

publishAttestationEvent :: (MonadFlow m, MonadReader r m, HasKafkaProducer r) => AttestationEvent -> m ()
publishAttestationEvent event =
  fork "app-attestation-event" $
    produceMessage (attestationEventTopic, Just $ TE.encodeUtf8 event.eventId) event
