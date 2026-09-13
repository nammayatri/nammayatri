module ExternalBPP.ExternalAPI.Metro.KMRL.Cancel
  ( softCancelTicket,
    hardCancelTicket,
    SoftCancelQuote (..),
    HardCancelResult (..),
  )
where

import Domain.Types.Extra.IntegratedBPPConfig (KMRLConfig (..))
import ExternalBPP.ExternalAPI.Metro.KMRL.Order (kmrlPayload, requiredPayload, ticketRefIdEnvelope)
import ExternalBPP.ExternalAPI.Metro.KMRL.Transport (callKMRL)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Network.HTTP.Client as HTTP

data SoftCancelQuote = SoftCancelQuote
  { ticketFare :: Double,
    refundType :: Maybe Text,
    ticketRefId :: Maybe Text,
    sourceStationDisplayName :: Maybe Text,
    destinationStationDisplayName :: Maybe Text,
    travellers :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data HardCancelResult = HardCancelResult
  { ticketRefId :: Maybe Text,
    ticketStatus :: Maybe Text,
    ticketType :: Maybe Text,
    travellers :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

softCancelTicket ::
  (MonadFlow m, EncFlow m r, MonadReader r m) =>
  KMRLConfig ->
  HTTP.Manager ->
  Text ->
  m SoftCancelQuote
softCancelTicket config manager ticketRefId = do
  logInfo $ "[KMRL:SoftCancel] quoting refund for ticketRefId: " <> ticketRefId
  callKMRL config manager config.softCancelUrl True (ticketRefIdEnvelope ticketRefId)
    >>= requiredPayload "softCancelTicket"

hardCancelTicket ::
  (MonadFlow m, EncFlow m r, MonadReader r m) =>
  KMRLConfig ->
  HTTP.Manager ->
  Text ->
  m (Maybe HardCancelResult)
hardCancelTicket config manager ticketRefId = do
  logInfo $ "[KMRL:HardCancel] cancelling ticketRefId: " <> ticketRefId
  callKMRL config manager config.hardCancelUrl True (ticketRefIdEnvelope ticketRefId)
    >>= kmrlPayload "hardCancelTicket"
