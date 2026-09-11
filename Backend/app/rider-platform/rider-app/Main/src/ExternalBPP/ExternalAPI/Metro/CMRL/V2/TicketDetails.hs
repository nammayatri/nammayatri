module ExternalBPP.ExternalAPI.Metro.CMRL.V2.TicketDetails
  ( getTicketDetails,
    ChennaiV2TicketDetail (..),
  )
where

import Data.Aeson
import qualified Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import Kernel.External.Encryption
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Utils.Common
import Servant hiding (throwError)

data ChennaiV2TicketDetail = ChennaiV2TicketDetail
  { statusCode :: Maybe Text,
    returnCode :: Maybe Text
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON ChennaiV2TicketDetail where
  parseJSON = withObject "ChennaiV2TicketDetail" $ \v ->
    ChennaiV2TicketDetail
      <$> v .:? "statusCode"
      <*> v .:? "returnCode"

type TicketDetailsAPI =
  "api" :> "qr" :> "v1" :> "tickets" :> "details-by-ticketId"
    :> Header "Authorization" Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> QueryParam' '[Required, Strict] "ticketId" Text
    :> Get '[JSON] [ChennaiV2TicketDetail]

ticketDetailsAPI :: Proxy TicketDetailsAPI
ticketDetailsAPI = Proxy

getTicketDetails ::
  ( MonadTime m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    HasMasterCloudForwarder r
  ) =>
  Domain.Types.Extra.IntegratedBPPConfig.CMRLV2Config ->
  Text ->
  m (Maybe Text)
getTicketDetails config ticketId = do
  logInfo $ "[CMRLV2:TicketDetails] Fetching details for ticketId: " <> ticketId
  let eulerClient accessToken =
        ET.client ticketDetailsAPI (Just ("Bearer " <> accessToken)) config.operatorNameId ticketId
  detailsRes <- callCMRLV2API config eulerClient "getTicketDetails" ticketDetailsAPI
  case detailsRes of
    (detail : _)
      | detail.returnCode == Just "0" -> do
        logInfo $ "[CMRLV2:TicketDetails] Got statusCode=" <> show detail.statusCode <> " for ticketId=" <> ticketId
        pure detail.statusCode
      | otherwise -> do
        logWarning $ "[CMRLV2:TicketDetails] Non-zero returnCode=" <> show detail.returnCode <> " for ticketId=" <> ticketId
        pure Nothing
    [] -> do
      logWarning $ "[CMRLV2:TicketDetails] Empty detail list for ticketId=" <> ticketId
      pure Nothing
