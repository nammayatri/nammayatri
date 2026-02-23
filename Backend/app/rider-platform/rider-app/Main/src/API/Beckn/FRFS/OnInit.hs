{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnInit where

import qualified Beckn.ACL.FRFS.OnInit as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.Aeson (eitherDecodeStrict')
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import qualified Domain.Types.Extra.IntegratedBPPConfig as DIBC
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import TransactionLogs.PushLogs

type API = Spec.OnInitAPIBS

handler :: SignatureAuthResult -> FlowServer API
handler = onInit

onInit :: SignatureAuthResult -> ByteString -> FlowHandler Spec.AckResponse
onInit _ reqBS = withFlowHandlerAPI $ do
  req <- case decodeOnInitReq reqBS of
    Right r -> pure r
    Left err -> throwError (InvalidRequest (toText err))
  transaction_id <- req.onInitReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnInit request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    onInitReq <- ACL.buildOnInitReq req
    Redis.whenWithLockRedis (onInitLockKey onInitReq.messageId) 60 $ do
      (merchant, booking, quoteCategories) <- DOnInit.validateRequest onInitReq

      integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
      let fareCachingAllowed = case integratedBppConfig.providerConfig of
            DIBC.ONDC ondcCfg -> fromMaybe False (DIBC.fareCachingAllowed ondcCfg)
            _ -> False

      if fareCachingAllowed
        then do
          logInfo $ "Fare caching enabled for booking: " <> booking.id.getId <> ". Skipping domain onInit processing."
          now <- getCurrentTime
          fork "FRFS on_init fare_caching" $ do
            if onInitReq.totalPrice == booking.totalPrice
              then do
                void $ QFRFSTicketBooking.updateOndcOnInitReceivedById (Just True) (Just now) booking.id
                whenJust onInitReq.validTill $ \validity -> void $ QFRFSTicketBooking.updateValidTillById validity booking.id
                void $ QFRFSTicketBooking.updateBppBankDetailsById (Just onInitReq.bankAccNum) (Just onInitReq.bankCode) booking.id
                whenJust onInitReq.bppOrderId $ \bppOrderId -> void $ QFRFSTicketBooking.updateBPPOrderIdById (Just bppOrderId) booking.id
              else do
                logError $ "Price mismatch for booking: " <> booking.id.getId <> ". Expected: " <> show booking.totalPrice <> ", Received: " <> show onInitReq.totalPrice <> ". Marking booking as failed."
                void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBookingStatus.FAILED booking.id
        else do
          fork "FRFS on_init processing" $ do
            Redis.whenWithLockRedis (onInitProcessingLockKey onInitReq.messageId) 60 $
              DOnInit.onInit onInitReq merchant booking quoteCategories Nothing

      fork "FRFS onInit received pushing ondc logs" do
        void $ pushLogs "on_init" (toJSON req) merchant.id.getId "PUBLIC_TRANSPORT"
  pure Utils.ack

onInitLockKey :: Text -> Text
onInitLockKey id = "FRFS:OnInit:MessageId-" <> id

onInitProcessingLockKey :: Text -> Text
onInitProcessingLockKey id = "FRFS:OnInit:Processing:MessageId-" <> id

decodeOnInitReq :: ByteString -> Either String Spec.OnInitReq
decodeOnInitReq bs =
  case eitherDecodeStrict' bs of
    Right v -> Right v
    Left _ ->
      case Utils.unescapeQuotedJSON bs of
        Just inner ->
          eitherDecodeStrict' inner
        Nothing ->
          Left "Unable to decode OnInitReq: invalid JSON format."
