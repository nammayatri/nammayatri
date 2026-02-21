{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnConfirm where

import qualified Beckn.ACL.FRFS.OnConfirm as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.Aeson (eitherDecodeStrict')
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DOnConfirm
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import TransactionLogs.PushLogs

type API = Spec.OnConfirmAPIBS

handler :: SignatureAuthResult -> FlowServer API
handler = onConfirm

onConfirm :: SignatureAuthResult -> ByteString -> FlowHandler Spec.AckResponse
onConfirm _ reqBS = withFlowHandlerAPI $ do
  req <- case decodeOnConfirmReq reqBS of
    Right r -> pure r
    Left err -> throwError (InvalidRequest (toText err))
  transaction_id <- req.onConfirmReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  bookingId <- req.onConfirmReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  ticketBooking <- QFRFSTicketBooking.findById (Id bookingId) >>= fromMaybeM (InvalidRequest "Invalid booking id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback ticketBooking.merchantOperatingCityId ticketBooking.merchantId (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity ticketBooking
  routeStopMappingFromStation <- OTPRest.getRouteStopMappingByStopCode ticketBooking.fromStationCode integratedBppConfig
  routeStopMappingToStation <- OTPRest.getRouteStopMappingByStopCode ticketBooking.toStationCode integratedBppConfig
  let fromStationProviderCode = fromMaybe ticketBooking.fromStationCode (listToMaybe routeStopMappingFromStation <&> (.providerCode))
      toStationProviderCode = fromMaybe ticketBooking.toStationCode (listToMaybe routeStopMappingToStation <&> (.providerCode))
  logDebug $ "Received OnConfirm request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    dOnConfirmReq <- ACL.buildOnConfirmReq fromStationProviderCode toStationProviderCode req
    case dOnConfirmReq of
      Just onConfirmReq -> do
        (merchant, booking, quoteCategories) <- DOnConfirm.validateRequest onConfirmReq
        fork "onConfirm request processing" $
          Redis.whenWithLockRedis (onConfirmProcessingLockKey onConfirmReq.bppOrderId) 60 $
            DOnConfirm.onConfirm merchant booking quoteCategories onConfirmReq
        fork "FRFS onConfirm received pushing ondc logs" do
          void $ pushLogs "on_confirm" (toJSON req) merchant.id.getId "PUBLIC_TRANSPORT"
      Nothing -> DOnConfirm.onConfirmFailure bapConfig ticketBooking
  pure Utils.ack

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "FRFS:OnConfirm:bppOrderId-" <> id

onConfirmProcessingLockKey :: Text -> Text
onConfirmProcessingLockKey id = "FRFS:OnConfirm:Processing:bppOrderId-" <> id

decodeOnConfirmReq :: ByteString -> Either String Spec.OnConfirmReq
decodeOnConfirmReq bs =
  case eitherDecodeStrict' bs of
    Right v -> Right v
    Left _ ->
      case Utils.unescapeQuotedJSON bs of
        Just inner ->
          eitherDecodeStrict' inner
        Nothing ->
          Left "Unable to decode OnConfirmReq: invalid JSON format."
