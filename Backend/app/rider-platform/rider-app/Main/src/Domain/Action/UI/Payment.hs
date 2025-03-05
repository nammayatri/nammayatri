{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payment
  ( DPayment.PaymentStatusResp (..),
    createOrder,
    getStatus,
    getOrder,
    juspayWebhookHandler,
  )
where

import Control.Applicative ((<|>))
import qualified Domain.Action.UI.BBPS as BBPS
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Types as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Servant (BasicAuthData)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.PlaceBasedServiceConfig as CQPBSC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.TicketBooking as QTB
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

-- create order -----------------------------------------------------

createOrder ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DRide.Ride ->
  Flow Payment.CreateOrderResp
createOrder (personId, merchantId) rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  -- ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus $ show ride.status)
  totalFare <- ride.totalFare & fromMaybeM (RideFieldNotPresent "totalFare")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  -- person <- QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  riderId <- B.runInReplica $ QRide.findRiderIdByRideId ride.id >>= fromMaybeM (InternalError "riderId not found")
  -- riderId <- QRide.findRiderIdByRideId ride.id >>= fromMaybeM (InternalError "riderId not found")
  unless (person.id == riderId) $ throwError NotAnExecutor
  customerEmail <- person.email & fromMaybeM (PersonFieldNotPresent "email") >>= decrypt
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing Payment.Normal
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = rideId.getId,
            orderShortId = ride.shortId.getShortId, -- should be Alphanumeric with character length less than 18.
            amount = totalFare.amount,
            customerId = person.id.getId,
            customerEmail,
            customerPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing, --- assigned in shared kernel
            splitSettlementDetails = Payment.mkSplitSettlementDetails isSplitEnabled totalFare.amount []
          }

  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
      createOrderCall = Payment.createOrder merchantId person.merchantOperatingCityId Nothing Payment.Normal -- api call
  DPayment.createOrderService commonMerchantId (Just $ cast person.merchantOperatingCityId) commonPersonId createOrderReq createOrderCall >>= fromMaybeM (InternalError "Order expired please try again")

-- order status -----------------------------------------------------

getStatus ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    L.MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
getStatus (personId, merchantId) orderId = do
  ticketBooking <- QTB.findById (cast orderId)
  mocId <- ticketBooking <&> (.merchantOperatingCityId) & fromMaybeM (InternalError "MerchantOperatingCityId not found in booking") ----- fix the api and pass mocId in params
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId mocId (ticketBooking <&> (.ticketPlaceId)) Payment.Normal -- api call
  DPayment.orderStatusService commonPersonId orderId orderStatusCall

getOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DOrder.PaymentOrder ->
  m DOrder.PaymentOrderAPIEntity
getOrder (personId, _) orderId = do
  order <- B.runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  -- order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  unless (order.personId == cast personId) $ throwError NotAnExecutor
  mkOrderAPIEntity order

mkOrderAPIEntity :: EncFlow m r => DOrder.PaymentOrder -> m DOrder.PaymentOrderAPIEntity
mkOrderAPIEntity DOrder.PaymentOrder {..} = do
  clientAuthToken_ <- decrypt `mapM` clientAuthToken
  return $ DOrder.PaymentOrderAPIEntity {clientAuthToken = clientAuthToken_, ..}

-- webhook ----------------------------------------------------------

juspayWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe Payment.PaymentServiceType ->
  Maybe Text ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayWebhookHandler merchantShortId mbCity mbServiceType mbPlaceId authData value = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  let city = fromMaybe merchant.defaultCity mbCity
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show city)
  let merchantId = merchant.id
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName (Id id) (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig' <- do
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCity.id (getPaymentServiceByType mbServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOperatingCity.id.getId "Payment" (show Payment.Juspay))
  paymentServiceConfig <- do
    case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig'.serviceConfig of
      Just (DMSC.PaymentServiceConfig vsc) -> pure vsc
      Just (DMSC.MetroPaymentServiceConfig vsc) -> pure vsc
      Just (DMSC.BusPaymentServiceConfig vsc) -> pure vsc
      Just (DMSC.BbpsPaymentServiceConfig vsc) -> pure vsc
      Just (DMSC.MultiModalPaymentServiceConfig vsc) -> pure vsc
      _ -> throwError $ InternalError "Unknown Service Config"
  orderWebhookResponse <- Juspay.orderStatusWebhook paymentServiceConfig DPayment.juspayWebhookService authData value
  osr <- case orderWebhookResponse of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  (orderShortId, status) <- getOrderData osr
  logDebug $ "order short Id from Response bap webhook: " <> show orderShortId
  Redis.whenWithLockRedis (mkOrderStatusCheckKey orderShortId status) 60 $ do
    case mbServiceType of
      Just Payment.FRFSBooking -> void $ FRFSTicketService.webhookHandlerFRFSTicket (ShortId orderShortId) merchantId
      Just Payment.FRFSBusBooking -> void $ FRFSTicketService.webhookHandlerFRFSTicket (ShortId orderShortId) merchantId
      Just Payment.BBPS -> void $ BBPS.webhookHandlerBBPS (ShortId orderShortId) merchantId
      _ -> pure ()
  pure Ack
  where
    getPaymentServiceByType = \case
      Just Payment.Normal -> DMSC.PaymentService Payment.Juspay
      Just Payment.BBPS -> DMSC.BbpsPaymentService Payment.Juspay
      Just Payment.FRFSBooking -> DMSC.MetroPaymentService Payment.Juspay
      Just Payment.FRFSBusBooking -> DMSC.BusPaymentService Payment.Juspay
      Just Payment.FRFSMultiModalBooking -> DMSC.MultiModalPaymentService Payment.Juspay
      Nothing -> DMSC.PaymentService Payment.Juspay
    getOrderData osr = case osr of
      Payment.OrderStatusResp {..} -> pure (orderShortId, transactionStatus)
      _ -> throwError $ InternalError "Order Id not found in response."

mkOrderStatusCheckKey :: Text -> Payment.TransactionStatus -> Text
mkOrderStatusCheckKey orderId status = "lockKey:orderId:" <> orderId <> ":status" <> show status
