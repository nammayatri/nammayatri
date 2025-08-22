{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.Common where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as FRFSUtils
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFS.GWLink as GWLink
import qualified Domain.Action.Beckn.FRFS.GWLink as GWSA
import Domain.Types.BecknConfig
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Domain.Types.Merchant as Merchant
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBookingPayment as QTBP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS
import Tools.Error
import qualified Tools.SMS as Sms
import qualified Utils.Common.JWT.Config as GW
import qualified Utils.Common.JWT.TransitClaim as TC

data DOnSelect = DOnSelect
  { providerId :: Text,
    totalPrice :: Price,
    fareBreakUp :: [DFareBreakUp],
    bppItemId :: Text,
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text
  }

data DFareBreakUp = DFareBreakUp
  { title :: Text,
    price :: HighPrecMoney,
    pricePerUnit :: HighPrecMoney,
    quantity :: Int
  }

data DOrder = DOrder
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    fareBreakUp :: [DFareBreakUp],
    bppOrderId :: Text,
    bppItemId :: Text,
    transactionId :: Text,
    orderStatus :: Maybe Spec.OrderStatus,
    messageId :: Text,
    tickets :: [DTicket]
  }

data DTicket = DTicket
  { qrData :: Text,
    vehicleNumber :: Maybe Text,
    description :: Maybe Text,
    bppFulfillmentId :: Maybe Text,
    ticketNumber :: Text,
    validTill :: UTCTime,
    status :: Text,
    qrRefreshAt :: Maybe UTCTime
  }

data DTicketPayload = DTicketPayload
  { fromRouteProviderCode :: Text,
    toRouteProviderCode :: Text,
    adultQuantity :: Int,
    childQuantity :: Int,
    vehicleTypeProviderCode :: Text,
    expiry :: Text,
    ticketNumber :: Text,
    ticketAmount :: Money,
    refreshAt :: Maybe UTCTime
  }

handleCancelledStatus :: Merchant.Merchant -> DFRFSTicketBooking.FRFSTicketBooking -> HighPrecMoney -> HighPrecMoney -> Text -> Bool -> Flow ()
handleCancelledStatus merchant booking refundAmount cancellationCharges messageId counterCancellationPossible = do
  person <- runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mRiderNumber <- mapM decrypt person.mobileNumber
  val :: Maybe Text <- Redis.get (makecancelledTtlKey booking.id)
  if val /= Just messageId && counterCancellationPossible
    then do
      void $ QTBooking.updateStatusById DFRFSTicketBooking.COUNTER_CANCELLED booking.id
      void $ QTicket.updateAllStatusByBookingId DFRFSTicket.COUNTER_CANCELLED booking.id
      void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.COUNTER_CANCELLED) booking.id
    else do
      void $ checkRefundAndCancellationCharges booking.id refundAmount cancellationCharges
      void $ QTBooking.updateStatusById DFRFSTicketBooking.CANCELLED booking.id
      void $ QTicket.updateAllStatusByBookingId DFRFSTicket.CANCELLED booking.id
      void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCELLED) booking.id
      void $ QTBP.updateStatusByTicketBookingId DTBP.REFUND_PENDING booking.id
      void $ QTBooking.updateIsBookingCancellableByBookingId (Just True) booking.id
      void $ QTBooking.updateCustomerCancelledByBookingId True booking.id
      void $ Redis.del (makecancelledTtlKey booking.id)
      riderConfig <- QRC.findByMerchantOperatingCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
      when riderConfig.enableAutoJourneyRefund $
        FRFSUtils.markAllRefundBookings booking booking.riderId
  void $ QPS.incrementTicketsBookedInEvent booking.riderId (- (booking.quantity))
  void $ CQP.clearPSCache booking.riderId
  void $ sendTicketCancelSMS mRiderNumber person.mobileCountryCode booking
  handleGoogleWalletStatusUpdate booking
  bapConfig <-
    CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback booking.merchantOperatingCityId merchant.id (show Spec.FRFS) (FRFSUtils.frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType)
      >>= fromMaybeM (InternalError "Beckn Config not found")
  updateTotalOrderValueAndSettlementAmount booking bapConfig

checkRefundAndCancellationCharges :: Id DFRFSTicketBooking.FRFSTicketBooking -> HighPrecMoney -> HighPrecMoney -> Flow ()
checkRefundAndCancellationCharges bookingId refundAmount cancellationCharges = do
  booking <- runInReplica $ QTBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  case booking of
    DFRFSTicketBooking.FRFSTicketBooking {refundAmount = Just rfAmount, cancellationCharges = Just charges} -> do
      when (Just rfAmount /= Just refundAmount) $
        throwError $ InternalError $ "Refund Amount mismatch in onCancel Req " <> "refundAmount: " <> show refundAmount <> " rfAmount: " <> show rfAmount
      when (Just charges /= Just cancellationCharges) $
        throwError $ InternalError $ "Cancellation Charges mismatch in onCancel Req " <> "cancellationCharges: " <> show cancellationCharges <> " charges: " <> show charges
    _ -> throwError $ InternalError "Refund Amount or Cancellation Charges not found in booking"

sendTicketCancelSMS :: Maybe Text -> Maybe Text -> DFRFSTicketBooking.FRFSTicketBooking -> Flow ()
sendTicketCancelSMS mRiderNumber mRiderMobileCountryCode booking =
  whenJust booking.partnerOrgId $ \pOrgId -> do
    fork "send ticket cancel sms" $
      withLogTag ("SMS:FRFSBookingId:" <> booking.id.getId) $ do
        mobileNumber <- mRiderNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
        let mocId = booking.merchantOperatingCityId
            countryCode = fromMaybe "+91" mRiderMobileCountryCode
            phoneNumber = countryCode <> mobileNumber

        mbBuildSmsReq <-
          MessageBuilder.buildFRFSTicketCancelMessage mocId pOrgId $
            MessageBuilder.BuildFRFSTicketCancelMessageReq
              { countOfTickets = booking.quantity,
                bookingId = booking.id
              }
        maybe
          (logError $ "SMS not sent, SMS template not found for partnerOrgId:" <> pOrgId.getId)
          ( \buildSmsReq -> do
              let smsReq = buildSmsReq phoneNumber
              Sms.sendSMS booking.merchantId mocId smsReq >>= Sms.checkSmsResult
          )
          mbBuildSmsReq

handleGoogleWalletStatusUpdate :: DFRFSTicketBooking.FRFSTicketBooking -> Flow ()
handleGoogleWalletStatusUpdate booking =
  whenJust booking.partnerOrgId $ \pOrgId -> do
    walletPOCfg <- do
      pOrgCfg <- CQPOC.findByIdAndCfgType pOrgId DPOC.WALLET_CLASS_NAME >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.WALLET_CLASS_NAME)
      DPOC.getWalletClassNameConfig pOrgCfg.config
    let mbClassName = HashMap.lookup booking.merchantOperatingCityId.getId walletPOCfg.className
    whenJust mbClassName $ \_ -> do
      fork ("updating status of tickets in google wallet for bookingId: " <> booking.id.getId) $ do
        tickets <- QTicket.findAllByTicketBookingId booking.id
        let serviceName = DEMSC.WalletService GW.GoogleWallet
        let mId = booking.merchantId
        let mocId' = booking.merchantOperatingCityId
        serviceAccount <- GWSA.getserviceAccount mId mocId' serviceName
        forM_ tickets $ \ticket -> do
          let googleStatus = GWLink.mapToGoogleTicketStatus ticket.status
          let resourceId = serviceAccount.saIssuerId <> "." <> ticket.id.getId
          let obj = TC.TransitObjectPatch {TC.state = show googleStatus}
          resp <- GWSA.getObjectGoogleWallet serviceAccount resourceId
          case resp of
            Nothing -> return ()
            Just _ -> do
              void $ GWSA.updateTicketStatusForGoogleWallet obj serviceAccount resourceId
              return ()

makecancelledTtlKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
makecancelledTtlKey bookingId = "FRFS:OnConfirm:CancelledTTL:bookingId-" <> bookingId.getId

updateTotalOrderValueAndSettlementAmount :: DFRFSTicketBooking.FRFSTicketBooking -> BecknConfig -> Flow ()
updateTotalOrderValueAndSettlementAmount booking bapConfig = do
  paymentBooking <- runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
  let finderFee :: Price = mkPrice Nothing $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
      finderFeeForEachTicket = modifyPrice finderFee $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational booking.quantity)
  tOrderPrice <- totalOrderValue paymentBooking.status booking
  let tOrderValue = modifyPrice tOrderPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational booking.quantity)
  settlementAmount <- tOrderValue `subtractPrice` finderFeeForEachTicket
  void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById settlementAmount tOrderValue booking.id

totalOrderValue :: DTBP.FRFSTicketBookingPaymentStatus -> DFRFSTicketBooking.FRFSTicketBooking -> Flow Price
totalOrderValue paymentBookingStatus booking =
  if paymentBookingStatus == DTBP.REFUND_PENDING || paymentBookingStatus == DTBP.REFUNDED
    then booking.price `addPrice` refundAmountToPrice -- Here the `refundAmountToPrice` value is in Negative
    else pure $ booking.price
  where
    refundAmountToPrice = mkPrice (Just INR) (fromMaybe (HighPrecMoney $ toRational (0 :: Int)) booking.refundAmount)
