{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnCancel where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.HashMap.Strict as HashMap
import qualified Domain.Action.Beckn.FRFS.GWLink as GWLink
import qualified Domain.Action.Beckn.FRFS.GWLink as GWSA
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
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
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QTBP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS
import Tools.Error
import qualified Tools.SMS as Sms
import qualified Utils.Common.JWT.Config as GW
import qualified Utils.Common.JWT.TransitClaim as TC

data DOnCancel = DOnCancel
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    bppOrderId :: Text,
    bppItemId :: Text,
    transactionId :: Text,
    messageId :: Text,
    orderStatus :: Spec.OrderStatus,
    refundAmount :: Maybe HighPrecMoney,
    baseFare :: HighPrecMoney,
    cancellationCharges :: Maybe HighPrecMoney
  }

validateRequest :: DOnCancel -> Flow (Merchant, FTBooking.FRFSTicketBooking)
validateRequest DOnCancel {..} = do
  booking <- runInReplica $ QTBooking.findBySearchId (Id transactionId) >>= fromMaybeM (BookingDoesNotExist messageId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  when (totalPrice /= baseFare + fromMaybe 0 refundAmount + fromMaybe 0 cancellationCharges) $ throwError (InternalError "Fare Mismatch in onCancel Req")
  return (merchant, booking)

onCancel :: Merchant -> Booking.FRFSTicketBooking -> DOnCancel -> Flow ()
onCancel _ booking' dOnCancel = do
  let booking = booking' {Booking.bppOrderId = Just dOnCancel.bppOrderId}
  person <- runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mRiderNumber <- mapM decrypt person.mobileNumber
  case dOnCancel.orderStatus of
    Spec.SOFT_CANCELLED -> do
      void $ QTBooking.updateRefundCancellationChargesAndIsCancellableByBookingId dOnCancel.refundAmount dOnCancel.cancellationCharges (Just True) booking.id
    Spec.CANCELLED -> do
      val :: Maybe Bool <- Redis.get (makecancelledTtlKey booking.id)
      case val of
        Nothing -> do
          void $ QTBooking.updateStatusById FTBooking.COUNTER_CANCELLED booking.id
          void $ QTicket.updateAllStatusByBookingId DFRFSTicket.COUNTER_CANCELLED booking.id
          void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.COUNTER_CANCELLED) booking.id
        Just _ -> do
          void $ checkRefundAndCancellationCharges booking.id
          void $ QTBooking.updateStatusById FTBooking.CANCELLED booking.id
          void $ QTicket.updateAllStatusByBookingId DFRFSTicket.CANCELLED booking.id
          void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCELLED) booking.id
          void $ QTBP.updateStatusByTicketBookingId DTBP.REFUND_PENDING booking.id
          void $ QTBooking.updateIsBookingCancellableByBookingId (Just True) booking.id
          void $ QTBooking.updateCustomerCancelledByBookingId True booking.id
          void $ Redis.del (makecancelledTtlKey booking.id)
      void $ QPS.incrementTicketsBookedInEvent booking.riderId (- (booking.quantity))
      void $ CQP.clearPSCache booking.riderId
      void $ sendTicketCancelSMS mRiderNumber person.mobileCountryCode
    Spec.CANCEL_INITIATED -> do
      void $ QTBooking.updateStatusById FTBooking.CANCEL_INITIATED booking.id
      void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCEL_INITIATED) booking.id
    _ -> throwError $ InvalidRequest "Unexpected orderStatus received"
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
  return ()
  where
    checkRefundAndCancellationCharges bookingId = do
      booking <- runInReplica $ QTBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      case booking of
        Booking.FRFSTicketBooking {refundAmount = Just rfAmount, cancellationCharges = Just charges} -> do
          when (Just rfAmount /= dOnCancel.refundAmount) $
            throwError $ InternalError "Refund Amount mismatch in onCancel Req"
          when (Just charges /= dOnCancel.cancellationCharges) $
            throwError $ InternalError "Cancellation Charges mismatch in onCancel Req"
        _ -> throwError $ InternalError "Refund Amount or Cancellation Charges not found in booking"
    sendTicketCancelSMS :: Maybe Text -> Maybe Text -> Flow ()
    sendTicketCancelSMS mRiderNumber mRiderMobileCountryCode =
      whenJust booking'.partnerOrgId $ \pOrgId -> do
        fork "send ticket cancel sms" $
          withLogTag ("SMS:FRFSBookingId:" <> booking'.id.getId) $ do
            mobileNumber <- mRiderNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
            let mocId = booking'.merchantOperatingCityId
                countryCode = fromMaybe "+91" mRiderMobileCountryCode
                phoneNumber = countryCode <> mobileNumber

            buildSmsReq <-
              MessageBuilder.buildFRFSTicketCancelMessage mocId pOrgId $
                MessageBuilder.BuildFRFSTicketCancelMessageReq
                  { countOfTickets = booking'.quantity,
                    bookingId = booking'.id
                  }

            Sms.sendSMS booking'.merchantId mocId (buildSmsReq phoneNumber) >>= Sms.checkSmsResult

makecancelledTtlKey :: Id FTBooking.FRFSTicketBooking -> Text
makecancelledTtlKey bookingId = "FRFS:OnConfirm:CancelledTTL:bookingId-" <> bookingId.getId
