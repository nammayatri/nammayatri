{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnConfirm where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Data.Aeson
import Data.HashMap.Strict
import qualified Data.Text as T
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.GWLink as GWSA
import Domain.Types.BecknConfig
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSQuote as FQ
import qualified Domain.Types.FRFSRecon as Recon
import qualified Domain.Types.FRFSTicket as Ticket
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant as Merchant
import qualified Domain.Types.PartnerOrgConfig as DPOC
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude ((+||), (||+))
import ExternalBPP.CallAPI
import Kernel.Beam.Functions
import Kernel.Beam.Functions as B
import Kernel.External.Encryption as ENC
import Kernel.Prelude as Prelude hiding (lookup)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchOpCity
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.CachedQueries.Station as CQStation
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSRecon as QRecon
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBokingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS
import qualified Storage.Queries.Station as QStation
import Tools.Error
import qualified Tools.SMS as Sms
import qualified Utils.Common.JWT.Config as GW
import qualified Utils.Common.JWT.TransitClaim as TC
import Web.JWT hiding (claims)

validateRequest :: DOrder -> Flow (Merchant, Booking.FRFSTicketBooking)
validateRequest DOrder {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QTBooking.findById (Id messageId) >>= fromMaybeM (BookingDoesNotExist messageId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime

  if booking.validTill < now
    then do
      -- Booking is expired
      merchantOperatingCity <- QMerchOpCity.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
      bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchantId) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId:- " <> merchantId.getId)
      void $ QTBooking.updateBPPOrderIdAndStatusById (Just bppOrderId) Booking.FAILED booking.id
      void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
      let updatedBooking = booking {Booking.bppOrderId = Just bppOrderId}
      void $ cancel merchant merchantOperatingCity bapConfig Spec.CONFIRM_CANCEL updatedBooking DIBC.APPLICATION
      throwM $ InvalidRequest "Booking expired, initated cancel request"
    else return (merchant, booking)

onConfirmFailure :: BecknConfig -> Booking.FRFSTicketBooking -> Flow ()
onConfirmFailure bapConfig ticketBooking = do
  merchant <- QMerch.findById ticketBooking.merchantId >>= fromMaybeM (MerchantNotFound ticketBooking.merchantId.getId)
  merchantOperatingCity <- QMerchOpCity.findById ticketBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound ticketBooking.merchantOperatingCityId.getId)
  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED ticketBooking.id
  void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING ticketBooking.id
  void $ cancel merchant merchantOperatingCity bapConfig Spec.CONFIRM_CANCEL ticketBooking DIBC.APPLICATION

onConfirm :: Merchant -> Booking.FRFSTicketBooking -> DOrder -> Flow ()
onConfirm merchant booking' dOrder = do
  let booking = booking' {Booking.bppOrderId = Just dOrder.bppOrderId}
  let discountedTickets = fromMaybe 0 booking.discountedTickets
  tickets <- createTickets booking dOrder.tickets discountedTickets
  void $ QTicket.createMany tickets
  void $ QTBooking.updateBPPOrderIdAndStatusById (Just dOrder.bppOrderId) Booking.CONFIRMED booking.id
  person <- runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mRiderNumber <- mapM ENC.decrypt person.mobileNumber
  buildReconTable merchant booking dOrder tickets mRiderNumber
  void $ sendTicketBookedSMS mRiderNumber person.mobileCountryCode
  void $ QPS.incrementTicketsBookedInEvent booking.riderId booking.quantity
  void $ CQP.clearPSCache booking.riderId
  whenJust booking.partnerOrgId $ \pOrgId -> do
    walletPOCfg <- do
      pOrgCfg <- CQPOC.findByIdAndCfgType pOrgId DPOC.WALLET_CLASS_NAME >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.WALLET_CLASS_NAME)
      DPOC.getWalletClassNameConfig pOrgCfg.config
    let mbClassName = lookup booking.merchantOperatingCityId.getId walletPOCfg.className
    whenJust mbClassName $ \className -> do
      fork ("adding googleJWTUrl" <> " Booking Id: " <> booking.id.getId) $ do
        let serviceName = DEMSC.WalletService GW.GoogleWallet
        let mId = booking'.merchantId
        let mocId' = booking'.merchantOperatingCityId
        serviceAccount <- GWSA.getserviceAccount mId mocId' serviceName
        transitObjects' <- createTransitObjects booking tickets person serviceAccount className
        url <- mkGoogleWalletLink serviceAccount transitObjects'
        void $ QTBooking.updateGoogleWalletLinkById (Just url) booking.id
  return ()
  where
    sendTicketBookedSMS :: Maybe Text -> Maybe Text -> Flow ()
    sendTicketBookedSMS mRiderNumber mRiderMobileCountryCode =
      whenJust booking'.partnerOrgId $ \pOrgId -> do
        fork "send ticket booked sms" $
          withLogTag ("SMS:FRFSBookingId:" <> booking'.id.getId) $ do
            mobileNumber <- mRiderNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
            smsCfg <- asks (.smsCfg)
            let mocId = booking'.merchantOperatingCityId
                countryCode = fromMaybe "+91" mRiderMobileCountryCode
                phoneNumber = countryCode <> mobileNumber
                sender = smsCfg.sender
            mbMsg <-
              MessageBuilder.buildFRFSTicketBookedMessage pOrgId $
                MessageBuilder.BuildFRFSTicketBookedMessageReq
                  { countOfTickets = booking'.quantity,
                    bookingId = booking'.id
                  }
            if isJust mbMsg
              then do
                let message = fromJust mbMsg
                logDebug $ "SMS Message:" +|| message ||+ ""
                Sms.sendSMS booking'.merchantId mocId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
              else do
                logError $ "SMS not sent, SMS template not found for partnerOrgId:" <> pOrgId.getId

buildReconTable :: Merchant -> Booking.FRFSTicketBooking -> DOrder -> [Ticket.FRFSTicket] -> Maybe Text -> Flow ()
buildReconTable merchant booking _dOrder tickets mRiderNumber = do
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  quote <- runInReplica $ QFRFSQuote.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)
  fromStation <- runInReplica $ QStation.findById booking.fromStationId >>= fromMaybeM (InternalError "Station not found")
  toStation <- runInReplica $ QStation.findById booking.toStationId >>= fromMaybeM (InternalError "Station not found")
  transactionRefNumber <- booking.paymentTxnId & fromMaybeM (InternalError "Payment Txn Id not found in booking")
  txn <- runInReplica $ QPaymentTransaction.findById (Id transactionRefNumber) >>= fromMaybeM (InvalidRequest "Payment Transaction not found for approved TicketBookingId")
  paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
  let paymentBookingStatus = paymentBooking.status
  now <- getCurrentTime
  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "BPP Order Id not found in booking")
  let finderFee :: Price = mkPrice Nothing $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee -- FIXME
      finderFeeForEachTicket = modifyPrice finderFee $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational booking.quantity)
  tOrderPrice <- totalOrderValue paymentBookingStatus booking
  let tOrderValue = modifyPrice tOrderPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational quote.quantity)
  settlementAmount <- tOrderValue `subtractPrice` finderFeeForEachTicket
  let reconEntry =
        Recon.FRFSRecon
          { Recon.id = "",
            Recon.beneficiaryIFSC = booking.bppBankCode,
            Recon.beneficiaryBankAccount = booking.bppBankAccountNumber,
            Recon.buyerFinderFee = finderFee,
            Recon.collectorIFSC = bapConfig.bapIFSC,
            Recon.collectorSubscriberId = bapConfig.subscriberId,
            Recon.date = show now,
            Recon.destinationStationCode = toStation.code,
            Recon.differenceAmount = Nothing,
            Recon.fare = quote.price,
            Recon.frfsTicketBookingId = booking.id,
            Recon.message = Nothing,
            Recon.mobileNumber = mRiderNumber,
            Recon.networkOrderId = bppOrderId,
            Recon.receiverSubscriberId = booking.bppSubscriberId,
            Recon.settlementAmount,
            Recon.settlementDate = Nothing,
            Recon.settlementReferenceNumber = Nothing,
            Recon.sourceStationCode = fromStation.code,
            Recon.transactionUUID = txn.txnUUID,
            Recon.ticketNumber = "",
            Recon.ticketQty = booking.quantity,
            Recon.time = show now,
            Recon.txnId = txn.txnId,
            Recon.totalOrderValue = tOrderValue,
            Recon.transactionRefNumber = transactionRefNumber,
            Recon.merchantId = Just booking.merchantId,
            Recon.merchantOperatingCityId = Just booking.merchantOperatingCityId,
            Recon.createdAt = now,
            Recon.updatedAt = now,
            Recon.ticketStatus = Nothing,
            Recon.providerId = booking.providerId,
            Recon.providerName = booking.providerName
          }

  reconEntries <- mapM (buildRecon reconEntry) tickets
  void $ QRecon.createMany reconEntries

totalOrderValue :: DFRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> Booking.FRFSTicketBooking -> Flow Price
totalOrderValue paymentBookingStatus booking =
  if paymentBookingStatus == DFRFSTicketBookingPayment.REFUND_PENDING || paymentBookingStatus == DFRFSTicketBookingPayment.REFUNDED
    then booking.price `addPrice` refundAmountToPrice -- Here the `refundAmountToPrice` value is in Negative
    else pure $ booking.price
  where
    refundAmountToPrice = mkPrice (Just INR) (fromMaybe (HighPrecMoney $ toRational (0 :: Int)) booking.refundAmount)

mkTicket :: Booking.FRFSTicketBooking -> DTicket -> Bool -> Flow Ticket.FRFSTicket
mkTicket booking dTicket isTicketFree = do
  now <- getCurrentTime
  ticketId <- generateGUID
  (_, status_) <- Utils.getTicketStatus booking dTicket

  return
    Ticket.FRFSTicket
      { Ticket.frfsTicketBookingId = booking.id,
        Ticket.id = ticketId,
        Ticket.description = dTicket.description,
        Ticket.qrData = dTicket.qrData,
        Ticket.qrRefreshAt = dTicket.qrRefreshAt,
        Ticket.riderId = booking.riderId,
        Ticket.status = status_,
        Ticket.ticketNumber = dTicket.ticketNumber,
        Ticket.validTill = dTicket.validTill,
        Ticket.merchantId = booking.merchantId,
        Ticket.merchantOperatingCityId = booking.merchantOperatingCityId,
        Ticket.partnerOrgId = booking.partnerOrgId,
        Ticket.partnerOrgTransactionId = booking.partnerOrgTransactionId,
        Ticket.createdAt = now,
        Ticket.updatedAt = now,
        Ticket.isTicketFree = Just isTicketFree
      }

mkTransitObjects :: Booking.FRFSTicketBooking -> Ticket.FRFSTicket -> Person.Person -> TC.ServiceAccount -> Text -> Flow TC.TransitObject
mkTransitObjects booking ticket person serviceAccount className = do
  toStation <- CQStation.findById booking.toStationId >>= fromMaybeM (StationDoesNotExist $ "StationId:" +|| booking.toStationId ||+ "")
  fromStation <- CQStation.findById booking.fromStationId >>= fromMaybeM (StationDoesNotExist $ "StationId:" +|| booking.fromStationId ||+ "")
  let tripType' = if booking._type == FQ.ReturnJourney then GWSA.ROUND_TRIP else GWSA.ONE_WAY
  let fromStaionNameLV = TC.LanguageValue {TC.language = "en-US", TC._value = fromStation.name}
  let toStaionNameLV = TC.LanguageValue {TC.language = "en-US", TC._value = toStation.name}
  let fromStationName = TC.Name {TC.defaultValue = fromStaionNameLV}
  let toStationName = TC.Name {TC.defaultValue = toStaionNameLV}
  let barcode' =
        TC.Barcode
          { TC._type = show GWSA.QR_CODE,
            TC.value = ticket.qrData
          }
  let passengerName' = fromMaybe "-" person.firstName
  let istTimeText = GWSA.showTimeIst ticket.validTill
  let textModuleTicketNumber = TC.TextModule {TC._header = "Ticket number", TC.body = ticket.ticketNumber, TC.id = "myfield1"}
  let textModuleValidUntil = TC.TextModule {TC._header = "Valid until", TC.body = istTimeText, TC.id = "myfield2"}
  let textModules = [textModuleTicketNumber, textModuleValidUntil]
  now <- getCurrentTime
  let nowText = GWSA.utcTimeToText now
  let validTillText = GWSA.utcTimeToText ticket.validTill
  let timeInterval = TC.TimeInterval {TC.start = TC.DateTime {TC.date = nowText}, TC.end = TC.DateTime {TC.date = validTillText}}
  return
    TC.TransitObject
      { TC.id = serviceAccount.saIssuerId <> "." <> ticket.id.getId,
        TC.classId = serviceAccount.saIssuerId <> "." <> className,
        TC.state = show GWSA.ACTIVE,
        TC.tripType = show tripType',
        TC.passengerType = show GWSA.SINGLE_PASSENGER,
        TC.passengerNames = passengerName',
        TC.ticketLeg =
          TC.TicketLeg
            { TC.originName = fromStationName,
              TC.destinationName = toStationName
            },
        TC.barcode = barcode',
        TC.textModulesData = textModules,
        TC.validTimeInterval = timeInterval
      }

createTickets :: Booking.FRFSTicketBooking -> [DTicket] -> Int -> Flow [Ticket.FRFSTicket]
createTickets booking dTickets discountedTickets = go dTickets discountedTickets []
  where
    go [] _ acc = return (Prelude.reverse acc)
    go (d : ds) freeTicketsLeft acc = do
      let isTicketFree = freeTicketsLeft > 0
      ticket <- mkTicket booking d isTicketFree
      let newFreeTickets = if isTicketFree then freeTicketsLeft - 1 else freeTicketsLeft
      go ds newFreeTickets (ticket : acc)

createTransitObjects :: Booking.FRFSTicketBooking -> [Ticket.FRFSTicket] -> Person.Person -> TC.ServiceAccount -> Text -> Flow [TC.TransitObject]
createTransitObjects booking tickets person serviceAccount className = go tickets []
  where
    go [] acc = return (Prelude.reverse acc)
    go (x : xs) acc = do
      transitObject <- mkTransitObjects booking x person serviceAccount className
      go xs (transitObject : acc)

buildRecon :: Recon.FRFSRecon -> Ticket.FRFSTicket -> Flow Recon.FRFSRecon
buildRecon recon ticket = do
  now <- getCurrentTime
  reconId <- generateGUID
  return
    recon
      { Recon.id = reconId,
        Recon.ticketNumber = ticket.ticketNumber,
        Recon.ticketStatus = Just ticket.status,
        Recon.createdAt = now,
        Recon.updatedAt = now
      }

mkGoogleWalletLink :: (MonadFlow m, HasFlowEnv m r '["googleSAPrivateKey" ::: String]) => TC.ServiceAccount -> [TC.TransitObject] -> m T.Text
mkGoogleWalletLink serviceAccount tObject = do
  let payload' =
        TC.Payload
          { TC.transitObjects = tObject
          }
  privateKey <- asks (.googleSAPrivateKey)
  let payloadValue = toJSON payload'
  let origins = ["www.example.com"] :: [String]
  let originsValue = toJSON origins
  let additionalClaims = TC.createAdditionalClaims [("payload", payloadValue), ("origins", originsValue), ("typ", String "savetowallet")]
  let jwtHeader =
        JOSEHeader
          { typ = Just "JWT",
            cty = Nothing,
            alg = Just RS256,
            kid = Nothing
          }
  let iss = stringOrURI . TC.saClientEmail $ serviceAccount
  let aud = Left <$> stringOrURI "google"
  iat <- numericDate <$> liftIO getPOSIXTime
  let claims =
        mempty
          { iat = iat,
            iss = iss,
            aud = aud,
            unregisteredClaims = additionalClaims
          }
  token' <- liftIO $ TC.createJWT' jwtHeader claims privateKey
  token <- fromEitherM (\err -> InternalError $ "Failed to get jwt token" <> show err) token'
  let textToken = snd token
  let url = "https://pay.google.com/gp/v/save/" <> textToken
  return url
