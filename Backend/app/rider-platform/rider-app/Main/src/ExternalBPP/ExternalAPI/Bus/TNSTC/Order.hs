module ExternalBPP.ExternalAPI.Bus.TNSTC.Order (createOrder) where

import qualified Data.Text as T
import Data.Time (Day, UTCTime (..))
import qualified Domain.Types.FRFSPassengerDetail as DFRFSPassengerDetail
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Person as DPerson
import qualified ExternalBPP.ExternalAPI.Bus.TNSTC.Booking as TNSTCBooking
import ExternalBPP.ExternalAPI.Bus.TNSTC.Place (tnstcPlaceCode)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Types (TnstcPickupPoint)
import qualified ExternalBPP.ExternalAPI.Bus.TNSTC.Types as TNSTCTypes
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSPassengerDetail as QFRFSPassengerDetail
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.RouteDetails as QRouteDetails

-- | ConfirmAdvSeatBooking. Reached only once the payment is SUCCESS -- SharedLogic.FRFSStatus
-- calls CallExternalBPP.confirm straight after marking the booking payment CHARGED -- so every
-- failure here is money taken without a ticket. The likeliest one is the seat hold expiring
-- ("Seat(s) selected earlier is already released"), since TNSTC holds for about seven minutes
-- and the rider spends that window in the payment gateway.
createOrder ::
  forall m r c.
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasShortDurationRetryCfg r c,
    HasField "requestId" r (Maybe Text)
  ) =>
  TNSTCConfig ->
  IntegratedBPPConfig ->
  FRFSTicketBooking ->
  [FRFSQuoteCategory] ->
  (Maybe Text, Maybe Text) ->
  m ProviderOrder
createOrder tnstcConfig integratedBPPConfig booking _quoteCategories (_mRiderName, mRiderNumber) = do
  quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (InvalidRequest $ "Quote not found: " <> booking.quoteId.getId)
  search <- QFRFSSearch.findById quote.searchId >>= fromMaybeM (InvalidRequest "Search not found for quote")
  journeyDate <- search.journeyDate & fromMaybeM (InvalidRequest "journeyDate missing on search")
  serviceId <- quote.providerServiceId & fromMaybeM (InvalidRequest "providerServiceId missing on quote")
  classId <- quote.providerClassId & fromMaybeM (InvalidRequest "providerClassId missing on quote")
  counterCode <- tnstcConfig.counterCode & fromMaybeM (InternalError "TNSTC counterCode not configured")
  createdBy <- tnstcConfig.createdBy & fromMaybeM (InternalError "TNSTC createdBy not configured")
  -- Already carries the "-<userId>" suffix: select stores it that way because TNSTC validates
  -- the full form here but never echoes the suffix back.
  wsRefNo <- quote.providerRefNo & fromMaybeM (InvalidRequest "providerRefNo missing on quote; select was not completed")
  concessionTypeId <- quote.concessionTypeId & fromMaybeM (InvalidRequest "concessionTypeId missing on quote")

  passengerDetails <- QFRFSPassengerDetail.findAllByQuoteId booking.quoteId
  -- Deliberately re-read rather than using the categories passed in: FRFSStatus hands us
  -- payment categories when they exist, and paymentCategoryToQuoteCategory drops
  -- providerBlockIds. Losing the hold ids here would fail the booking after payment.
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
  let selected = filter (\c -> c.selectedQuantity > 0) quoteCategories
      -- seatLabels and providerBlockIds were stored index-aligned per category at select, so
      -- zipping within a category preserves the pairing TNSTC matches positionally.
      pairs = concatMap (\c -> zip (fromMaybe [] c.seatLabels) (fromMaybe [] c.providerBlockIds)) selected
      seatLabels = map fst pairs
      blockIds = map snd pairs
      isMale p = p.gender == DPerson.MALE
      adultOrChildOf p = if p.isChild then "C" else "A"
      basicAmt = booking.totalPrice.amount - fromMaybe 0 quote.extraFees
      showAmt :: HighPrecMoney -> Text
      showAmt = T.pack . show . (realToFrac :: HighPrecMoney -> Double)
      genderOf p = case p.gender of DPerson.FEMALE -> "F"; _ -> "M"

  when (null pairs) $ throwError (InvalidRequest "No held seats on this booking; select was not completed")
  when (null passengerDetails) $
    throwError (InvalidRequest "No passenger details on this booking; select was not completed")

  orderedPax <- forM seatLabels $ \lbl ->
    find (\p -> p.seatLabel == lbl) passengerDetails
      & fromMaybeM (InternalError $ "No passenger row for held seat " <> lbl <> " on booking " <> booking.id.getId)

  wirePax <- forM orderedPax $ \p -> do
    name <- p.name & fromMaybeM (InternalError $ "Passenger name missing for seat " <> p.seatLabel <> " on booking " <> booking.id.getId)
    age <- p.age & fromMaybeM (InternalError $ "Passenger age missing for seat " <> p.seatLabel <> " on booking " <> booking.id.getId)
    return (p, name, show age)
  (leadPax, leadName, leadAge) <-
    listToMaybe wirePax & fromMaybeM (InternalError $ "No passengers on booking " <> booking.id.getId)

  phoneNumber <- mRiderNumber & fromMaybeM (InternalError $ "Rider phone number missing for booking " <> booking.id.getId)

  let countPax childWanted maleWanted =
        length [p | p <- orderedPax, p.isChild == childWanted, isMale p == maleWanted]
      adultMale = countPax False True
      adultFemale = countPax False False
      childMale = countPax True True
      childFemale = countPax True False

  tripCode <- quote.providerTripCode & fromMaybeM (InvalidRequest $ "providerTripCode missing on quote " <> quote.id.getId)
  startPlaceCode <- tnstcPlaceCode integratedBPPConfig (T.take 3 (T.drop 4 tripCode)) search.fromStationCode
  endPlaceCode <- tnstcPlaceCode integratedBPPConfig (T.take 3 (T.drop 7 tripCode)) search.toStationCode
  (mbPickup, mbDropOff) <- resolveBoardingPoints tnstcConfig quote passengerDetails journeyDate serviceId counterCode startPlaceCode endPlaceCode

  let describePoint label placeCode mbPoint =
        "TNSTC " <> label <> " unresolved bookingId=" <> booking.id.getId
          <> " placeId="
          <> show (listToMaybe (mapMaybe (.pickupPointPlaceId) passengerDetails))
          <> " placeCode="
          <> placeCode
          <> " resolved="
          <> show (mbPoint <&> (.tppName))
  pickupPoint <- case mbPickup of
    Just p -> return p
    Nothing -> do
      logError $ describePoint "pickup point" startPlaceCode mbPickup
      throwError (InternalError $ "TNSTC pickup point could not be resolved for booking " <> booking.id.getId)
  dropOffPoint <- case mbDropOff of
    Just p -> return p
    Nothing -> do
      logError $ describePoint "drop-off point" endPlaceCode mbDropOff
      throwError (InternalError $ "TNSTC drop-off point could not be resolved for booking " <> booking.id.getId)
  pickupTime <- pickupPoint.tppTime & fromMaybeM (InternalError $ "TNSTC pickup point " <> pickupPoint.tppPlaceId <> " has no departure time for this service")
  dropOffTime <- dropOffPoint.tppTime & fromMaybeM (InternalError $ "TNSTC drop-off point " <> dropOffPoint.tppPlaceId <> " has no arrival time for this service")

  idProofNumber <- mapM decrypt (listToMaybe (mapMaybe (.idProofNumber) passengerDetails)) :: m (Maybe Text)
  let idProofLookupId = listToMaybe (mapMaybe (.idProofLookupId) passengerDetails)
  idProof <- case (idProofLookupId, idProofNumber) of
    (Nothing, Nothing) -> return Nothing
    (Just lookupId, Just number) -> return (Just (lookupId, number))
    (Just _, Nothing) -> throwError (InternalError $ "ID proof type given without a number on booking " <> booking.id.getId)
    (Nothing, Just _) -> throwError (InternalError $ "ID proof number given without a type on booking " <> booking.id.getId)

  res <-
    TNSTCBooking.confirmAdvSeatBooking tnstcConfig $
      TNSTCBooking.ConfirmAdvSeatBookingReq
        { rqcAdultOrChild = adultOrChildOf leadPax,
          rqcAddnlAdultOrChilds = map adultOrChildOf orderedPax,
          rqcAdultMale = adultMale,
          rqcAdultFemale = adultFemale,
          rqcChildMale = childMale,
          rqcChildFemale = childFemale,
          rqcAge = leadAge,
          rqcGender = genderOf leadPax,
          rqcPassengerName = leadName,
          rqcAddnlAges = map (\(_, _, age) -> age) wirePax,
          rqcAddnlGenders = map genderOf orderedPax,
          rqcAddnlPassengerNames = map (\(_, name, _) -> name) wirePax,
          rqcEmailId = "",
          rqcPhoneNumber = phoneNumber,
          rqcBasicFare = showAmt basicAmt,
          rqcTotalFare = showAmt booking.totalPrice.amount,
          rqcClassId = classId,
          rqcConcessionTypeId = concessionTypeId,
          rqcCounterCode = counterCode,
          rqcCreatedBy = createdBy,
          rqcEndPlaceCode = endPlaceCode,
          rqcEndPlaceId = search.toStationCode,
          rqcJourneyDate = journeyDate,
          rqcPickupPointDropOffId = dropOffPoint.tppPlaceId,
          rqcPickupPointPlaceId = pickupPoint.tppPlaceId,
          rqcPickupPointTime = pickupTime,
          rqcPickupPointDropOffTime = dropOffTime,
          rqcSeatBlockIds = blockIds,
          rqcSeatNumbers = seatLabels,
          rqcServiceId = serviceId,
          rqcStartPlaceCode = startPlaceCode,
          rqcStartPlaceId = search.fromStationCode,
          rqcUserName = tnstcConfig.username,
          rqcWsRefNo = wsRefNo,
          rqcIdProofLookupId = maybe "" fst idProof,
          rqcIdProofNumber = maybe "" snd idProof
        }

  pnr <- res.tbkPnrNumber & fromMaybeM (InternalError "TNSTC confirmed without returning a PNR")
  logInfo $
    "TNSTC ConfirmAdvSeatBooking bookingId=" <> booking.id.getId <> " pnr=" <> pnr
      <> " pnrMasterID="
      <> show res.tbkPnrMasterId
      <> " seatIDs="
      <> show res.tbkSeatIds

  -- Boarding/alighting details are a display concern; the rider has already paid and been
  -- ticketed by this point, so a failure here must never surface as a booking failure.
  void $ try @_ @SomeException $ storeBoardingDetails search mbPickup mbDropOff

  let istOffset = 19800 :: NominalDiffTime
      mbArrival = do
        arrivalDate <- quote.arrivalDate
        arrivalTime <- quote.arrivalTime
        TNSTCTypes.parseTnstcTimestamp (T.strip arrivalDate <> " " <> T.strip arrivalTime)
      dayAfterJourneyIST = addUTCTime ((2 * 86400) - istOffset) (UTCTime journeyDate 0)
      qrValidTill = fromMaybe dayAfterJourneyIST mbArrival
      tickets =
        map
          ( \lbl ->
              ProviderTicket
                { ticketNumber = pnr <> "-" <> lbl,
                  vehicleNumber = Nothing,
                  description = Just ("Seat " <> lbl),
                  qrData = pnr,
                  qrStatus = "UNCLAIMED",
                  qrValidity = qrValidTill,
                  qrRefreshAt = Nothing,
                  commencingHours = Nothing
                }
          )
          seatLabels
  return ProviderOrder {orderId = pnr, tickets = tickets}

-- | Writes the rider's chosen pickup/drop-off point onto the journey leg and its route details.
-- No new columns: `fromStopPlatformCode` / `fromDepartureTime` / `toArrivalTime` already exist
-- there and mean exactly this. TNSTC gives a single time per point, so arrival and departure
-- at that point are the same instant -- which is what the GTFS path does too.
-- | Looks up the rider's chosen pickup and drop-off point in the cached point list.
resolveBoardingPoints ::
  ( MonadFlow m,
    CacheFlow m r,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasField "requestId" r (Maybe Text)
  ) =>
  TNSTCConfig ->
  DFRFSQuote.FRFSQuote ->
  [DFRFSPassengerDetail.FRFSPassengerDetail] ->
  Day ->
  Text ->
  Text ->
  Text ->
  Text ->
  m (Maybe TnstcPickupPoint, Maybe TnstcPickupPoint)
resolveBoardingPoints tnstcConfig _quote passengerDetails journeyDate serviceId counterCode startPlaceCode endPlaceCode = do
  let pointsAt placeCode =
        TNSTCBooking.getPickupPointsCached tnstcConfig _quote.integratedBppConfigId.getId $
          TNSTCBooking.GetPickupPointsReq
            { rqppCounterCode = counterCode,
              rqppJourneyDate = journeyDate,
              rqppServiceId = serviceId,
              rqppPlaceId = placeCode,
              rqppUserName = tnstcConfig.username
            }
      pick placeId = find (\p -> p.tppPlaceId == placeId)
      _unusedQuote = ()
  let mbPickupId = listToMaybe (mapMaybe (.pickupPointPlaceId) passengerDetails)
      mbDropOffId = listToMaybe (mapMaybe (.dropOffPointPlaceId) passengerDetails)
  mbPickup <- case mbPickupId of
    Just placeId -> pick placeId <$> pointsAt startPlaceCode
    Nothing -> return Nothing
  mbDropOff <- case mbDropOffId of
    Just placeId -> pick placeId <$> pointsAt endPlaceCode
    Nothing -> return Nothing
  return (mbPickup, mbDropOff)

-- | Writes the rider's chosen pickup/drop-off point onto the journey leg and its route details.
-- No new columns: `fromStopPlatformCode` / `fromDepartureTime` / `toArrivalTime` already exist
-- there and mean exactly this. TNSTC gives a single time per point, so arrival and departure
-- at that point are the same instant -- which is what the GTFS path does too.
-- | The boarding point's time already reaches the ticket via booking.startTime, which
-- buildJourneyAndLeg uses for the leg and route details. Only the platform is left: at leg
-- creation it is read off the waybill trip, which TNSTC has none of.
storeBoardingDetails ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DFRFSSearch.FRFSSearch ->
  Maybe TnstcPickupPoint ->
  Maybe TnstcPickupPoint ->
  m ()
storeBoardingDetails search mbPickup mbDropOff = do
  let pickupPlatform = mbPickup >>= (.tppPlatformNo)
      dropOffPlatform = mbDropOff >>= (.tppPlatformNo)
      legSearchId = Just search.id.getId
  whenJust pickupPlatform $ \platform -> do
    mbLeg <- QJourneyLeg.findByLegSearchId legSearchId
    whenJust mbLeg $ \leg ->
      QRouteDetails.updateBoardingPlatforms (Just platform) dropOffPlatform leg.id.getId
    logInfo $ "TNSTC boarding platform searchId=" <> search.id.getId <> " platform=" <> platform
