module ExternalBPP.ExternalAPI.Bus.TNSTC.Order (createOrder) where

import qualified Data.Text as T
import Data.Time (Day)
import qualified Domain.Types.FRFSPassengerDetail as DFRFSPassengerDetail
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Person as DPerson
import qualified ExternalBPP.ExternalAPI.Bus.TNSTC.Booking as TNSTCBooking
import ExternalBPP.ExternalAPI.Bus.TNSTC.Place (tnstcPlaceCode)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Types (TnstcPickupPoint)
import ExternalBPP.ExternalAPI.Types
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

  passengerDetails <- QFRFSPassengerDetail.findAllByBookingId (Just booking.id)
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
      isChildCat c = c.category `elem` [CHILD, CHILD_SLEEPER]
      isMale p = p.gender == DPerson.MALE
      countPax childWanted maleWanted =
        length [p | p <- passengerDetails, p.isChild == childWanted, isMale p == maleWanted]
      -- Fall back to the category split (all male) only when no passenger rows were captured.
      havePax = not (null passengerDetails)
      adultMale = if havePax then countPax False True else sum [c.selectedQuantity | c <- selected, not (isChildCat c)]
      adultFemale = if havePax then countPax False False else 0
      childMale = if havePax then countPax True True else sum [c.selectedQuantity | c <- selected, isChildCat c]
      childFemale = if havePax then countPax True False else 0
      adultOrChildOf p = if p.isChild then "C" else "A"
      basicAmt = booking.totalPrice.amount - fromMaybe 0 quote.extraFees
      showAmt :: HighPrecMoney -> Text
      showAmt = T.pack . show . (realToFrac :: HighPrecMoney -> Double)
      -- The confirm schema has exactly one additional-passenger slot (addnlPasngrName /
      -- addnlAge / addnlGender, all singular), so only the first two riders can be named.
      -- Everyone else travels on the seat numbers alone, which TNSTC accepts.
      -- Ordered to match the seatNumber list we emit, since TNSTC pairs them positionally.
      orderedPax = mapMaybe (\lbl -> find (\p -> p.seatLabel == lbl) passengerDetails) seatLabels
      genderOf p = case p.gender of DPerson.FEMALE -> "F"; _ -> "M"
      -- TNSTC parses addnlAge unconditionally, even for a single passenger, so it must always
      -- be a number.
      ageOf p = maybe "30" show p.age

  when (null pairs) $ throwError (InvalidRequest "No held seats on this booking; select was not completed")

  -- TNSTC writes these two straight into PNRMASTER.PNR_PICKUPPOINTPICKTIME / DROPOFPOINTTIME.
  -- Omit them and it composes the literal 'null:00' and dies on a data-truncation error --
  -- after the seats are held and the rider has paid. Resolved from the cached point list, so
  -- normally a cache hit rather than another vendor round trip.
  let tripCode = fromMaybe "" quote.providerTripCode
  startPlaceCode <- tnstcPlaceCode integratedBPPConfig (T.take 3 (T.drop 4 tripCode)) search.fromStationCode
  endPlaceCode <- tnstcPlaceCode integratedBPPConfig (T.take 3 (T.drop 7 tripCode)) search.toStationCode
  (mbPickup, mbDropOff) <- resolveBoardingPoints tnstcConfig quote passengerDetails journeyDate serviceId counterCode startPlaceCode endPlaceCode

  -- A stored point id that no longer resolves would send an empty pickupPointTime and TNSTC
  -- would fail on PNR_PICKUPPOINTPICKTIME -- after the rider has paid, with a vendor SQL error
  -- that says nothing about the cause. Fail here instead, naming the id, so the reason is in
  -- the booking's failureReason rather than buried in a JDBC message.
  whenJust (listToMaybe (mapMaybe (.pickupPointPlaceId) passengerDetails)) $ \placeId ->
    when (isNothing (mbPickup >>= (.tppTime))) $ do
      logError $
        "TNSTC pickup point unresolved bookingId=" <> booking.id.getId
          <> " placeId="
          <> placeId
          <> " placeCode="
          <> startPlaceCode
          <> " resolved="
          <> show (mbPickup <&> (.tppName))
      throwError (InternalError $ "TNSTC pickup point " <> placeId <> " has no departure time for this service")
  whenJust (listToMaybe (mapMaybe (.dropOffPointPlaceId) passengerDetails)) $ \placeId ->
    when (isNothing (mbDropOff >>= (.tppTime))) $ do
      logError $
        "TNSTC drop-off point unresolved bookingId=" <> booking.id.getId
          <> " placeId="
          <> placeId
          <> " placeCode="
          <> endPlaceCode
      throwError (InternalError $ "TNSTC drop-off point " <> placeId <> " has no arrival time for this service")
  when (length passengerDetails /= length pairs) $
    logWarning $
      "TNSTC confirm bookingId=" <> booking.id.getId <> " has " <> show (length passengerDetails)
        <> " passenger rows for "
        <> show (length pairs)
        <> " held seats"

  res <-
    TNSTCBooking.confirmAdvSeatBooking tnstcConfig $
      TNSTCBooking.ConfirmAdvSeatBookingReq
        { rqcAdultOrChild = maybe "A" adultOrChildOf (listToMaybe orderedPax),
          rqcAddnlAdultOrChilds = map adultOrChildOf orderedPax,
          rqcAdultMale = adultMale,
          rqcAdultFemale = adultFemale,
          rqcChildMale = childMale,
          rqcChildFemale = childFemale,
          rqcAge = maybe "30" ageOf (listToMaybe orderedPax),
          rqcGender = maybe "M" genderOf (listToMaybe orderedPax),
          rqcPassengerName = fromMaybe "" (listToMaybe orderedPax >>= (.name)),
          rqcAddnlAges = map ageOf orderedPax,
          rqcAddnlGenders = map genderOf orderedPax,
          rqcAddnlPassengerNames = map (fromMaybe "" . (.name)) orderedPax,
          rqcEmailId = "",
          rqcPhoneNumber = fromMaybe "" mRiderNumber,
          rqcBasicFare = showAmt basicAmt,
          rqcTotalFare = showAmt booking.totalPrice.amount,
          rqcClassId = classId,
          rqcConcessionTypeId = concessionTypeId,
          rqcCounterCode = counterCode,
          rqcCreatedBy = createdBy,
          rqcEndPlaceCode = endPlaceCode,
          rqcEndPlaceId = search.toStationCode,
          rqcJourneyDate = journeyDate,
          rqcPickupPointDropOffId = fromMaybe "" (mbDropOff <&> (.tppPlaceId)),
          rqcPickupPointPlaceId = fromMaybe "" (mbPickup <&> (.tppPlaceId)),
          rqcPickupPointTime = fromMaybe "" (mbPickup >>= (.tppTime)),
          rqcPickupPointDropOffTime = fromMaybe "" (mbDropOff >>= (.tppTime)),
          rqcSeatBlockIds = blockIds,
          rqcSeatNumbers = seatLabels,
          rqcServiceId = serviceId,
          rqcStartPlaceCode = startPlaceCode,
          rqcStartPlaceId = search.fromStationCode,
          rqcUserName = tnstcConfig.username,
          rqcWsRefNo = wsRefNo
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

  now <- getCurrentTime
  -- TNSTC issues no QR; the PNR is the travel document, so it is what each seat's ticket
  -- carries. Validity runs to the end of the journey day.
  let qrValidTill = addUTCTime (2 * 86400) now
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
      pick placeId points = find (\p -> p.tppPlaceId == placeId) points
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

