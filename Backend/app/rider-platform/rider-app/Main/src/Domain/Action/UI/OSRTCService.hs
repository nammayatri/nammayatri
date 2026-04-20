module Domain.Action.UI.OSRTCService where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import qualified Data.Aeson as A
import Data.List (sortBy)
import qualified Data.Text as T
import qualified Domain.Types.Extra.IntegratedBPPConfig as EIBC
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Cancel as OSRTCCancel
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Enums as OSRTCEnums
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Trip as OSRTCTrip
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Types as OSRTCTypes
import qualified ExternalBPP.Flow.Common as FlowCommon
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (EncKind (..), EncryptedField, decrypt, encrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as NandiFlow
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.MultiModal as MM

getOSRTCConfig :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DFRFSSearch.FRFSSearch -> m EIBC.OSRTCConfig
getOSRTCConfig search = do
  integratedBppConfig <- SIBC.findIntegratedBPPConfigById search.integratedBppConfigId
  case integratedBppConfig.providerConfig of
    DIBC.OSRTC cfg -> return cfg
    _ -> throwError $ InternalError "Expected OSRTC provider config"

getOSRTCConfigFromBooking :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DFRFSTicketBooking.FRFSTicketBooking -> m EIBC.OSRTCConfig
getOSRTCConfigFromBooking booking = do
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBppConfig.providerConfig of
    DIBC.OSRTC cfg -> return cfg
    _ -> throwError $ InternalError "Expected OSRTC provider config"

validateOwnership :: (MonadFlow m) => Maybe (Id DP.Person) -> Id DP.Person -> Text -> m ()
validateOwnership mbPersonId ownerId resourceDesc =
  forM_ mbPersonId $ \personId ->
    unless (personId == ownerId) $
      throwError $ InvalidRequest $ "You are not authorized to access this " <> resourceDesc

-- Seat availability: proxy call to OSRTC GetSeatAvailability
postFrfsOsrtcSeatAvailability ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  (Maybe (Id DP.Person), Id Merchant) ->
  Id DFRFSSearch.FRFSSearch ->
  FRFSTicketService.OSRTCSeatAvailabilityReq ->
  m FRFSTicketService.OSRTCSeatAvailabilityRes
postFrfsOsrtcSeatAvailability (mbPersonId, _merchantId) searchId req = do
  search <- B.runInReplica $ QFRFSSearch.findById searchId >>= fromMaybeM (InvalidRequest "Search not found")
  validateOwnership mbPersonId search.riderId "search"
  osrtcConfig <- getOSRTCConfig search
  let osrtcReq =
        OSRTCTypes.OSRTCSeatAvailabilityReq
          { intServiceTripDepartureID = req.serviceTripDepartureId,
            intFromStationID = req.fromStationId,
            intToStationID = req.toStationId
          }
  resp <- OSRTCTrip.getSeatAvailability osrtcConfig osrtcReq
  seatAvailRes <- resp._data & fromMaybeM (InternalError $ "OSRTC GetSeatAvailability failed: returnCode=" <> show resp.returnCode <> " returnMessage=" <> resp.returnMessage)
  return $
    FRFSTicketService.OSRTCSeatAvailabilityRes
      { totalCount = seatAvailRes.intTotalCount,
        availableCount = seatAvailRes.intAvailableCount,
        unavailableCount = seatAvailRes.intUnAvailableCount,
        singleAvailableSeats = seatAvailRes.intSingleAvailableSeats,
        serviceTripDepartureId = seatAvailRes.intServiceTripDepartureID,
        seats = A.toJSON seatAvailRes.lstSeats
      }

-- Update FRFSQuoteCategory.finalPrice to per-seat OSRTC total so Confirm V2 → Juspay createOrder uses toll-inclusive amount.
syncOSRTCFareToQuote ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DFRFSSearch.FRFSSearch ->
  Int ->
  Int ->
  Double ->
  m ()
syncOSRTCFareToQuote searchId serviceTripDepartureId numSeats totalFinalAmount = do
  when (numSeats > 0) $ do
    let perSeatAmount = HighPrecMoney $ toRational totalFinalAmount / toRational numSeats
        perSeatPrice = mkPrice (Just INR) perSeatAmount
        tripDepIdText = T.pack (show serviceTripDepartureId)
    quotes <- QFRFSQuote.findAllBySearchId searchId
    let matching = filter (\q -> q.bppItemId == tripDepIdText) quotes
    logInfo $ "syncOSRTCFareToQuote: searchId=" <> searchId.getId <> " trip=" <> tripDepIdText <> " numSeats=" <> show numSeats <> " totalFinalAmount=" <> show totalFinalAmount <> " perSeat=" <> show perSeatAmount <> " quotesFound=" <> show (length quotes) <> " matchingQuotes=" <> show (length matching)
    forM_ matching $ \quote -> do
      cats <- QFRFSQuoteCategory.findAllByQuoteId quote.id
      logInfo $ "syncOSRTCFareToQuote: quoteId=" <> quote.id.getId <> " catCount=" <> show (length cats)
      forM_ cats $ \cat -> do
        logInfo $ "syncOSRTCFareToQuote: updating cat=" <> cat.id.getId <> " oldOffered=" <> show cat.offeredPrice.amount <> " oldFinal=" <> show (cat.finalPrice <&> (.amount)) <> " -> newFinal=" <> show perSeatAmount
        QFRFSQuoteCategory.updateFinalPriceByQuoteCategoryId (Just perSeatPrice) cat.id

-- Fare calculation: calls OSRTC TicketFareCalculation and stores booking data in Redis
postFrfsOsrtcFareCalculation ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  (Maybe (Id DP.Person), Id Merchant) ->
  Id DFRFSSearch.FRFSSearch ->
  FRFSTicketService.OSRTCFareCalculationReq ->
  m FRFSTicketService.OSRTCFareCalculationRes
postFrfsOsrtcFareCalculation (mbPersonId, _merchantId) searchId req = do
  search <- B.runInReplica $ QFRFSSearch.findById searchId >>= fromMaybeM (InvalidRequest "Search not found")
  validateOwnership mbPersonId search.riderId "search"
  osrtcConfig <- getOSRTCConfig search
  when (length req.seats > 8) $
    throwError $ InvalidRequest "Maximum 8 seats allowed per booking"
  unless (length req.perSeatPassengers == length req.seats) $
    throwError $ InvalidRequest "perSeatPassengers must have the same number of entries as seats"
  let seatsBySrNo = sortBy (compare `on` (.srNo)) req.seats
      passengersBySrNo = sortBy (compare `on` (.srNo)) req.perSeatPassengers
  forM_ (zip seatsBySrNo passengersBySrNo) $ \(seat, pass) ->
    unless (seat.srNo == pass.srNo && seat.selectedSeatCode == pass.selectedSeatCode) $
      throwError $
        InvalidRequest $
          "Seat/passenger mismatch: seat srNo=" <> show seat.srNo <> " seatCode=" <> seat.selectedSeatCode
            <> " does not match passenger srNo="
            <> show pass.srNo
            <> " seatCode="
            <> pass.selectedSeatCode
  categoryWiseSeats <-
    mapM
      ( \seat -> do
          seatType <-
            OSRTCEnums.fromOSRTCId seat.seatTypeId
              & fromMaybeM (InvalidRequest $ "Invalid OSRTC seat type id: " <> show seat.seatTypeId)
          ticketCategory <-
            OSRTCEnums.fromOSRTCId seat.ticketCategoryId
              & fromMaybeM (InvalidRequest $ "Invalid OSRTC ticket category id: " <> show seat.ticketCategoryId)
          return
            OSRTCTypes.OSRTCCategoryWiseFareReq
              { intSrNo = seat.srNo,
                strSelectedSeatCode = seat.selectedSeatCode,
                strSelectedSeatNo = seat.selectedSeatNo,
                intSeatTypeID = seatType,
                intTicketCategoryID = ticketCategory
              }
      )
      req.seats
  let osrtcReq =
        OSRTCTypes.OSRTCFareCalcReq
          { intServiceTripDepartureID = req.serviceTripDepartureId,
            intFromStationID = req.fromStationId,
            intToStationID = req.toStationId,
            intPlatformID = osrtcConfig.platformId,
            strCategoryWiseJSON = categoryWiseSeats
          }
  resp <- OSRTCTrip.ticketFareCalculation osrtcConfig osrtcReq
  fareRes <- case resp._data of
    Just (r : _) -> return r
    _ -> throwError $ InternalError $ "OSRTC TicketFareCalculation failed: returnCode=" <> show resp.returnCode <> " returnMessage=" <> resp.returnMessage
  logInfo $ "OSRTC fareCalc lstSeatWiseFare: " <> encodeToText fareRes.lstSeatWiseFare -- diagnose: may carry seat ids InsertTicketBooking needs
  syncOSRTCFareToQuote searchId req.serviceTripDepartureId (length req.seats) fareRes.intTotalFinalAmount -- without this, Juspay createOrder uses base-fare from search quote and OSRTC rejects UpdateTicketBookingResponse with Amount Mismatch (toll/surcharges only known after fareCalc)
  let passengers = req.perSeatPassengers
  when (null passengers) $ throwError (InvalidRequest "perSeatPassengers cannot be empty")
  categoryWiseBookingSeats <-
    mapM
      ( \pass -> do
          seatTypeVal <-
            OSRTCEnums.fromOSRTCId pass.seatTypeId
              & fromMaybeM (InvalidRequest $ "Invalid OSRTC seat type id in passenger data: " <> show pass.seatTypeId)
          ticketCategoryVal <-
            OSRTCEnums.fromOSRTCId pass.ticketCategoryId
              & fromMaybeM (InvalidRequest $ "Invalid OSRTC ticket category id in passenger data: " <> show pass.ticketCategoryId)
          genderVal <-
            OSRTCEnums.fromOSRTCId pass.gender
              & fromMaybeM (InvalidRequest $ "Invalid OSRTC gender id: " <> show pass.gender)
          berthTypeVal <-
            OSRTCEnums.fromOSRTCId pass.berthTypeId
              & fromMaybeM (InvalidRequest $ "Invalid OSRTC berth type id: " <> show pass.berthTypeId)
          return
            OSRTCTypes.OSRTCCategoryWiseBookingReq
              { intSrNo = pass.srNo,
                strSelectedSeatCode = pass.selectedSeatCode,
                strSelectedSeatNo = pass.selectedSeatNo,
                intSeatTypeID = seatTypeVal,
                intBearthTypeID = berthTypeVal,
                intTicketCategoryID = ticketCategoryVal,
                strPassengerName = pass.passengerName,
                intAge = pass.age,
                strGender = genderVal,
                strMobileNo = pass.mobileNo
              }
      )
      passengers
  rider <- QPerson.findById search.riderId >>= fromMaybeM (PersonNotFound search.riderId.getId)
  when (isNothing rider.email) $
    throwError $ InvalidRequest "EMAIL_REQUIRED_FOR_OSRTC: Please add a verified email to your profile before booking an OSRTC ticket"
  -- Mobile is hard-required to match email's policy: OSRTC sends the booking confirmation SMS
  -- to the supplied number, so trusting client-side input would let a request redirect SMS
  -- to a number the authenticated rider does not own. Frontend handles the add-mobile UX
  -- when this code is returned.
  when (isNothing rider.mobileNumber) $
    throwError $ InvalidRequest "MOBILE_REQUIRED_FOR_OSRTC: Please add a verified mobile number to your profile before booking an OSRTC ticket"
  verifiedMobileNo <- rider.mobileNumber & fromMaybeM (InternalError "OSRTC: rider mobile number missing after isNothing check") >>= decrypt
  verifiedEmail <- rider.email & fromMaybeM (InternalError "OSRTC: rider email missing after isNothing check") >>= decrypt
  unless (verifiedMobileNo == req.mobileNo) $
    throwError $ InvalidRequest "MOBILE_MISMATCH: Mobile number does not match rider profile"
  let verifiedPassengerName = case rider.firstName of
        Just fn -> fn <> maybe "" (\ln -> " " <> ln) rider.lastName
        Nothing -> req.passengerName
  let bookingReq =
        OSRTCTypes.OSRTCInsertBookingReq
          { intServiceTripDepartureID = req.serviceTripDepartureId,
            intSourcePlaceID = req.fromStationId,
            intDestinationPlaceID = req.toStationId,
            strPassengerName = verifiedPassengerName,
            strEmail = verifiedEmail,
            strMobileNo = verifiedMobileNo,
            intTotalPaidAmount = fromRational $ toRational fareRes.intTotalFinalAmount,
            intPlatformID = osrtcConfig.platformId,
            intPaymentModeID = osrtcConfig.intPaymentModeId,
            strCategoryWiseJSON = categoryWiseBookingSeats
          }
  storeOSRTCBookingData searchId bookingReq
  forM_ search.journeyDate $ \jd ->
    Hedis.setExp (FlowCommon.osrtcJourneyDateKey searchId) jd FlowCommon.osrtcBookingDataTtl
  return $
    FRFSTicketService.OSRTCFareCalculationRes
      { totalFinalAmount = HighPrecMoney $ toRational fareRes.intTotalFinalAmount,
        totalBaseFare = HighPrecMoney $ toRational fareRes.intTotalBaseFare,
        totalDiscount = HighPrecMoney $ toRational $ fromMaybe 0 fareRes.intTotalDiscount,
        fareBreakdown = A.toJSON fareRes.lstFinalFare
      }

-- Store OSRTC booking preparation data in Redis (called before quote confirm).
-- The payload contains decrypted PII (rider email/mobile, passenger names, per-seat
-- mobile numbers); encrypt before placing in Redis so an attacker with read access
-- to the Redis instance cannot enumerate riders' contact details.
storeOSRTCBookingData ::
  (CacheFlow m r, MonadFlow m, EncFlow m r) =>
  Id DFRFSSearch.FRFSSearch ->
  OSRTCTypes.OSRTCInsertBookingReq ->
  m ()
storeOSRTCBookingData searchId bookingData = do
  encrypted :: EncryptedField 'AsEncrypted Text <- encrypt (encodeToText bookingData)
  Hedis.setExp (FlowCommon.osrtcBookingDataKey searchId) (encodeToText encrypted) FlowCommon.osrtcBookingDataTtl -- store as Text; Hedis's typed EncryptedField round-trip mangles quotes

-- Refund preview: calls OSRTC GetRefundAmount
postFrfsOsrtcRefundAmount ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  (Maybe (Id DP.Person), Id Merchant) ->
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  FRFSTicketService.OSRTCRefundPreviewReq ->
  m FRFSTicketService.OSRTCRefundPreviewRes
postFrfsOsrtcRefundAmount (mbPersonId, _merchantId) bookingId req = do
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Booking not found")
  validateOwnership mbPersonId booking.riderId "booking"
  osrtcConfig <- getOSRTCConfigFromBooking booking
  pnrNo <- booking.bppOrderId & fromMaybeM (InternalError "OSRTC PNR not found on booking")
  let osrtcReq =
        OSRTCTypes.OSRTCGetRefundReq
          { strSeatCodes = req.seatCodes,
            strPNRNo = pnrNo
          }
  resp <- OSRTCCancel.getRefundAmount osrtcConfig osrtcReq
  refundRes <- case resp._data of
    Just (r : _) -> return r
    _ -> throwError $ InternalError $ "OSRTC GetRefundAmount failed: returnCode=" <> show resp.returnCode <> " returnMessage=" <> resp.returnMessage
  return $
    FRFSTicketService.OSRTCRefundPreviewRes
      { refundAmount = HighPrecMoney $ toRational refundRes.intRefundAmount
      }

-- Cancel tickets: calls OSRTC InsertTicketCancel
postFrfsOsrtcCancelTicket ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  (Maybe (Id DP.Person), Id Merchant) ->
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  FRFSTicketService.OSRTCCancelTicketReq ->
  m FRFSTicketService.OSRTCCancelTicketRes
postFrfsOsrtcCancelTicket (mbPersonId, _merchantId) bookingId req = do
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Booking not found")
  validateOwnership mbPersonId booking.riderId "booking"
  case booking.status of
    DFRFSTicketBookingStatus.CANCEL_INITIATED ->
      throwError $ InvalidRequest "Cancellation already initiated for this booking"
    DFRFSTicketBookingStatus.CONFIRMED -> pure ()
    s -> throwError $ InvalidRequest $ "Cannot cancel booking in status: " <> show s
  osrtcConfig <- getOSRTCConfigFromBooking booking
  pnrNo <- booking.bppOrderId & fromMaybeM (InternalError "OSRTC PNR not found on booking")
  let osrtcReq =
        OSRTCTypes.OSRTCCancelReq
          { strPNRNo = pnrNo,
            strSeatCodes = req.seatCodes,
            strRefundRemarks = req.refundRemarks
          }
  resp <- OSRTCCancel.insertTicketCancel osrtcConfig osrtcReq
  cancelRes <- case resp._data of
    Just (r : _) -> return r
    _ -> throwError $ InternalError $ "OSRTC InsertTicketCancel failed: returnCode=" <> show resp.returnCode <> " returnMessage=" <> resp.returnMessage
  logInfo $ "OSRTC cancel success: PNR=" <> pnrNo <> " seats=" <> cancelRes.strSeatCodes
  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBookingStatus.CANCEL_INITIATED bookingId
  void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCEL_INITIATED) bookingId
  when (cancelRes.intRefundAmount > 0) $ do
    let refundAmt = Just $ HighPrecMoney $ toRational cancelRes.intRefundAmount
    -- Pass through existing cancellationCharges and isBookingCancellable so we don't blank
    -- values that may have been set by an earlier canCancel precheck or admin tooling.
    void $ QFRFSTicketBooking.updateRefundCancellationChargesAndIsCancellableByBookingId refundAmt booking.cancellationCharges booking.isBookingCancellable bookingId
  return $
    FRFSTicketService.OSRTCCancelTicketRes
      { pnrNo = cancelRes.strPNRNo,
        refundAmount = HighPrecMoney $ toRational cancelRes.intRefundAmount,
        seatCodes = cancelRes.strSeatCodes
      }

-- Stations list: reads OSRTC stations from Nandi GTFS feed, maps providerCode -> intStationId
getFrfsOsrtcStations ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r, MonadFlow m, HasRequestId r, HasShortDurationRetryCfg r c, MonadReader r m) =>
  (Maybe (Id DP.Person), Id Merchant) ->
  Maybe DIBC.PlatformType ->
  Context.City ->
  Spec.VehicleCategory ->
  m [FRFSTicketService.OSRTCStationRes]
getFrfsOsrtcStations (_mbPersonId, merchantId) mbPlatformType city vehicleType = do
  let platformType = fromMaybe DIBC.PARTNERORG mbPlatformType
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId-" <> merchantId.getId <> "-city-" <> show city)
  integratedBppConfig <- SIBC.findIntegratedBPPConfig Nothing merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) platformType
  case integratedBppConfig.providerConfig of
    DIBC.OSRTC _ -> pure ()
    _ -> throwError $ InvalidRequest "Resolved integrated BPP config is not OSRTC for the given merchant/city/vehicleType/platformType"
  baseUrl <- MM.getOTPRestServiceReq merchantId merchantOpCity.id
  rawStations <- NandiFlow.getStationsByGtfsId baseUrl integratedBppConfig.feedKey
  return $
    mapMaybe
      ( \st ->
          case readMaybe (T.unpack st.providerCode) of
            Just intStationId ->
              Just
                FRFSTicketService.OSRTCStationRes
                  { intStationId = intStationId,
                    stationCode = st.stopCode,
                    stationName = st.stopName
                  }
            Nothing -> Nothing
      )
      rawStations

-- Live tracking: proxy OSRTC GetTrackingData (PNR from bppOrderId after confirm)
getFrfsOsrtcTracking ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  (Maybe (Id DP.Person), Id Merchant) ->
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  m FRFSTicketService.OSRTCTrackingRes
getFrfsOsrtcTracking (mbPersonId, _merchantId) bookingId = do
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Booking not found")
  validateOwnership mbPersonId booking.riderId "booking"
  unless (booking.status `elem` [DFRFSTicketBookingStatus.CONFIRMED, DFRFSTicketBookingStatus.CONFIRMING]) $
    throwError $
      InvalidRequest $
        "Tracking is only available for confirmed bookings (status must be CONFIRMED or CONFIRMING); current: " <> show booking.status
  osrtcConfig <- getOSRTCConfigFromBooking booking
  pnrNo <- booking.bppOrderId & fromMaybeM (InternalError "OSRTC PNR not found on booking")
  resp <- OSRTCTrip.getTrackingData osrtcConfig OSRTCTypes.OSRTCGetTrackingReq {strPNRNo = pnrNo}
  trackingPayload <- resp._data & fromMaybeM (InternalError $ "OSRTC GetTrackingData failed: returnCode=" <> show resp.returnCode <> " returnMessage=" <> resp.returnMessage)
  case trackingPayload of
    A.Null -> throwError $ InternalError "OSRTC GetTrackingData returned empty payload"
    A.Array trackingItems | null trackingItems -> throwError $ InternalError "OSRTC GetTrackingData returned empty payload"
    _ -> return FRFSTicketService.OSRTCTrackingRes {tracking = trackingPayload}
