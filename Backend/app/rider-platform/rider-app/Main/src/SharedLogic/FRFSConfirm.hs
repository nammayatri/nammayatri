module SharedLogic.FRFSConfirm where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Monad.Extra hiding (fromMaybeM)
import Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty hiding (groupBy, map, nub, nubBy)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import qualified Domain.Types.RouteDetails as DRD
import qualified Domain.Types.Trip as DTrip
import EulerHS.Prelude hiding (all, and, any, concatMap, elem, find, foldr, forM_, fromList, groupBy, hoistMaybe, id, length, map, mapM_, maximum, null, readMaybe, toList, whenJust)
import qualified ExternalBPP.CallAPI.Init as CallExternalBPP
import qualified ExternalBPP.CallAPI.Types as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.Interface.Types
import Kernel.External.MultiModal.Interface.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude hiding (whenJust)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common hiding (mkPrice)
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.FRFSSeatBooking as SeatBooking
import SharedLogic.FRFSStatus
import SharedLogic.FRFSUtils
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Seat as QSeat
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Location as QLocation
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RouteDetails as QRouteDetails
import Tools.Error
import Tools.Maps as Maps
import Tools.Metrics.BAPMetrics (HasBAPMetrics)

data HoldContext = HoldContext
  { hcHoldId :: Text,
    hcFromIdx :: Int,
    hcToIdx :: Int
  }

confirmAndUpsertBooking :: (CallExternalBPP.FRFSConfirmFlow m r c) => Id Domain.Types.Person.Person -> DFRFSQuote.FRFSQuote -> [API.Types.UI.FRFSTicketService.FRFSCategorySelectionReq] -> Maybe CrisSdkResponse -> Maybe Bool -> Maybe Bool -> DIBC.IntegratedBPPConfig -> Maybe Text -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking, FRFSUtils.FRFSFareParameters, [FRFSQuoteCategory.FRFSQuoteCategory], Bool)
confirmAndUpsertBooking personId quote selectedQuoteCategories crisSdkResponse isSingleMode mbIsMockPayment integratedBppConfig mbTripId = do
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
  mbBooking <- QFRFSTicketBooking.findBySearchId quote.searchId
  isMultiInitAllowed <-
    case mbBooking of
      Just booking -> do
        case integratedBppConfig.providerConfig of
          DIBC.ONDC DIBC.ONDCBecknConfig {multiInitAllowed} ->
            return $
              multiInitAllowed == Just True
                && booking.status `elem` [DFRFSTicketBooking.NEW, DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
          _ -> return $ booking.status `elem` [DFRFSTicketBooking.NEW, DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
      Nothing -> return True

  let allSeatIds = nub $ concatMap (\categoryReq -> fromMaybe [] categoryReq.seatIds) selectedQuoteCategories
      firstTripId = mbTripId

  mbHoldCtxForAll <-
    if not (null allSeatIds)
      then case firstTripId of
        Just tripId -> do
          logInfo $ "FRFSConfirm:confirmAndUpsertBooking seatHold flow personId=" <> personId.getId <> " tripId=" <> tripId <> " seatCount=" <> show (length allSeatIds)
          let routeStations :: Maybe [FRFSTicketService.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
              mbRouteCode = listToMaybe (fromMaybe [] routeStations) <&> (.code)
          case mbRouteCode of
            Nothing -> do
              logWarning $ "FRFSConfirm:confirmAndUpsertBooking skipping hold, routeCode not found for quoteId=" <> quote.id.getId
              pure Nothing
            Just routeCode -> do
              mIndices <- JourneyUtils.getRouteStopIndices routeCode quote.fromStationCode quote.toStationCode integratedBppConfig
              case mIndices of
                Just (fromIdx, toIdx) -> do
                  holdId <- generateGUID
                  let ttl' = 300
                  success <- SeatBooking.holdSeats tripId allSeatIds fromIdx toIdx holdId ttl'
                  unless success $
                    throwError (InvalidRequest "Selected seat is no longer available.")
                  pure $ Just (holdId, fromIdx, toIdx)
                Nothing -> do
                  logWarning $ "FRFSConfirm:confirmAndUpsertBooking skipping hold, stop indices not found for routeCode=" <> routeCode <> " from=" <> quote.fromStationCode <> " to=" <> quote.toStationCode
                  pure Nothing
        _ -> pure Nothing
      else pure Nothing

  quoteCategorySelections <-
    if isMultiInitAllowed
      then mapM processCategorySelection selectedQuoteCategories
      else return $ quoteCategories <&> (\qc -> FRFSUtils.QuoteCategorySelection qc.id qc.selectedQuantity Nothing Nothing)

  updatedQuoteCategories <-
    if isMultiInitAllowed
      then FRFSUtils.updateQuoteCategoriesWithSelections quoteCategorySelections quoteCategories
      else return quoteCategories

  let fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories updatedQuoteCategories)
  (rider, dConfirmRes) <- confirm isMultiInitAllowed fareParameters mbBooking mbHoldCtxForAll firstTripId

  whenJust mbHoldCtxForAll $ \(holdId, _, _) -> do
    logInfo $ "FRFSConfirm:confirmAndUpsertBooking tracking hold bookingId=" <> dConfirmRes.id.getId <> " holdId=" <> holdId
    SeatBooking.trackHoldForBooking dConfirmRes.id.getId holdId

  return (rider, dConfirmRes, fareParameters, updatedQuoteCategories, isMultiInitAllowed)
  where
    processCategorySelection :: CallExternalBPP.FRFSConfirmFlow m r c => FRFSCategorySelectionReq -> m FRFSUtils.QuoteCategorySelection
    processCategorySelection categoryReq = do
      mbLabels <- case categoryReq.seatIds of
        Just categorySeatIds | not (null categorySeatIds) -> do
          seats <- mapM QSeat.findById categorySeatIds
          let sLabels = map (.seatLabel) (catMaybes seats)
          pure $ if null sLabels then Nothing else Just sLabels
        _ -> pure Nothing

      return $
        FRFSUtils.QuoteCategorySelection
          { qcQuoteCategoryId = categoryReq.quoteCategoryId,
            qcQuantity = categoryReq.quantity,
            qcSeatIds = categoryReq.seatIds,
            qcSeatLabels = mbLabels
          }

    confirm :: CallExternalBPP.FRFSConfirmFlow m r c => Bool -> FRFSUtils.FRFSFareParameters -> Maybe DFRFSTicketBooking.FRFSTicketBooking -> Maybe (Text, Int, Int) -> Maybe Text -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    confirm isMultiInitAllowed fareParameters mbBooking mbHoldCtxForAll firstTripId = do
      rider <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ FRFSQuoteExpired quote.id.getId
      unless (personId == quote.riderId) $ throwError AccessDenied
      maybeM
        (buildAndCreateBooking rider quote fareParameters mbIsMockPayment mbHoldCtxForAll firstTripId)
        ( \booking -> do
            updatedBooking <-
              if isMultiInitAllowed
                then do
                  let mBookAuthCode = crisSdkResponse <&> (.bookAuthCode)
                      totalPrice = fareParameters.totalPrice
                  void $ QFRFSTicketBooking.updateBookingAuthCodeById mBookAuthCode booking.id
                  void $ QFRFSTicketBooking.updateQuoteAndBppItemIdAndRouteStationsJson quote.id quote.bppItemId quote.routeStationsJson booking.id
                  void $ QFRFSTicketBooking.updateIsFareChangedById Nothing booking.id
                  return $ booking {DFRFSTicketBooking.quoteId = quote.id, DFRFSTicketBooking.bppItemId = quote.bppItemId, DFRFSTicketBooking.bookingAuthCode = mBookAuthCode, DFRFSTicketBooking.totalPrice = totalPrice}
                else return booking
            pure (rider, updatedBooking)
        )
        (pure mbBooking)

    buildAndCreateBooking :: CallExternalBPP.FRFSConfirmFlow m r c => Domain.Types.Person.Person -> DFRFSQuote.FRFSQuote -> FRFSUtils.FRFSFareParameters -> Maybe Bool -> Maybe (Text, Int, Int) -> Maybe Text -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    buildAndCreateBooking rider quote'@DFRFSQuote.FRFSQuote {..} fareParameters mbMockPayment mbHoldCtxForAll firstTripId = do
      uuid <- generateGUID
      now <- getCurrentTime
      mbSearch <- QFRFSSearch.findById searchId

      let isFareChanged = if isJust partnerOrgId then isJust oldCacheDump else False

      let booking =
            DFRFSTicketBooking.FRFSTicketBooking
              { id = uuid,
                bppOrderId = Nothing,
                quoteId = id,
                status = DFRFSTicketBooking.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = quote'.merchantId,
                totalPrice = fareParameters.totalPrice,
                frfsTicketBookingPaymentIdForTicketGeneration = Nothing,
                paymentTxnId = Nothing,
                bppBankAccountNumber = Nothing,
                bppBankCode = Nothing,
                cancellationCharges = Nothing,
                refundAmount = Nothing,
                isBookingCancellable = Nothing,
                customerCancelled = False,
                payerVpa = Nothing,
                cashbackPayoutOrderId = Nothing,
                cashbackStatus = if isJust quote.discountedTickets then Just DFTB.PENDING else Nothing,
                bppDelayedInterest = quote.bppDelayedInterest,
                journeyOnInitDone = Nothing,
                startTime = Just now, -- TODO
                isFareChanged = Just isFareChanged,
                integratedBppConfigId = quote.integratedBppConfigId,
                googleWalletJWTUrl = Nothing,
                bookingAuthCode = crisSdkResponse <&> (.bookAuthCode),
                osType = crisSdkResponse <&> (.osType),
                osBuildVersion = crisSdkResponse <&> (.osBuildVersion),
                recentLocationId = mbSearch >>= (.recentLocationId),
                failureReason = Nothing,
                isSingleMode = isSingleMode,
                isMockPayment = mbMockPayment,
                ondcOnInitReceived = Nothing,
                ondcOnInitReceivedAt = Nothing,
                holdId = mbHoldCtxForAll <&> (\(h, _, _) -> h),
                tripId = firstTripId,
                fromStopIdx = mbHoldCtxForAll <&> (\(_, f, _) -> f),
                toStopIdx = mbHoldCtxForAll <&> (\(_, _, t) -> t),
                ..
              }
      QFRFSTicketBooking.create booking

      -- Update userBookedRouteShortName and userBookedBusServiceTierType from route_stations_json
      let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< routeStationsJson
      let mbFirstRouteStation = listToMaybe (fromMaybe [] routeStations)
      let mbBookedRouteShortName = mbFirstRouteStation <&> (.shortName)
      let mbBookedServiceTierType = mbFirstRouteStation >>= (.vehicleServiceTier) <&> (._type)
      when (isJust mbBookedRouteShortName && isJust mbBookedServiceTierType) $ do
        mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just searchId.getId)
        whenJust mbJourneyLeg $ \journeyLeg -> do
          whenJust mbBookedRouteShortName $ \bookedRouteShortName ->
            QRouteDetails.updateUserBookedRouteShortName (Just bookedRouteShortName) journeyLeg.id.getId
          QJourneyLeg.updateByPrimaryKey $ journeyLeg {DJL.userBookedBusServiceTierType = mbBookedServiceTierType}

      return (rider, booking)

postFrfsQuoteV2ConfirmUtil :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> DFRFSQuote.FRFSQuote -> [API.Types.UI.FRFSTicketService.FRFSCategorySelectionReq] -> Maybe CrisSdkResponse -> Maybe Bool -> Maybe Bool -> Maybe Bool -> DIBC.IntegratedBPPConfig -> Maybe Text -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2ConfirmUtil (mbPersonId, merchantId_) quote selectedQuoteCategories crisSdkResponse isSingleMode mbEnableOffer mbIsMockPayment integratedBppConfig mbTripId = do
  when (null selectedQuoteCategories) $ throwError $ NoSelectedCategoryFound quote.id.getId
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  (rider, dConfirmRes, fareParameters, updatedQuoteCategories, isMultiInitAllowed) <- confirmAndUpsertBooking personId quote selectedQuoteCategories crisSdkResponse isSingleMode mbIsMockPayment integratedBppConfig mbTripId
  (mbJourneyId, _) <- getAllJourneyFrfsBookings dConfirmRes
  when (isNothing mbJourneyId) $
    fork "FRFS buildJourneyAndLeg" $ buildJourneyAndLeg dConfirmRes fareParameters
  merchantOperatingCity <- getMerchantOperatingCityFromBooking dConfirmRes
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< dConfirmRes.routeStationsJson
  now <- getCurrentTime
  when isMultiInitAllowed $ do
    bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
    let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
    mRiderNumber <- mapM decrypt rider.mobileNumber
    -- Add default TTL of 30 seconds or the value provided in the config
    let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.initTTLSec) now
    void $ QFRFSTicketBooking.updateValidTillById validTill dConfirmRes.id
    let dConfirmRes' = dConfirmRes {DFRFSTicketBooking.validTill = validTill}
    when (dConfirmRes.status /= DFRFSTicketBooking.NEW) $ do
      void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.NEW dConfirmRes.id
    CallExternalBPP.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) dConfirmRes' updatedQuoteCategories mbEnableOffer
  frfsBookingStatus (dConfirmRes.riderId, merchantId_) (integratedBppConfig.platformType == DIBC.MULTIMODAL) (withPaymentStatusResponseHandler dConfirmRes updatedQuoteCategories fareParameters routeStations stations merchantOperatingCity) dConfirmRes rider (\_ _ -> pure ())
  where
    withPaymentStatusResponseHandler ::
      CallExternalBPP.FRFSConfirmFlow m r c =>
      DFRFSTicketBooking.FRFSTicketBooking ->
      [FRFSQuoteCategory.FRFSQuoteCategory] ->
      FRFSFareParameters ->
      Maybe [FRFSTicketService.FRFSRouteStationsAPI] ->
      [FRFSTicketService.FRFSStationAPI] ->
      DMOC.MerchantOperatingCity ->
      ((DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, Maybe DPayment.PaymentStatusResp) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) ->
      m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
    withPaymentStatusResponseHandler booking quoteCategories fareParameters routeStations stations merchantOperatingCity action = do
      mbPaymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findTicketBookingPayment booking
      mbPaymentOrder <- maybe (pure Nothing) (QPaymentOrder.findById . (.paymentOrderId)) mbPaymentBooking
      case (mbPaymentBooking, mbPaymentOrder) of
        (Just paymentBooking, Just paymentOrder) -> do
          action (paymentBooking, paymentOrder, Nothing)
        _ -> return $ makeBookingStatusAPI (booking, quoteCategories) fareParameters routeStations stations merchantOperatingCity.city

    makeBookingStatusAPI (booking, quoteCategories) fareParameters routeStations stations city = do
      FRFSTicketService.FRFSTicketBookingStatusAPIRes
        { bookingId = booking.id,
          city,
          updatedAt = booking.updatedAt,
          createdAt = booking.createdAt,
          _type = booking._type,
          quoteCategories = map mkFRFSQuoteCategoryAPIEntity quoteCategories,
          price = Just booking.totalPrice.amount,
          priceWithCurrency = Just $ mkPriceAPIEntity booking.totalPrice,
          quantity = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity),
          validTill = booking.validTill,
          vehicleType = booking.vehicleType,
          status = booking.status,
          payment = Nothing,
          tickets = [],
          discountedTickets = booking.discountedTickets,
          eventDiscountAmount = booking.eventDiscountAmount,
          isFareChanged = booking.isFareChanged,
          googleWalletJWTUrl = booking.googleWalletJWTUrl,
          integratedBppConfigId = booking.integratedBppConfigId,
          ..
        }

buildJourneyAndLeg ::
  ( HasBAPMetrics m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool,
    HasShortDurationRetryCfg r c
  ) =>
  DFTB.FRFSTicketBooking ->
  FRFSFareParameters ->
  m ()
buildJourneyAndLeg booking fareParameters = do
  Hedis.whenWithLockRedis mkBookingJourneyCreateKey 60 $ do
    integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking

    now <- getCurrentTime
    journeyGuid <- generateGUID
    journeyLegGuid <- generateGUID

    distanceAndDuration <- getDistanceAndDuration booking.fromStationPoint booking.toStationPoint
    let distance = fromMaybe (Distance 0 Meter) (fst <$> distanceAndDuration)
        duration = snd <$> distanceAndDuration

    fromLocationId <- generateGUID
    let fromLocation =
          DL.Location
            { id = fromLocationId,
              createdAt = now,
              updatedAt = now,
              lat = maybe 0.0 (.lat) booking.fromStationPoint,
              lon = maybe 0.0 (.lon) booking.fromStationPoint,
              address =
                DLA.LocationAddress
                  { street = Nothing,
                    door = Nothing,
                    city = Nothing,
                    state = Nothing,
                    country = Nothing,
                    building = Nothing,
                    areaCode = Nothing,
                    area = booking.fromStationAddress,
                    ward = Nothing,
                    placeId = Nothing,
                    instructions = Nothing,
                    title = Nothing,
                    extras = Nothing
                  },
              merchantId = Just booking.merchantId,
              merchantOperatingCityId = Just booking.merchantOperatingCityId
            }

    toLocationId <- generateGUID
    let toLocation =
          DL.Location
            { id = toLocationId,
              createdAt = now,
              updatedAt = now,
              lat = maybe 0.0 (.lat) booking.toStationPoint,
              lon = maybe 0.0 (.lon) booking.toStationPoint,
              address =
                DLA.LocationAddress
                  { street = Nothing,
                    door = Nothing,
                    city = Nothing,
                    state = Nothing,
                    country = Nothing,
                    building = Nothing,
                    areaCode = Nothing,
                    area = booking.toStationAddress,
                    ward = Nothing,
                    placeId = Nothing,
                    instructions = Nothing,
                    title = Nothing,
                    extras = Nothing
                  },
              merchantId = Just booking.merchantId,
              merchantOperatingCityId = Just booking.merchantOperatingCityId
            }

    let journey =
          DJ.Journey
            { id = journeyGuid,
              convenienceCost = 0,
              estimatedDistance = distance,
              estimatedDuration = duration,
              isPaymentSuccess = Nothing,
              totalLegs = 1,
              modes = [mapVehicleCategoryToTripMode booking.vehicleType],
              searchRequestId = booking.searchId.getId, -- Note :: This is not SearchRequest Table's ID. Do not use it to Query SearchReqeust Anywhere in Application.
              merchantId = booking.merchantId,
              status = DJ.CONFIRMED,
              riderId = booking.riderId,
              startTime = Just booking.createdAt,
              endTime = Nothing,
              merchantOperatingCityId = booking.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now,
              recentLocationId = booking.recentLocationId,
              isPublicTransportIncluded = Just True,
              isSingleMode = Just True,
              relevanceScore = Nothing,
              hasPreferredServiceTier = Nothing,
              hasPreferredTransitModes = Just False,
              fromLocation = fromLocation,
              toLocation = Just toLocation,
              paymentOrderShortId = Nothing,
              journeyExpiryTime = Nothing,
              hasStartedTrackingWithoutBooking = Nothing
            }

    journeyRouteDetailsId <- generateGUID
    let estimatedPrice = find (\priceItem -> priceItem.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice)

    let mbRouteStations :: Maybe [FRFSTicketService.FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
        mbRouteStation = listToMaybe =<< mbRouteStations

    routeLiveInfo <-
      case (mbRouteStation, booking.vehicleNumber) of
        (Just routeStation, Just vehicleNumber) -> JourneyUtils.getLiveRouteInfo integratedBppConfig vehicleNumber routeStation.code
        _ -> return Nothing

    let journeyLeg =
          DJL.JourneyLeg
            { id = journeyLegGuid,
              mode = mapVehicleCategoryToTripMode booking.vehicleType,
              groupCode = Nothing,
              startLocation = LatLngV2 fromLocation.lat fromLocation.lon,
              endLocation = LatLngV2 toLocation.lat toLocation.lon,
              distance = Just distance,
              duration = duration,
              agency = Just $ MultiModalAgency {name = integratedBppConfig.agencyKey, gtfsId = Just integratedBppConfig.feedKey},
              fromArrivalTime = Nothing,
              fromDepartureTime = Just booking.createdAt,
              toArrivalTime =
                duration >>= \duration' ->
                  Just $ addUTCTime (fromIntegral $ getSeconds duration') booking.createdAt,
              toDepartureTime = Nothing,
              fromStopDetails = Nothing,
              toStopDetails = Nothing,
              routeDetails =
                [ DRD.RouteDetails
                    { agencyGtfsId = Just integratedBppConfig.feedKey,
                      agencyName = Just integratedBppConfig.agencyKey,
                      alternateShortNames = [],
                      alternateRouteIds = Nothing,
                      endLocationLat = toLocation.lat,
                      endLocationLon = toLocation.lon,
                      frequency = Nothing,
                      fromArrivalTime = Nothing,
                      fromDepartureTime = Just booking.createdAt,
                      fromStopCode = Just booking.fromStationCode,
                      fromStopGtfsId = Just booking.fromStationCode,
                      fromStopName = booking.fromStationName,
                      fromStopPlatformCode = Nothing,
                      id = journeyRouteDetailsId,
                      journeyLegId = journeyLegGuid.getId,
                      legStartTime = Just booking.createdAt,
                      legEndTime =
                        duration >>= \duration' ->
                          Just $ addUTCTime (fromIntegral $ getSeconds duration') booking.createdAt,
                      routeCode = mbRouteStation <&> (.code),
                      routeColorCode = mbRouteStation >>= (.color),
                      routeColorName = mbRouteStation >>= (.color),
                      routeGtfsId = mbRouteStation <&> (.code),
                      routeLongName = mbRouteStation <&> (.longName),
                      routeShortName = mbRouteStation <&> (.shortName),
                      userBookedRouteShortName = Nothing,
                      startLocationLat = fromLocation.lat,
                      startLocationLon = fromLocation.lon,
                      subLegOrder = Just 1,
                      toArrivalTime =
                        duration >>= \duration' ->
                          Just $ addUTCTime (fromIntegral $ getSeconds duration') booking.createdAt,
                      toDepartureTime = Nothing,
                      toStopCode = Just booking.toStationCode,
                      toStopGtfsId = Just booking.toStationCode,
                      toStopName = booking.toStationName,
                      toStopPlatformCode = Nothing,
                      trackingStatus = Nothing,
                      trackingStatusLastUpdatedAt = Just now,
                      merchantId = Just booking.merchantId,
                      merchantOperatingCityId = Just booking.merchantOperatingCityId,
                      createdAt = now,
                      updatedAt = now
                    }
                ],
              liveVehicleAvailableServiceTypes = Nothing,
              estimatedMinFare = estimatedPrice <&> (.amount),
              estimatedMaxFare = estimatedPrice <&> (.amount),
              merchantId = booking.merchantId,
              merchantOperatingCityId = booking.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now,
              legSearchId = Just booking.searchId.getId,
              legPricingId = Just booking.quoteId.getId,
              changedBusesInSequence = Nothing,
              finalBoardedBusNumber = routeLiveInfo <&> (.vehicleNumber),
              finalBoardedBusNumberSource = routeLiveInfo <&> \_ -> DJL.UserSpotBooked,
              finalBoardedDepotNo = routeLiveInfo >>= (.depot),
              finalBoardedScheduleNo = routeLiveInfo >>= (.scheduleNo),
              finalBoardedWaybillId = routeLiveInfo >>= (.waybillId),
              finalBoardedBusServiceTierType = routeLiveInfo <&> (.serviceType),
              userBookedBusServiceTierType = mbRouteStation >>= (.vehicleServiceTier) <&> (._type),
              osmEntrance = Nothing,
              osmExit = Nothing,
              straightLineEntrance = Nothing,
              straightLineExit = Nothing,
              journeyId = journeyGuid,
              isDeleted = Just False,
              sequenceNumber = 0,
              multimodalSearchRequestId = Just booking.searchId.getId, -- Note :: This is not SearchRequest Table's ID. Do not use it to Query SearchReqeust Anywhere in Application.
              busLocationData = booking.busLocationData,
              busConductorId = routeLiveInfo >>= (.busConductorId),
              busDriverId = routeLiveInfo >>= (.busDriverId),
              providerRouteId = Nothing
            }

    QLocation.createMany [fromLocation, toLocation]
    QJourney.create journey
    QJourneyLeg.create journeyLeg
  where
    mkBookingJourneyCreateKey = "booking:journey:create:bookingId-" <> booking.id.getId

    mapVehicleCategoryToTripMode = \case
      Spec.BUS -> DTrip.Bus
      Spec.METRO -> DTrip.Metro
      Spec.SUBWAY -> DTrip.Subway

    getDistanceAndDuration :: (ServiceFlow m r) => Maybe LatLong -> Maybe LatLong -> m (Maybe (Distance, Seconds))
    getDistanceAndDuration (Just source) (Just destination) =
      runMaybeT $ do
        let req =
              GetDistancesReq
                { origins = NonEmpty.fromList [source],
                  destinations = NonEmpty.fromList [destination],
                  travelMode = Just Maps.CAR,
                  sourceDestinationMapping = Nothing,
                  distanceUnit = Meter
                }
        distances <- lift $ Maps.getMultimodalJourneyDistances booking.merchantId booking.merchantOperatingCityId Nothing req
        leastDistanceRoute <- hoistMaybe $ minimumByMay (\r1 r2 -> compare r1.distance r2.distance) (toList distances)
        pure (leastDistanceRoute.distanceWithUnit, leastDistanceRoute.duration)
    getDistanceAndDuration _ _ = return Nothing
