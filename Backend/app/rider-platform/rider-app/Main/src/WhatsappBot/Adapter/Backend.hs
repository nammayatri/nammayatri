{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Production 'BackendHandle' for the WhatsApp booking engine, over rider-app's
-- in-process 'Flow'. Each method calls the SAME API-layer wrapper / Domain fn the
-- rider-app UI hits (so the Beckn round-trip, ConfigPilot, etc. all run), then
-- flattens the result into the port-local @Bot*@ DTO the engine reads. Every
-- backend call is wrapped so a thrown Kernel exception becomes @Left BotError@ —
-- reproducing the TS per-call try/catch (@ny/client.ts@). Merchant identity +
-- rental config are closed over per session (resolved in the webhook dispatch);
-- @BotAuth.personId@ is the rider-app @Id Person@ as opaque Text.
--
-- Signature/field choices are the verified adapter-map (migration/JOURNAL.md,
-- scout wf_b95af1cc-dc9): every wrapper takes @(Id Person, Id Merchant)@ ids.
module WhatsappBot.Adapter.Backend (mkBackendHandle) where

import qualified API.Types.UI.Sos as SosAPI
import qualified API.UI.Confirm as UIConfirm
import qualified API.UI.Search as UISearch
import qualified API.UI.Select as UISelect
import qualified Beckn.ACL.Cancel as ACL
import Control.Applicative ((<|>))
import Data.List (sortOn)
import qualified Domain.Action.UI.Booking as DBooking
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Estimate as UEstimate
import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Action.UI.Registration as Reg
import qualified Domain.Action.UI.SavedReqLocation as DSavedReqLocation
import qualified Domain.Action.UI.Sos as DSos
import qualified Domain.Types.Booking.API as DBAPI
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingStatus as DBS
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.IntegratedBPPConfig as DIBPC
import qualified Domain.Types.LocationAddress as LA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.SavedReqLocation as DSRL
import Environment (Flow)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Common (Meters (..), Seconds (..))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (fromMaybeM, getCurrentTime, logError, throwError, withShortRetry)
import qualified Safety.Domain.Types.Sos as SafetySos
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Search as SLS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QR
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error (GenericError (InvalidRequest))
import qualified Tools.Maps as Maps
import qualified WhatsappBot.Cities as Cities
import WhatsappBot.Handles (BackendHandle (..))
import WhatsappBot.Types

-- | Build the prod backend handle for one resolved merchant-city.
mkBackendHandle :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> MerchantCtx -> BackendHandle Flow
mkBackendHandle merchantId mocId ctx =
  BackendHandle
    { authenticate = \phone ->
        runBot "authenticate" $ do
          personId <- Reg.createPersonWithPhoneNumber merchantId phone Nothing
          pure BotAuth {personId = personId.getId},
      getSavedLocations = \auth ->
        runBot "getSavedLocations" $ do
          res <- DSavedReqLocation.getSavedReqLocations (Id auth.personId)
          pure (map toBotSavedLocation res.list),
      searchPlaces = \_auth query mBias ->
        runBot "searchPlaces" $ do
          let center = maybe Cities.defaultCity (\ll -> Cities.findNearestCity ll.lat ll.lon) mBias
              locationText = show center.lat <> "," <> show center.lon
          resp <-
            Maps.autoComplete merchantId mocId Nothing $
              Maps.AutoCompleteReq
                { input = query,
                  sessionToken = Nothing,
                  location = locationText,
                  radius = 50000,
                  radiusWithUnit = Nothing,
                  language = Maps.ENGLISH,
                  strictbounds = Just False,
                  origin = Nothing,
                  types_ = Nothing,
                  country = Maps.India
                }
          pure (mapMaybe toBotPrediction resp.predictions),
      getPlaceDetails = \_auth placeId -> do
        res <- try @_ @SomeException $ Maps.getPlaceName merchantId mocId Nothing (placeNameReq (Maps.ByPlaceId placeId))
        case res of
          Left e -> logFail "getPlaceDetails" e $> Left (BotError "getPlaceDetails failed")
          Right resp -> pure $ maybe (Left (BotError "place not found")) (Right . toBotPlace placeId) (listToMaybe resp),
      reverseGeocode = \_auth ll -> do
        -- TS reverseGeocode never fails: it degrades to bare coords (client.ts:363-397).
        let fallback = bareCoordsPlace ll.lat ll.lon
        res <- try @_ @SomeException $ Maps.getPlaceName merchantId mocId Nothing (placeNameReq (Maps.ByLatLong (Maps.LatLong ll.lat ll.lon)))
        case res of
          Left e -> logFail "reverseGeocode" e $> Right fallback
          Right resp -> pure $ Right $ maybe fallback (toBotPlace (coordText ll.lat ll.lon)) (listToMaybe resp),
      searchRide = \auth origin destination ->
        runBot "searchRide" $ do
          let req = SLS.OneWaySearch (oneWaySearchReq origin destination)
          resp <- runSearch auth req
          pure resp.searchId.getId,
      getEstimates = \_auth searchId ->
        -- Poll-read: any error (incl. "still processing" 400) => [] so the engine keeps polling (client.ts:448-454).
        pollQuotes "getEstimates" searchId $ \res ->
          preferAuto (\e -> show e.vehicleVariant) (res.estimates) & map toBotEstimate,
      searchFlexi = \auth origin ->
        runBot "searchFlexi" $ do
          now <- getCurrentTime
          let req = SLS.RentalSearch (rentalSearchReq origin now)
          resp <- runSearch auth req
          pure resp.searchId.getId,
      getFlexiQuotes = \_auth searchId ->
        pollQuotes "getFlexiQuotes" searchId $ \res ->
          let qs = mapMaybe rentalOrDemandQuote res.quotes
           in preferAuto (\qe -> show qe.vehicleVariant) qs & map toBotQuote,
      confirmQuote = \auth quoteId ->
        runBot "confirmQuote" $ do
          res <- UIConfirm.confirm' (Id auth.personId, merchantId) (Id quoteId) Nothing Nothing Nothing Nothing Nothing
          pure res.bookingId.getId,
      selectEstimate = \auth estimateId ->
        runBot "selectEstimate" $ do
          let pid = Id auth.personId
          void $
            ActorInfo.withPersonIdActorInfo pid $
              UISelect.select2' (pid, merchantId) (Id estimateId) selectReq Nothing,
      getBookingDetails = \auth bookingId -> do
        let pid = Id auth.personId
        res <- try @_ @SomeException $ do
          mBooking <- QRB.findById (Id bookingId)
          case mBooking of
            Just booking | booking.riderId == pid -> do
              entity <- DBooking.bookingStatus (Id bookingId) (pid, merchantId) (Just True)
              pure (Just (toBotBooking entity))
            _ -> pure Nothing -- not found / not ours
        case res of
          Left e -> logFail "getBookingDetails" e $> Left (BotError "getBookingDetails failed")
          Right mb -> pure (Right mb),
      getActiveBookings = \auth mAfter ->
        runBot "getActiveBookings" $ do
          let pid = Id auth.personId
          res <-
            DBooking.bookingListV2
              (pid, merchantId)
              (Just 10)
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              []
              []
              [DBS.NEW, DBS.CONFIRMED, DBS.TRIP_ASSIGNED, DBS.AWAITING_REASSIGNMENT, DBS.REALLOCATED]
              []
              Nothing
              (Just DBAPI.BookingRequest)
              Nothing
              Nothing
              Nothing
          let rides = mapMaybe onlyRide res.list
              filtered = maybe rides (\t -> filter (\e -> e.createdAt >= t) rides) mAfter
          pure (map toBotBooking filtered),
      triggerSOS = \auth rideId mLoc ->
        -- No ride-ownership pre-check (unlike getBookingDetails/cancelRide): the engine
        -- only ever passes a rideId it resolved from THIS person's own active booking
        -- (getActiveBookings is scoped to personId), so it can't be a stranger's ride.
        runBot "triggerSOS" $ do
          res <-
            DSos.postSosCreate (Just (Id auth.personId), merchantId) $
              SosAPI.SosReq
                { customerLocation = (\ll -> Maps.LatLong ll.lat ll.lon) <$> mLoc,
                  flow = SafetySos.Police,
                  isRideEnded = Nothing,
                  notifyAllContacts = Nothing,
                  rideId = Just (Id rideId),
                  sendPNOnPostRideSOS = Nothing,
                  triggerApiList = Nothing
                }
          pure res.sosId.getId,
      markRideAsSafe = \auth sosId ->
        runBot "markRideAsSafe" $
          void $
            DSos.postSosMarkRideAsSafe
              (Just (Id auth.personId), merchantId)
              (Id sosId)
              SosAPI.MarkAsSafeReq {contacts = Nothing, isEndLiveTracking = Nothing, isMock = Nothing, isRideEnded = Nothing},
      cancelRide = \auth bookingId _rideStatus ->
        runBot "cancelRide" $ cancelRideImpl merchantId (Id auth.personId) bookingId,
      updateProfile = \auth upd ->
        runBot "updateProfile" $
          void $
            DProfile.updatePerson (Id auth.personId) merchantId (updateProfileReq upd) Nothing Nothing Nothing Nothing Nothing Nothing
    }
  where
    -- run a search' with the standard "no client version / not-dashboard" trailing args
    runSearch auth req =
      UISearch.search'
        (Id auth.personId, merchantId)
        req
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

    placeNameReq getBy =
      Maps.GetPlaceNameReq {getBy = getBy, sessionToken = Just "default-token", language = Just Maps.ENGLISH}

    oneWaySearchReq origin destination =
      SLS.OneWaySearchReq
        { origin = mkSearchLoc origin,
          destination = Just (mkSearchLoc destination),
          stops = Nothing,
          city = Nothing,
          isSourceManuallyMoved = Nothing,
          isDestinationManuallyMoved = Nothing,
          isSpecialLocation = Nothing,
          enforceTollRoute = Nothing,
          startTime = Nothing,
          isReallocationEnabled = Nothing,
          fareParametersInRateCard = Nothing,
          quotesUnifiedFlow = Just True,
          sessionToken = Nothing,
          placeNameSource = Just "API_MCP",
          driverIdentifier = Nothing,
          isMeterRideSearch = Nothing,
          recentLocationId = Nothing,
          platformType = Just DIBPC.APPLICATION,
          isReserveRide = Nothing,
          subscriptionId = Nothing,
          verifyBeforeCancellingOldBooking = Nothing,
          numberOfLuggages = Nothing,
          doMultimodalSearch = Nothing
        }

    rentalSearchReq origin now =
      SLS.RentalSearchReq
        { origin = mkSearchLoc origin,
          stops = Nothing,
          isSourceManuallyMoved = Just False,
          isSpecialLocation = Nothing,
          startTime = now,
          estimatedRentalDistance = Meters ctx.flexiRentalDistanceM,
          estimatedRentalDuration = Seconds ctx.flexiRentalDurationS,
          quotesUnifiedFlow = Nothing,
          isReallocationEnabled = Nothing,
          fareParametersInRateCard = Nothing,
          placeNameSource = Nothing,
          recentLocationId = Nothing,
          numberOfLuggages = Nothing,
          doMultimodalSearch = Nothing
        }

    selectReq =
      UISelect.DSelectReq
        { customerExtraFee = Nothing,
          customerExtraFeeWithCurrency = Nothing,
          autoAssignEnabled = True,
          autoAssignEnabledV2 = Just True,
          isPetRide = Just False,
          paymentMethodId = Nothing,
          paymentInstrument = Nothing,
          otherSelectedEstimates = Just [],
          isAdvancedBookingEnabled = Nothing,
          deliveryDetails = Nothing,
          disabilityDisable = Just True,
          billingCategory = Nothing,
          preferSafetyPlus = Nothing,
          driverPreference = Nothing,
          selectedOfferId = Nothing
        }

    updateProfileReq upd =
      DProfile.UpdateProfileReq
        { firstName = Nothing,
          middleName = Nothing,
          lastName = Nothing,
          email = Nothing,
          deviceToken = Nothing,
          notificationToken = Nothing,
          referralCode = Nothing,
          language = upd.language >>= parseLangCode,
          gender = Nothing,
          bundleVersion = Nothing,
          clientVersion = Nothing,
          businessEmail = Nothing,
          disability = Nothing,
          hasDisability = Nothing,
          enableOtpLessRide = Nothing,
          deviceId = Nothing,
          androidId = Nothing,
          liveActivityToken = Nothing,
          dateOfBirth = Nothing,
          profilePicture = Nothing,
          verificationChannel = Nothing,
          registrationLat = Nothing,
          registrationLon = Nothing,
          latestLat = Nothing,
          latestLon = Nothing,
          marketingParams = Nothing,
          mbMobileNumber = Nothing,
          mbMobileCountryCode = Nothing,
          paymentMode = Nothing,
          driverPreference = Nothing
        }

-- | Cancel one booking, replicating @API/UI/Cancel.hs:110-116@ in Flow with an
-- ownership pre-check. reasonStage from the fetched booking status (client.ts:650).
cancelRideImpl :: Id DM.Merchant -> Id SP.Person -> Text -> Flow ()
cancelRideImpl merchantId pid bookingId = do
  booking <- QRB.findById (Id bookingId) >>= fromMaybeM (InvalidRequest "booking not found")
  unless (booking.riderId == pid) $ throwError (InvalidRequest "not your booking")
  mRide <- B.runInReplica $ QR.findActiveByRBId booking.id
  let stage =
        if booking.status `elem` [DBS.TRIP_ASSIGNED, DBS.CONFIRMED]
          then SCR.OnAssign
          else SCR.OnSearch
      cancelReq =
        DCancel.CancelReq
          { reasonCode = SCR.CancellationReasonCode "CHANGE_OF_MIND",
            reasonStage = stage,
            additionalInfo = Nothing,
            reallocate = Nothing,
            blockOnCancellationRate = Nothing,
            abortPaytmEdc = Nothing
          }
  dCancelRes <- DCancel.cancel booking mRide cancelReq SBCR.ByUser
  void $ withShortRetry $ CallBPP.cancelV2 merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes Nothing

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Run a Flow action, mapping any thrown exception to @Left (BotError ...)@ (the
-- clean label is user-visible in a few engine failure templates; the raw
-- exception is logged for ops). Mirrors the TS per-call try/catch.
runBot :: Text -> Flow a -> Flow (Either BotError a)
runBot label act = do
  res <- try @_ @SomeException act
  case res of
    Left e -> logFail label e $> Left (BotError (label <> " failed"))
    Right a -> pure (Right a)

logFail :: Text -> SomeException -> Flow ()
logFail label e = logError $ "whatsapp-bot/" <> label <> ": " <> show e

-- | Poll-read quotes/estimates: on ANY error return @Right []@ (still-processing
-- parity, client.ts 400 => []).
pollQuotes :: Text -> Text -> (DQuote.GetQuotesRes -> [a]) -> Flow (Either BotError [a])
pollQuotes label searchId extract = do
  res <- try @_ @SomeException $ DQuote.getQuotes (Id searchId) Nothing
  case res of
    Left e -> logFail label e $> Right []
    Right r -> pure (Right (extract r))

rentalOrDemandQuote :: DQuote.OfferRes -> Maybe DQuote.QuoteAPIEntity
rentalOrDemandQuote = \case
  DQuote.OnRentalCab qe -> Just qe
  DQuote.OnDemandCab qe -> Just qe
  _ -> Nothing

onlyRide :: DBooking.BookingAPIEntityV2 -> Maybe DBAPI.BookingAPIEntity
onlyRide = \case
  DBooking.Ride e -> Just e
  _ -> Nothing

-- | Stable AUTO_RICKSHAW-first sort (client.ts prefers auto, else first).
preferAuto :: (a -> Text) -> [a] -> [a]
preferAuto variantOf = sortOn (\x -> if variantOf x == "AUTO_RICKSHAW" then (0 :: Int) else 1)

mkSearchLoc :: BotPlace -> SLS.SearchReqLocation
mkSearchLoc p =
  SLS.SearchReqLocation
    { gps = Maps.LatLong {lat = p.lat, lon = p.lon},
      address =
        LA.LocationAddress
          { street = p.address.street,
            door = Nothing,
            city = p.address.city,
            state = p.address.state,
            country = p.address.country <|> Just "India",
            building = p.address.building,
            areaCode = Nothing,
            area = p.address.area,
            ward = Nothing,
            placeId = Just p.placeId,
            instructions = Nothing,
            title = Nothing,
            extras = Nothing
          }
    }

toBotSavedLocation :: DSRL.SavedReqLocationAPIEntity -> BotSavedLocation
toBotSavedLocation s =
  BotSavedLocation
    { tag = s.tag,
      lat = s.lat,
      lon = s.lon,
      area = s.area,
      building = s.building,
      city = s.city,
      country = s.country,
      state = s.state,
      placeId = s.placeId
    }

toBotPrediction :: Maps.Prediction -> Maybe BotPrediction
toBotPrediction p =
  (\pid -> BotPrediction {description = p.description, placeId = pid, distance = fromIntegral <$> p.distance}) <$> p.placeId

toBotPlace :: Text -> Maps.PlaceName -> BotPlace
toBotPlace fallbackPid p =
  BotPlace
    { lat = p.location.lat,
      lon = p.location.lon,
      placeId = fromMaybe fallbackPid p.placeId,
      address =
        BotAddress
          { area = getComp "sublocality_level_1" <|> getComp "sublocality",
            building = getComp "premise" <|> getComp "street_number",
            city = getComp "locality",
            state = getComp "administrative_area_level_1",
            country = getComp "country",
            street = getComp "route"
          }
    }
  where
    getComp typ = (\c -> c.longName) <$> find (\c -> typ `elem` c.types) p.addressComponents

coordText :: Double -> Double -> Text
coordText lat lon = show lat <> "," <> show lon

bareCoordsPlace :: Double -> Double -> BotPlace
bareCoordsPlace lat lon =
  BotPlace
    { lat = lat,
      lon = lon,
      placeId = coordText lat lon,
      address = BotAddress {area = Nothing, building = Nothing, city = Nothing, country = Nothing, state = Nothing, street = Nothing}
    }

toBotEstimate :: UEstimate.EstimateAPIEntity -> BotEstimate
toBotEstimate e =
  BotEstimate
    { estimateId = e.id.getId,
      estimatedFare = fromIntegral e.estimatedFare.getMoney,
      serviceTierName = fromMaybe (show e.vehicleVariant) e.serviceTierName,
      vehicleVariant = show e.vehicleVariant,
      totalFareRange =
        FareRange
          { minFare = fromIntegral e.totalFareRange.minFare.getMoney,
            maxFare = fromIntegral e.totalFareRange.maxFare.getMoney
          },
      estimatedPickupDuration = (\s -> fromIntegral s.getSeconds) <$> e.estimatedPickupDuration
    }

toBotQuote :: DQuote.QuoteAPIEntity -> BotQuote
toBotQuote qe =
  BotQuote
    { quoteId = qe.id.getId,
      serviceTierName = qe.serviceTierName <|> Just (show qe.vehicleVariant),
      estimatedFare = Just (fromIntegral qe.estimatedTotalFare.getMoney),
      vehicleVariant = Just (show qe.vehicleVariant)
    }

toBotBooking :: DBAPI.BookingAPIEntity -> BotBookingDetails
toBotBooking e =
  let mRide = listToMaybe e.rideList
   in BotBookingDetails
        { bookingId = e.id.getId,
          rideId = (\r -> r.id.getId) <$> mRide,
          bookingStatus = show e.status,
          rideStatus = (\r -> show r.status) <$> mRide,
          rideStartTime = mRide >>= (.rideStartTime),
          rideEndTime = mRide >>= (.rideEndTime),
          driverArrivalTime = mRide >>= (.driverArrivalTime),
          driverName = (\r -> r.driverName) <$> mRide,
          driverNumber = (mRide >>= (.driverNumber)) <|> Just e.merchantExoPhone,
          vehicleNumber = (\r -> r.vehicleNumber) <$> mRide,
          rideOtp = (\r -> r.rideOtp) <$> mRide,
          endOtp = mRide >>= (.endOtp),
          driverRating = realToFrac <$> (mRide >>= (.driverRatings)),
          etaMinutes = Nothing,
          computedFare = (\m -> fromIntegral m.getMoney) <$> (mRide >>= (.computedPrice)),
          chargeableDistanceM = realToFrac <$> (mRide >>= (.chargeableRideDistance))
        }

-- | Map a 2-letter bot language code to the rider-app 'Maps.Language' enum
-- (engine.ts:19-22: lowercase ISO 400s, so send the enum name).
parseLangCode :: Text -> Maybe Maps.Language
parseLangCode = \case
  "en" -> Just Maps.ENGLISH
  "hi" -> Just Maps.HINDI
  "kn" -> Just Maps.KANNADA
  "ta" -> Just Maps.TAMIL
  "te" -> Just Maps.TELUGU
  "gu" -> Just Maps.GUJARATI
  _ -> Nothing
