module SharedLogic.RideFeedback.Context
  ( RideFeedbackContext (..),
    BookingCtx (..),
    FareCtx (..),
    RideCtx (..),
    DriverCtx (..),
    CityCtx (..),
    DerivedCtx (..),
    AnswerCtx (..),
    ContextInput (..),
    buildRideFeedbackContext,
    withAnswer,
    mkAnswerCtx,
  )
where

import Control.Applicative ((<|>))
import Data.Default.Class (Default (..))
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Booking as DB
import Domain.Types.Common (ServiceTierType (COMFY), TripCategory, isRideOtpTrip)
import qualified Domain.Types.DriverStats as DDS
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import Domain.Types.VehicleVariant (VehicleVariant (SEDAN))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Seconds (..))
import Kernel.Types.Id
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.RideFeedback.Rule (stableBucket)

-- | The JSON object the RIDE-FEEDBACK targeting logic and action rules are evaluated against.
-- Fields are listed explicitly (no whole-entity serialisation) so OTPs, phone numbers
-- and other PII never reach a rule or the dashboard preview. ride and driver are null before a
-- driver is assigned (booking stages).
data RideFeedbackContext = RideFeedbackContext
  { stage :: DRFC.RideFeedbackStage,
    booking :: BookingCtx,
    ride :: Maybe RideCtx,
    driver :: Maybe DriverCtx,
    city :: CityCtx,
    derived :: DerivedCtx,
    answer :: Maybe AnswerCtx
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingCtx = BookingCtx
  { merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    vehicleServiceTierType :: ServiceTierType,
    serviceTierName :: Text,
    tripCategory :: Text,
    -- | The rider shows an OTP to a driver at pickup (airport and spot queues); the ride starts then.
    isRideOtp :: Bool,
    isAirConditioned :: Maybe Bool,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    estimatedFare :: Double,
    estimatedDistanceMeters :: Maybe Int,
    estimatedDurationSeconds :: Maybe Int,
    isScheduled :: Bool,
    isPetRide :: Bool,
    hasStops :: Maybe Bool,
    roundTrip :: Maybe Bool,
    -- | The quoted fare includes tolls / a parking charge: the TOLL_CHARGES / PARKING_CHARGE items
    -- of the fare breakup the rider sees, which are built from these same fare parameters.
    hasTollCharges :: Bool,
    hasParkingCharge :: Bool,
    -- | The quoted fare, item by item.
    fare :: FareCtx,
    -- | e.g. "Cash", "UPI", "Card_DefaultCardType"; "" when not known.
    paymentInstrument :: Text,
    isCashPayment :: Bool,
    numberOfLuggages :: Int,
    -- | Deliveries: "" for passenger rides.
    parcelType :: Text,
    parcelQuantity :: Int,
    isSafetyPlus :: Bool,
    isInsured :: Bool,
    isPickupOrDestinationEdited :: Bool,
    stopsCount :: Int,
    distanceToPickupMeters :: Int,
    -- | The driver's ETA to the pickup when they took the ride.
    etaToPickupSeconds :: Int,
    seatingCapacity :: Int,
    isReferredRide :: Bool,
    isDashboardRequest :: Bool,
    isAutoAccepted :: Bool,
    -- | e.g. "ENGLISH", "HINDI"; "" when not known.
    customerLanguage :: Text,
    -- | Special-zone side of the trip: "Default", "Pickup", "Drop" or "PickupDrop".
    areaType :: Text,
    -- | Special zones: "CommissionOnly", "FullPayment", "ParkingOnly", "CommissionAndParking"; "" otherwise.
    fareSettlementType :: Text,
    hasPickupGate :: Bool,
    -- | A round trip with a return leg booked.
    hasReturnLeg :: Bool,
    bookingDeposit :: Double,
    -- | The rider platform (BAP) that booked the ride.
    bapId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Each charge of the quoted fare, 0 when it does not apply (never null: comparing null fails a rule).
data FareCtx = FareCtx
  { -- | "PROGRESSIVE", "SLAB", "RENTAL", "INTER_CITY" or "AMBULANCE".
    fareType :: Text,
    baseFare :: Double,
    tollCharges :: Double,
    parkingCharge :: Double,
    waitingCharge :: Double,
    rideExtraTimeFare :: Double,
    nightShiftCharge :: Double,
    congestionCharge :: Double,
    priorityCharges :: Double,
    -- | Added by the rider when booking.
    customerExtraFee :: Double,
    -- | Added by the driver on their offer.
    driverSelectedFare :: Double,
    airportConvenienceFee :: Double,
    boothCharge :: Double,
    stopCharges :: Double,
    petCharges :: Double,
    luggageCharge :: Double,
    returnFeeCharge :: Double,
    insuranceCharge :: Double,
    schedulingCharge :: Double,
    platformFee :: Double,
    serviceCharge :: Double,
    govtCharges :: Double,
    driverAllowance :: Double,
    personalDiscount :: Double,
    businessDiscount :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideCtx = RideCtx
  { status :: DRide.RideStatus,
    vehicleVariant :: Maybe VehicleVariant,
    rideTags :: [Text],
    traveledDistanceMeters :: Double,
    isAirConditioned :: Maybe Bool,
    isInsured :: Bool,
    driverArrived :: Bool,
    -- | 0 until the driver arrives.
    secondsSinceDriverArrived :: Int,
    -- | How much later than their ETA the driver reached (or, not there yet, still is from) the pickup; 0 when on time.
    pickupDelaySeconds :: Int,
    -- | While heading to the pickup: "REACHED_PICKUP", "PROGRESSING", "DETOURING", "STALLED", "MOVING_AWAY", "GPS_DARK"; "" when not tracked.
    pickupBehaviour :: Text,
    -- | Distance travelled as a percentage of the estimated distance (0 when there is no estimate).
    progressPercent :: Int,
    driverDeviatedFromRoute :: Bool,
    driverDeviatedToTollRoute :: Bool,
    driverGpsTurnedOff :: Bool,
    safetyAlertTriggered :: Bool,
    hasSos :: Bool,
    estimatedTollCharges :: Double,
    tipAmount :: Double,
    isFreeRide :: Bool,
    onlinePayment :: Bool,
    isAdvanceBooking :: Bool,
    isOtpLessRide :: Bool,
    isDriverSpecialLocWarrior :: Bool,
    hasFleetOwner :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | The driver of the ride: what driver-side targeting can use.
data DriverCtx = DriverCtx
  { driverTags :: [Text],
    rating :: Maybe Double,
    totalRides :: Int,
    isFemaleDriver :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CityCtx = CityCtx
  { -- | The city name, e.g. "Chennai" (City's own JSON is its STD code, e.g. "std:044").
    cityName :: Text,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DerivedCtx = DerivedCtx
  { -- | Since the journey was first booked (a reallocation keeps counting from the first booking).
    secondsSinceBooking :: Int,
    secondsSinceRideStart :: Maybe Int,
    secondsSinceAssign :: Maybe Int,
    localHour :: Int,
    dayOfWeek :: Text,
    -- | Stable 0–99 bucket of the rider (or the ride when the rider is unknown), for % splits in rules.
    rolloutBucket :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AnswerCtx = AnswerCtx
  { selectedOptionKeys :: [Text],
    primaryOptionKey :: Maybe Text,
    text :: Maybe Text,
    number :: Maybe Double,
    rating :: Maybe Int,
    hasMedia :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Sample input the dashboard shows when verifying a RIDE_FEEDBACK logic (a Bangalore AC cab, 5 minutes into the ride).
instance Default RideFeedbackContext where
  def =
    RideFeedbackContext
      { stage = DRFC.RIDE_IN_PROGRESS,
        booking =
          BookingCtx
            { merchantOperatingCityId = Id "merchant-operating-city-id",
              vehicleServiceTierType = COMFY,
              serviceTierName = "AC Mini",
              tripCategory = "OneWay",
              isRideOtp = False,
              isAirConditioned = Just True,
              specialLocationTag = Nothing,
              specialLocationName = Nothing,
              estimatedFare = 310,
              estimatedDistanceMeters = Just 8000,
              estimatedDurationSeconds = Just 1200,
              isScheduled = False,
              isPetRide = False,
              hasStops = Just False,
              roundTrip = Just False,
              hasTollCharges = False,
              hasParkingCharge = False,
              fare =
                FareCtx
                  { fareType = "PROGRESSIVE",
                    baseFare = 100,
                    tollCharges = 0,
                    parkingCharge = 0,
                    waitingCharge = 0,
                    rideExtraTimeFare = 0,
                    nightShiftCharge = 0,
                    congestionCharge = 0,
                    priorityCharges = 0,
                    customerExtraFee = 0,
                    driverSelectedFare = 0,
                    airportConvenienceFee = 0,
                    boothCharge = 0,
                    stopCharges = 0,
                    petCharges = 0,
                    luggageCharge = 0,
                    returnFeeCharge = 0,
                    insuranceCharge = 0,
                    schedulingCharge = 0,
                    platformFee = 0,
                    serviceCharge = 0,
                    govtCharges = 0,
                    driverAllowance = 0,
                    personalDiscount = 0,
                    businessDiscount = 0
                  },
              paymentInstrument = "UPI",
              isCashPayment = False,
              numberOfLuggages = 0,
              parcelType = "",
              parcelQuantity = 0,
              isSafetyPlus = False,
              isInsured = False,
              isPickupOrDestinationEdited = False,
              stopsCount = 0,
              distanceToPickupMeters = 1200,
              etaToPickupSeconds = 300,
              seatingCapacity = 4,
              isReferredRide = False,
              isDashboardRequest = False,
              isAutoAccepted = False,
              customerLanguage = "ENGLISH",
              areaType = "Default",
              fareSettlementType = "",
              hasPickupGate = False,
              hasReturnLeg = False,
              bookingDeposit = 0,
              bapId = "bap-id"
            },
        ride =
          Just
            RideCtx
              { status = DRide.INPROGRESS,
                vehicleVariant = Just SEDAN,
                rideTags = [],
                traveledDistanceMeters = 2000,
                isAirConditioned = Just True,
                isInsured = False,
                driverArrived = True,
                secondsSinceDriverArrived = 420,
                pickupDelaySeconds = 0,
                pickupBehaviour = "REACHED_PICKUP",
                progressPercent = 25,
                driverDeviatedFromRoute = False,
                driverDeviatedToTollRoute = False,
                driverGpsTurnedOff = False,
                safetyAlertTriggered = False,
                hasSos = False,
                estimatedTollCharges = 0,
                tipAmount = 0,
                isFreeRide = False,
                onlinePayment = False,
                isAdvanceBooking = False,
                isOtpLessRide = False,
                isDriverSpecialLocWarrior = False,
                hasFleetOwner = False
              },
        driver = Just DriverCtx {driverTags = [], rating = Just 4.8, totalRides = 250, isFemaleDriver = False},
        city = CityCtx {cityName = "Bangalore", merchantOperatingCityId = Id "merchant-operating-city-id"},
        derived =
          DerivedCtx
            { secondsSinceBooking = 900,
              secondsSinceRideStart = Just 300,
              secondsSinceAssign = Just 600,
              localHour = 18,
              dayOfWeek = "MON",
              rolloutBucket = 42
            },
        answer = Nothing
      }

data ContextInput = ContextInput
  { now :: UTCTime,
    timeDiffFromUtc :: Seconds,
    stage :: DRFC.RideFeedbackStage,
    booking :: DB.Booking,
    -- | When the journey was first booked.
    bookedAt :: UTCTime,
    -- | The ride with its driver and their stats, once a driver is assigned.
    rideWithDriver :: Maybe (DRide.Ride, DP.Person, Maybe DDS.DriverStats),
    city :: Context.City
  }

buildRideFeedbackContext :: ContextInput -> RideFeedbackContext
buildRideFeedbackContext ContextInput {..} =
  RideFeedbackContext
    { stage,
      booking =
        BookingCtx
          { merchantOperatingCityId = booking.merchantOperatingCityId,
            vehicleServiceTierType = booking.vehicleServiceTier,
            serviceTierName = booking.vehicleServiceTierName,
            tripCategory = tripCategoryTag booking.tripCategory,
            isRideOtp = isRideOtpTrip booking.tripCategory,
            isAirConditioned = booking.isAirConditioned,
            specialLocationTag = booking.specialLocationTag,
            specialLocationName = booking.specialLocationName,
            estimatedFare = realToFrac booking.estimatedFare,
            estimatedDistanceMeters = (.getMeters) <$> booking.estimatedDistance,
            estimatedDurationSeconds = (.getSeconds) <$> booking.estimatedDuration,
            isScheduled = booking.isScheduled,
            isPetRide = booking.isPetRide,
            hasStops = booking.hasStops,
            roundTrip = booking.roundTrip,
            hasTollCharges = maybe False (> 0) booking.fareParams.tollCharges,
            hasParkingCharge = maybe False (> 0) booking.fareParams.parkingCharge,
            fare = mkFareCtx booking.fareParams,
            paymentInstrument = maybe "" show booking.paymentInstrument,
            isCashPayment = booking.paymentInstrument == Just DMPM.Cash,
            numberOfLuggages = fromMaybe 0 booking.numberOfLuggages,
            parcelType = maybe "" show booking.parcelType,
            parcelQuantity = fromMaybe 0 booking.parcelQuantity,
            isSafetyPlus = booking.isSafetyPlus,
            isInsured = booking.isInsured,
            isPickupOrDestinationEdited = fromMaybe False booking.isPickupOrDestinationEdited,
            stopsCount = length booking.stops,
            distanceToPickupMeters = maybe 0 (.getMeters) booking.distanceToPickup,
            etaToPickupSeconds = maybe 0 (.getSeconds) booking.dqDurationToPickup,
            seatingCapacity = fromMaybe 0 booking.vehicleServiceTierSeatingCapacity,
            isReferredRide = fromMaybe False booking.isReferredRide,
            isDashboardRequest = booking.isDashboardRequest,
            isAutoAccepted = fromMaybe False booking.isAutoAccepted,
            customerLanguage = maybe "" show booking.customerLanguage,
            areaType = maybe "Default" areaTypeOf booking.area,
            fareSettlementType = maybe "" show booking.fareSettlementType,
            hasPickupGate = isJust booking.pickupGateId,
            hasReturnLeg = isJust booking.returnTime,
            bookingDeposit = money booking.bookingDeposit,
            bapId = booking.bapId
          },
      ride =
        mbRide <&> \ride ->
          RideCtx
            { status = ride.status,
              vehicleVariant = ride.vehicleVariant,
              rideTags = map (tagName . (.getTagNameValue)) (fromMaybe [] ride.rideTags),
              traveledDistanceMeters = realToFrac ride.traveledDistance,
              isAirConditioned = ride.isAirConditioned,
              isInsured = ride.isInsured,
              driverArrived = isJust ride.driverArrivalTime,
              secondsSinceDriverArrived = maybe 0 secondsSince ride.driverArrivalTime,
              pickupDelaySeconds = pickupDelay ride,
              pickupBehaviour = maybe "" show ride.pickupBehaviour,
              progressPercent = progress ride,
              driverDeviatedFromRoute = fromMaybe False ride.driverDeviatedFromRoute,
              driverDeviatedToTollRoute = fromMaybe False ride.driverDeviatedToTollRoute,
              driverGpsTurnedOff = fromMaybe False ride.driverGpsTurnedOff,
              safetyAlertTriggered = ride.safetyAlertTriggered,
              hasSos = isJust ride.sosId,
              estimatedTollCharges = money ride.estimatedTollCharges,
              tipAmount = money ride.tipAmount,
              isFreeRide = fromMaybe False ride.isFreeRide,
              onlinePayment = ride.onlinePayment,
              isAdvanceBooking = ride.isAdvanceBooking,
              isOtpLessRide = fromMaybe False ride.enableOtpLessRide,
              isDriverSpecialLocWarrior = ride.isDriverSpecialLocWarrior,
              hasFleetOwner = isJust ride.fleetOwnerId
            },
      driver =
        rideWithDriver <&> \(_, driverPerson, driverStats) ->
          DriverCtx
            { driverTags = map (tagName . (.getTagNameValueExpiry)) (fromMaybe [] driverPerson.driverTag),
              rating = realToFrac <$> (driverStats >>= (.rating)),
              totalRides = maybe 0 (.totalRides) driverStats,
              isFemaleDriver = driverPerson.gender == DP.FEMALE
            },
      city = CityCtx {cityName = show city, merchantOperatingCityId = booking.merchantOperatingCityId},
      derived =
        DerivedCtx
          { secondsSinceBooking = secondsSince bookedAt,
            secondsSinceRideStart = secondsSince <$> (mbRide >>= (.tripStartTime)),
            secondsSinceAssign = secondsSince . (.createdAt) <$> mbRide,
            localHour = Time.todHour localTime,
            dayOfWeek = T.toUpper . T.take 3 . show $ Time.dayOfWeek (Time.localDay localDateTime),
            rolloutBucket = stableBucket (maybe booking.id.getId (.getId) booking.riderId)
          },
      answer = Nothing
    }
  where
    mbRide = rideWithDriver <&> \(ride, _, _) -> ride
    secondsSince t = floor (Time.diffUTCTime now t)
    money = maybe 0 realToFrac
    -- The driver's lateness against the ETA they had when the ride was assigned: by their arrival,
    -- or by now while they are still on the way (the ride started without an arrival: by its start).
    pickupDelay ride = fromMaybe 0 $ do
      eta <- booking.dqDurationToPickup
      let expected = Time.addUTCTime (fromIntegral eta.getSeconds) ride.createdAt
          reached = ride.driverArrivalTime <|> ride.tripStartTime <|> (if ride.status == DRide.NEW then Just now else Nothing)
      reachedAt <- reached
      pure $ max 0 (floor (Time.diffUTCTime reachedAt expected))
    progress ride = case booking.estimatedDistance of
      Just d | d.getMeters > 0 -> floor (100 * realToFrac ride.traveledDistance / (fromIntegral d.getMeters :: Double))
      _ -> 0
    localDateTime = Time.utcToLocalTime Time.utc (Time.addUTCTime (fromIntegral timeDiffFromUtc.getSeconds) now)
    localTime = Time.localTimeOfDay localDateTime
    -- Tags are stored as name#value(#expiry); rules match on the name.
    tagName = T.takeWhile (/= '#')

withAnswer :: DRFR.RideFeedbackAnswer -> RideFeedbackContext -> RideFeedbackContext
withAnswer ans ctx = ctx {answer = Just (mkAnswerCtx ans)}

mkAnswerCtx :: DRFR.RideFeedbackAnswer -> AnswerCtx
mkAnswerCtx ans =
  let keys = fromMaybe [] ans.selectedOptionKeys
   in AnswerCtx
        { selectedOptionKeys = keys,
          primaryOptionKey = listToMaybe keys,
          text = ans.text,
          number = ans.number,
          rating = ans.rating,
          hasMedia = maybe False (not . null) ans.mediaFileIds
        }

-- | Constructor name only ("OneWay", "Rental", "InterCity", ...), which is what rules compare against.
tripCategoryTag :: TripCategory -> Text
tripCategoryTag = T.takeWhile (/= ' ') . show

mkFareCtx :: DFP.FareParameters -> FareCtx
mkFareCtx fp =
  FareCtx
    { fareType = case fp.fareParametersDetails of
        DFP.ProgressiveDetails _ -> "PROGRESSIVE"
        DFP.SlabDetails _ -> "SLAB"
        DFP.RentalDetails _ -> "RENTAL"
        DFP.InterCityDetails _ -> "INTER_CITY"
        DFP.AmbulanceDetails _ -> "AMBULANCE",
      baseFare = realToFrac fp.baseFare,
      tollCharges = money fp.tollCharges,
      parkingCharge = money fp.parkingCharge,
      waitingCharge = money fp.waitingCharge,
      rideExtraTimeFare = money fp.rideExtraTimeFare,
      nightShiftCharge = money fp.nightShiftCharge,
      congestionCharge = money fp.congestionCharge,
      priorityCharges = money fp.priorityCharges,
      customerExtraFee = money fp.customerExtraFee,
      driverSelectedFare = money fp.driverSelectedFare,
      airportConvenienceFee = money fp.airportConvenienceFee,
      boothCharge = money fp.boothCharge,
      stopCharges = money fp.stopCharges,
      petCharges = money fp.petCharges,
      luggageCharge = money fp.luggageCharge,
      returnFeeCharge = money fp.returnFeeCharge,
      insuranceCharge = money fp.insuranceCharge,
      schedulingCharge = money fp.schedulingCharge,
      platformFee = money fp.platformFee,
      serviceCharge = money fp.serviceCharge,
      govtCharges = money fp.govtCharges,
      driverAllowance = money fp.driverAllowance,
      personalDiscount = money fp.personalDiscount,
      businessDiscount = money fp.businessDiscount
    }
  where
    money = maybe 0 realToFrac

-- | Which side of the trip is in a special zone (the zone ids themselves are not useful to rules).
areaTypeOf :: SL.Area -> Text
areaTypeOf = \case
  SL.Default -> "Default"
  SL.Pickup _ _ -> "Pickup"
  SL.Drop _ -> "Drop"
  SL.PickupDrop {} -> "PickupDrop"
