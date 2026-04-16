{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.OnDemand.Utils.Common where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import qualified BecknV2.OnDemand.Enums as Enums
import BecknV2.OnDemand.Tags ((~=), (~=?), (~=|))
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.OnDemand.Utils.Constructors
import BecknV2.OnDemand.Utils.Context as ContextUtils
import BecknV2.OnDemand.Utils.Payment
import qualified BecknV2.Utils as Utils
import Control.Lens
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.List.NonEmpty
import qualified Data.Map.Strict
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Domain.Action.Beckn.Search
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Types as DT
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.DriverStats as DDriverStats
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FarePolicy as Policy
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as MOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as Variant
import EulerHS.Prelude hiding (id, state, view, whenM, (%~), (^?))
import qualified EulerHS.Prelude as Prelude
import GHC.Float (double2Int)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Maps as Maps
import qualified Kernel.Storage.Hedis as Hedis
import Text.Printf (printf)
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude hiding (find, length, map, null, readMaybe)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common hiding (mkPrice)
import qualified Kernel.Types.Common as Common
import Kernel.Types.Confidence
import Kernel.Types.Id
import qualified Kernel.Types.Price
import Kernel.Utils.Common hiding (mkPrice)
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified Storage.CachedQueries.BlackListOrg as QBlackList
import qualified Storage.CachedQueries.WhiteListOrg as QWhiteList
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error

data Pricing = Pricing
  { pricingId :: Text,
    pricingMaxFare :: HighPrecMoney,
    pricingMinFare :: HighPrecMoney,
    vehicleServiceTier :: DT.ServiceTierType,
    serviceTierName :: Text,
    serviceTierDescription :: Maybe Text,
    vehicleVariant :: Variant.VehicleVariant,
    tripCategory :: DT.TripCategory,
    fareParams :: Maybe Params.FareParameters,
    farePolicy :: Maybe Policy.FarePolicy,
    estimatedDistance :: Maybe Meters,
    specialLocationTag :: Maybe Text,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isBlockedRoute :: Maybe Bool,
    fulfillmentType :: Text,
    distanceToNearestDriver :: Maybe Meters,
    tollNames :: Maybe [Text],
    tipOptions :: Maybe [Int],
    currency :: Currency,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    specialLocationName :: Maybe Text,
    vehicleIconUrl :: Maybe BaseUrl,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    businessDiscount :: Maybe HighPrecMoney,
    personalDiscount :: Maybe HighPrecMoney,
    qar :: Maybe Double,
    isScheduled :: Bool
  }

data RateCardBreakupItem = RateCardBreakupItem
  { title :: Text,
    value :: Text
  }

-- | Get the fulfillment ID from booking, stripping "I-" prefix from estimateId if present
getBookingFulfillmentId :: DBooking.Booking -> Text
getBookingFulfillmentId booking =
  case booking.estimateId of
    Just eid -> let raw = getId eid in fromMaybe raw (T.stripPrefix "I-" raw)
    Nothing -> booking.quoteId

mkStops :: Maps.LatLong -> Maybe Maps.LatLong -> [Maps.LatLong] -> Maybe [Spec.Stop]
mkStops origin mbDestination intermediateStops = do
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps destination = Gps.Gps {lat = destination.lat, lon = destination.lon}
  Just $
    catMaybes
      [ Just $
          emptyStop
            { Spec.stopLocation =
                Just $ emptyLocation {Spec.locationGps = Utils.gpsToText originGps},
              Spec.stopType = Just $ show Enums.START,
              Spec.stopId = Nothing
            },
        ( \destination ->
            emptyStop
              { Spec.stopLocation =
                  Just $ emptyLocation {Spec.locationGps = Utils.gpsToText $ destinationGps destination},
                Spec.stopType = Just $ show Enums.END,
                Spec.stopId = Nothing,
                Spec.stopParentStopId = Nothing
              }
        )
          <$> mbDestination
      ]
      <> (map (\(location, order) -> mkIntermediateStopSearch location order (order - 1)) $ zip intermediateStops [1 ..])

parseLatLong :: MonadFlow m => Text -> m Maps.LatLong
parseLatLong a =
  case T.splitOn "," a of
    [latStr, longStr] ->
      let lat = fromMaybe 0.0 $ readMaybe $ T.unpack latStr
          lon = fromMaybe 0.0 $ readMaybe $ T.unpack longStr
       in return $ Maps.LatLong lat lon
    _ -> throwError . InvalidRequest $ "Unable to parse LatLong"

getContextCity :: MonadFlow m => Spec.Context -> m Context.City
getContextCity context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  city <- location.locationCity & fromMaybeM (InvalidRequest "Missing locationCity")
  cityText <- city.cityCode & fromMaybeM (InvalidRequest "Missing cityCode")
  decode (encode cityText) & fromMaybeM (InvalidRequest $ "Error in parsing cityCode: " <> cityText)

getContextCountry :: MonadFlow m => Spec.Context -> m Context.Country
getContextCountry context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  country <- location.locationCountry & fromMaybeM (InvalidRequest "Missing locationCountry")
  countryCodeText <- country.countryCode & fromMaybeM (InvalidRequest "Missing countryCode")
  decode (encode countryCodeText) & fromMaybeM (InvalidRequest $ "Error in parsing countryCode: " <> countryCodeText)

getContextBapUri :: MonadFlow m => Spec.Context -> m BaseUrl
getContextBapUri context = do
  bapUriText <- context.contextBapUri & fromMaybeM (InvalidRequest "Missing contextBapUri")
  decode (encode bapUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBapUri: " <> bapUriText)

getContextBppUri :: MonadFlow m => Spec.Context -> m (Maybe BaseUrl)
getContextBppUri context = do
  let mbBppUriText = context.contextBppUri
  case mbBppUriText of
    Nothing -> pure Nothing
    Just bppUriText -> Just <$> A.decode (A.encode bppUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBppUri: " <> bppUriText)

withTransactionIdLogTag :: (Log m) => Text -> m a -> m a
withTransactionIdLogTag = withTransactionIdLogTag'

getContextBapId :: MonadFlow m => Spec.Context -> m Text
getContextBapId context = do
  context.contextBapId & fromMaybeM (InvalidRequest "Missing contextBapId")

mkBppUri ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Text ->
  m BaseUrl
mkBppUri merchantId =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId)

castVariant :: Variant.VehicleVariant -> (Text, Text)
castVariant Variant.SEDAN = (show Enums.CAB, "SEDAN")
castVariant Variant.HATCHBACK = (show Enums.CAB, "HATCHBACK")
castVariant Variant.SUV = (show Enums.CAB, "SUV")
castVariant Variant.AUTO_RICKSHAW = (show Enums.AUTO_RICKSHAW, "AUTO_RICKSHAW")
castVariant Variant.TAXI = (show Enums.CAB, "TAXI")
castVariant Variant.TAXI_PLUS = (show Enums.CAB, "TAXI_PLUS")
castVariant Variant.PREMIUM_SEDAN = (show Enums.CAB, "PREMIUM_SEDAN")
castVariant Variant.BLACK = (show Enums.CAB, "BLACK")
castVariant Variant.BLACK_XL = (show Enums.CAB, "BLACK_XL")
castVariant Variant.BIKE = (show Enums.TWO_WHEELER, "BIKE")
castVariant Variant.DELIVERY_BIKE = (show Enums.TWO_WHEELER, "DELIVERY_BIKE")
castVariant Variant.AMBULANCE_TAXI = (show Enums.AMBULANCE, "AMBULANCE_TAXI")
castVariant Variant.AMBULANCE_TAXI_OXY = (show Enums.AMBULANCE, "AMBULANCE_TAXI_OXY")
castVariant Variant.AMBULANCE_AC = (show Enums.AMBULANCE, "AMBULANCE_AC")
castVariant Variant.AMBULANCE_AC_OXY = (show Enums.AMBULANCE, "AMBULANCE_AC_OXY")
castVariant Variant.AMBULANCE_VENTILATOR = (show Enums.AMBULANCE, "AMBULANCE_VENTILATOR")
castVariant Variant.SUV_PLUS = (show Enums.CAB, "SUV_PLUS")
castVariant Variant.HERITAGE_CAB = (show Enums.CAB, "HERITAGE_CAB")
castVariant Variant.EV_AUTO_RICKSHAW = (show Enums.AUTO_RICKSHAW, "EV_AUTO_RICKSHAW")
castVariant Variant.DELIVERY_LIGHT_GOODS_VEHICLE = (show Enums.TRUCK, "DELIVERY_LIGHT_GOODS_VEHICLE")
castVariant Variant.DELIVERY_TRUCK_MINI = (show Enums.TRUCK, "DELIVERY_TRUCK_MINI")
castVariant Variant.DELIVERY_TRUCK_SMALL = (show Enums.TRUCK, "DELIVERY_TRUCK_SMALL")
castVariant Variant.DELIVERY_TRUCK_MEDIUM = (show Enums.TRUCK, "DELIVERY_TRUCK_MEDIUM")
castVariant Variant.DELIVERY_TRUCK_LARGE = (show Enums.TRUCK, "DELIVERY_TRUCK_LARGE")
castVariant Variant.DELIVERY_TRUCK_ULTRA_LARGE = (show Enums.TRUCK, "DELIVERY_TRUCK_ULTRA_LARGE")
castVariant Variant.BUS_NON_AC = (show Enums.BUS, "BUS_NON_AC")
castVariant Variant.BUS_AC = (show Enums.BUS, "BUS_AC")
castVariant Variant.BOAT = (show Enums.BOAT, "BOAT")
castVariant Variant.AUTO_PLUS = (show Enums.AUTO_RICKSHAW, "AUTO_PLUS")
castVariant Variant.VIP_ESCORT = (show Enums.CAB, "VIP_ESCORT")
castVariant Variant.VIP_OFFICER = (show Enums.CAB, "VIP_OFFICER")
castVariant Variant.AC_PRIORITY = (show Enums.CAB, "AC_PRIORITY")
castVariant Variant.BIKE_PLUS = (show Enums.TWO_WHEELER, "BIKE_PLUS")
castVariant Variant.E_RICKSHAW = (show Enums.TOTO, "E_RICKSHAW")
castVariant Variant.AUTO_LITE = (show Enums.AUTO_RICKSHAW, "AUTO_LITE")
castVariant Variant.PINK_AUTO = (show Enums.AUTO_RICKSHAW, "PINK_AUTO")

-- | Normalize quote breakup title to spec-compliant TRV:10 enum values
-- Spec allows: BASE_FARE, DISTANCE_FARE, REFUND, TOLL_CHARGES, PARKING_CHARGES, TAX, WAITING_CHARGES, DRIVER_BATA, NIGHT_CHARGES, BUYER_ADDITIONAL_AMOUNT
-- Note: CANCELLATION_CHARGES excluded (only valid in on_cancel), ADD_ONS excluded (not defined in on_search)
normalizeBreakupTitle :: Text -> Maybe Text
normalizeBreakupTitle title = case title of
  "BASE_FARE" -> Just "BASE_FARE"
  "DISTANCE_FARE" -> Just "DISTANCE_FARE"
  "CANCELLATION_CHARGES" -> Nothing -- Only valid in on_cancel, not in regular breakup
  "REFUND" -> Just "REFUND"
  "TOLL_CHARGES" -> Just "TOLL_CHARGES"
  "PARKING_CHARGES" -> Just "PARKING_CHARGES"
  "PARKING_CHARGE" -> Just "PARKING_CHARGES"
  "TAX" -> Just "TAX"
  "WAITING_CHARGES" -> Just "WAITING_CHARGES"
  "WAITING_OR_PICKUP_CHARGES" -> Just "WAITING_CHARGES"
  "DRIVER_BATA" -> Just "DRIVER_BATA"
  "DRIVER_ALLOWANCE" -> Just "DRIVER_BATA"
  "NIGHT_CHARGES" -> Just "NIGHT_CHARGES"
  "NIGHT_SHIFT_CHARGE" -> Just "NIGHT_CHARGES"
  "PICKUP_CHARGE" -> Just "DISTANCE_FARE"
  "DEAD_KILOMETER_FARE" -> Just "DISTANCE_FARE"
  "EXTRA_DISTANCE_FARE" -> Just "DISTANCE_FARE"
  "DIST_BASED_FARE" -> Just "DISTANCE_FARE"
  "TIME_BASED_FARE" -> Just "DISTANCE_FARE"
  "SGST" -> Just "TAX"
  "CGST" -> Just "TAX"
  "RIDE_VAT" -> Just "TAX"
  "TOLL_VAT" -> Just "TAX"
  "BUYER_ADDITIONAL_AMOUNT" -> Just "BUYER_ADDITIONAL_AMOUNT"
  "ADD_ONS" -> Nothing -- ADD_ONS not defined in on_search, filter out
  "CUSTOMER_SELECTED_FARE" -> Just "BUYER_ADDITIONAL_AMOUNT"
  "DRIVER_SELECTED_FARE" -> Just "DRIVER_BATA"
  "SERVICE_CHARGE" -> Just "BASE_FARE"
  "CONGESTION_CHARGE" -> Just "BASE_FARE"
  "RETURN_FEE" -> Just "BASE_FARE"
  "BOOTH_CHARGE" -> Just "BASE_FARE"
  "LUGGAGE_CHARGE" -> Just "BASE_FARE"
  "SAFETY_PLUS_CHARGES" -> Just "BASE_FARE"
  "PER_STOP_CHARGES" -> Just "BASE_FARE"
  "RIDE_STOP_CHARGES" -> Just "BASE_FARE"
  "INSURANCE_CHARGES" -> Just "TAX"
  "PLATFORM_FEE" -> Just "BASE_FARE"
  "CARD_CHARGES_ON_FARE" -> Just "TAX"
  "CARD_CHARGES_FIXED" -> Just "TAX"
  "RIDE_DURATION_FARE" -> Just "DISTANCE_FARE"
  "FIXED_GOVERNMENT_RATE" -> Just "BASE_FARE"
  "TOTAL_FARE" -> Nothing -- Exclude total fare from breakup
  "EXTRA_TIME_FARE" -> Just "WAITING_CHARGES"
  "PET_CHARGES" -> Just "BASE_FARE"
  "PRIORITY_CHARGES" -> Just "BASE_FARE"
  "BUSINESS_DISCOUNT" -> Just "BASE_FARE"
  "PERSONAL_DISCOUNT" -> Just "BASE_FARE"
  "NO_CHARGES" -> Nothing
  _ -> Just "BASE_FARE" -- Map unknown charges to BASE_FARE to avoid price mismatch

rationaliseMoney :: Money -> Text
rationaliseMoney = OS.valueToString . OS.DecimalValue . toRational

castDPaymentType :: DMPM.PaymentType -> Text
castDPaymentType DMPM.ON_FULFILLMENT = show Enums.ON_FULFILLMENT
castDPaymentType DMPM.POSTPAID = show Enums.ON_FULFILLMENT

parseVehicleVariant :: Maybe Text -> Maybe Text -> Maybe Variant.VehicleVariant
parseVehicleVariant mbCategory mbVariant = case (mbCategory, mbVariant) of
  (Just "CAB", Just "SEDAN") -> Just Variant.SEDAN
  (Just "CAB", Just "SUV") -> Just Variant.SUV
  (Just "CAB", Just "HATCHBACK") -> Just Variant.HATCHBACK
  (Just "CAB", Just "PREMIUM_SEDAN") -> Just Variant.PREMIUM_SEDAN
  (Just "CAB", Just "BLACK") -> Just Variant.BLACK
  (Just "CAB", Just "SUV_PLUS") -> Just Variant.SUV_PLUS
  (Just "CAB", Just "BLACK_XL") -> Just Variant.BLACK_XL
  (Just "CAB", Just "HERITAGE_CAB") -> Just Variant.HERITAGE_CAB
  (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just Variant.AUTO_RICKSHAW
  (Just "AUTO_RICKSHAW", Just "EV_AUTO_RICKSHAW") -> Just Variant.EV_AUTO_RICKSHAW
  (Just "AUTO_RICKSHAW", Just "AUTO_PLUS") -> Just Variant.AUTO_PLUS
  (Just "CAB", Just "TAXI") -> Just Variant.TAXI
  (Just "CAB", Just "TAXI_PLUS") -> Just Variant.TAXI_PLUS
  (Just "MOTORCYCLE", Just "BIKE") -> Just Variant.BIKE -- becomes redundant, TODO : remove in next release
  (Just "MOTORCYCLE", Just "DELIVERY_BIKE") -> Just Variant.DELIVERY_BIKE -- becomes redundant, TODO : remove in next release
  (Just "TWO_WHEELER", Just "BIKE") -> Just Variant.BIKE
  (Just "TWO_WHEELER", Just "DELIVERY_BIKE") -> Just Variant.DELIVERY_BIKE
  (Just "AMBULANCE", Just "AMBULANCE_TAXI") -> Just Variant.AMBULANCE_TAXI
  (Just "AMBULANCE", Just "AMBULANCE_TAXI_OXY") -> Just Variant.AMBULANCE_TAXI_OXY
  (Just "AMBULANCE", Just "AMBULANCE_AC") -> Just Variant.AMBULANCE_AC
  (Just "AMBULANCE", Just "AMBULANCE_AC_OXY") -> Just Variant.AMBULANCE_AC_OXY
  (Just "AMBULANCE", Just "AMBULANCE_VENTILATOR") -> Just Variant.AMBULANCE_VENTILATOR
  (Just "TRUCK", Just "DELIVERY_LIGHT_GOODS_VEHICLE") -> Just Variant.DELIVERY_LIGHT_GOODS_VEHICLE
  (Just "TRUCK", Just "DELIVERY_TRUCK_MINI") -> Just Variant.DELIVERY_TRUCK_MINI
  (Just "TRUCK", Just "DELIVERY_TRUCK_SMALL") -> Just Variant.DELIVERY_TRUCK_SMALL
  (Just "TRUCK", Just "DELIVERY_TRUCK_MEDIUM") -> Just Variant.DELIVERY_TRUCK_MEDIUM
  (Just "TRUCK", Just "DELIVERY_TRUCK_LARGE") -> Just Variant.DELIVERY_TRUCK_LARGE
  (Just "TRUCK", Just "DELIVERY_TRUCK_ULTRA_LARGE") -> Just Variant.DELIVERY_TRUCK_ULTRA_LARGE
  (Just "BUS", Just "BUS_NON_AC") -> Just Variant.BUS_NON_AC
  (Just "BUS", Just "BUS_AC") -> Just Variant.BUS_AC
  (Just "CAB", Just "VIP_ESCORT") -> Just Variant.VIP_ESCORT
  (Just "CAB", Just "VIP_OFFICER") -> Just Variant.VIP_OFFICER
  (Just "CAB", Just "AC_PRIORITY") -> Just Variant.AC_PRIORITY
  (Just "TWO_WHEELER", Just "BIKE_PLUS") -> Just Variant.BIKE_PLUS
  (Just "MOTORCYCLE", Just "BIKE_PLUS") -> Just Variant.BIKE_PLUS
  (Just "TOTO", Just "E_RICKSHAW") -> Just Variant.E_RICKSHAW
  (Just "AUTO_RICKSHAW", Just "AUTO_LITE") -> Just Variant.AUTO_LITE
  (Just "AUTO_RICKSHAW", Just "PINK_AUTO") -> Just Variant.PINK_AUTO
  _ -> Nothing

parseAddress :: MonadFlow m => Spec.Location -> m (Maybe DL.LocationAddress)
parseAddress Spec.Location {..} = do
  let areaCode = locationAreaCode
  let city' = locationCity >>= (.cityName)
  let state' = locationState >>= (.stateName)
  let country' = locationCountry >>= (.countryName)
  case locationAddress of
    Nothing ->
      pure . Just $
        DL.LocationAddress
          { area = Nothing,
            areaCode = locationAreaCode,
            building = Nothing,
            city = locationCity >>= (.cityName),
            country = locationCountry >>= (.countryName),
            door = Nothing,
            extras = Nothing,
            fullAddress = Nothing,
            instructions = Nothing,
            state = locationState >>= (.stateName),
            street = Nothing
          }
    Just locationAddress' -> do
      address@OS.Address {..} <- buildAddressFromText locationAddress'
      let fullAddress = mkFullAddress address
      pure $
        Just $
          DL.LocationAddress
            { area = ward, -- TODO: Fetch this, discuss with ONDC
              city = city',
              state = state',
              country = country',
              instructions = Nothing,
              extras = Nothing,
              ..
            }
  where
    mkFullAddress OS.Address {..} = do
      let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, city, state, area_code, country]
      if null strictFields
        then Nothing
        else Just $ T.intercalate ", " strictFields
    -- mkFullAddress city state country = do
    --   let strictFields = catMaybes $ filter (not . isEmpty) [locationAddress, city, state, country]
    --   if null strictFields
    --     then Nothing
    --     else Just $ T.intercalate ", " strictFields

    isEmpty :: Maybe Text -> Bool
    isEmpty = maybe True (T.null . T.replace " " "")

mkStops' :: DLoc.Location -> Maybe DLoc.Location -> [DLoc.Location] -> Maybe Text -> Maybe [Spec.Stop]
mkStops' origin mbDestination intermediateStops mAuthorization =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps dest = Gps.Gps {lat = dest.lat, lon = dest.lon}
   in Just $
        catMaybes
          [ Just $
              emptyStop
                { Spec.stopLocation =
                    Just $
                      emptyLocation
                        { Spec.locationAddress = Nothing,
                          Spec.locationAreaCode = Nothing,
                          Spec.locationCity = Nothing,
                          Spec.locationCountry = Nothing,
                          Spec.locationGps = Utils.gpsToText originGps,
                          Spec.locationState = Nothing
                        },
                  Spec.stopType = Just $ show Enums.START,
                  Spec.stopAuthorization = mAuthorization >>= mkAuthorization,
                  Spec.stopId = Nothing
                },
            ( \destination ->
                emptyStop
                  { Spec.stopLocation =
                      Just $
                        emptyLocation
                          { Spec.locationAddress = Nothing,
                            Spec.locationAreaCode = Nothing,
                            Spec.locationCity = Nothing,
                            Spec.locationCountry = Nothing,
                            Spec.locationGps = Utils.gpsToText $ destinationGps destination,
                            Spec.locationState = Nothing
                          },
                    Spec.stopType = Just $ show Enums.END,
                    Spec.stopId = Nothing,
                    Spec.stopParentStopId = Nothing
                  }
            )
              <$> mbDestination
          ]
          <> (map (\(location, order) -> mkIntermediateStop location order (order - 1)) $ zip intermediateStops [1 ..])
  where
    mkAuthorization :: Text -> Maybe Spec.Authorization
    mkAuthorization auth =
      Just $
        Spec.Authorization
          { authorizationStatus = Just "UNCLAIMED",
            authorizationToken = Just auth,
            authorizationType = Just $ show Enums.OTP,
            authorizationValidTo = Just "2099-12-31T23:59:59Z"
          }

mkAddress :: DLoc.LocationAddress -> Text
mkAddress DLoc.LocationAddress {..} =
  let res = map replaceEmpty [door, building, street, area, city, state, country]
      addr = T.intercalate ", " $ catMaybes res
   in if T.null addr then fromMaybe "India" (fullAddress <|> area <|> city <|> state <|> country) else addr

mkIntermediateStop :: DLoc.Location -> Int -> Int -> Spec.Stop
mkIntermediateStop stop _id _parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in emptyStop
        { Spec.stopLocation =
            Just $
              emptyLocation
                { Spec.locationAddress = Nothing,
                  Spec.locationAreaCode = Nothing,
                  Spec.locationCity = Nothing,
                  Spec.locationCountry = Nothing,
                  Spec.locationGps = Utils.gpsToText gps,
                  Spec.locationState = Nothing,
                  Spec.locationId = Just stop.id.getId
                },
          Spec.stopType = Just $ show Enums.INTERMEDIATE_STOP,
          Spec.stopId = Nothing,
          Spec.stopParentStopId = Nothing
        }

mkIntermediateStopSearch :: Maps.LatLong -> Int -> Int -> Spec.Stop
mkIntermediateStopSearch stop _id _parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in emptyStop
        { Spec.stopLocation =
            Just $ emptyLocation {Spec.locationGps = Utils.gpsToText gps},
          Spec.stopType = Just $ show Enums.INTERMEDIATE_STOP,
          Spec.stopId = Nothing,
          Spec.stopParentStopId = Nothing
        }

data DriverInfo = DriverInfo
  { mobileNumber :: Text,
    alternateMobileNumber :: Maybe Text,
    name :: Text,
    tags :: Maybe [Spec.TagGroup]
  }

showVariant :: Variant.VehicleVariant -> Maybe Text
showVariant = A.decode . A.encode

-- common for on_update & on_status
mkStopsOUS :: DBooking.Booking -> DRide.Ride -> Text -> Maybe Text -> Maybe [Spec.Stop]
mkStopsOUS booking ride rideOtp mbEvent =
  let origin = booking.fromLocation
      mbDestination = booking.toLocation
      intermediateStops = booking.stops
      originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps dest = Gps.Gps {lat = dest.lat, lon = dest.lon}
      -- Use fulfillment event to determine auth status (handles race condition with ride.status)
      isRideStartedOrEnded = case mbEvent of
        Just event -> event == show Enums.RIDE_STARTED || event == show Enums.RIDE_ENDED
        Nothing -> False
      authStatus = if ride.status == DRide.INPROGRESS || ride.status == DRide.COMPLETED || isRideStartedOrEnded
                   then "CLAIMED" else "UNCLAIMED"
   in Just $
        catMaybes
          [ Just $
              emptyStop
                { Spec.stopLocation =
                    Just $
                      emptyLocation
                        { Spec.locationAddress = Nothing,
                          Spec.locationAreaCode = Nothing,
                          Spec.locationCity = Nothing,
                          Spec.locationCountry = Nothing,
                          Spec.locationGps = Utils.gpsToText originGps,
                          Spec.locationState = Nothing
                        },
                  Spec.stopType = Just $ show Enums.START,
                  Spec.stopId = Nothing,
                  Spec.stopAuthorization =
                    Just $
                      Spec.Authorization
                        { authorizationStatus = Just authStatus,
                          authorizationToken = Just rideOtp,
                          authorizationType = Just $ show Enums.OTP,
                          authorizationValidTo = Just "2099-12-31T23:59:59Z"
                        },
                  Spec.stopTime = Nothing
                },
            ( \destination ->
                emptyStop
                  { Spec.stopLocation =
                      Just $
                        emptyLocation
                          { Spec.locationAddress = Nothing,
                            Spec.locationAreaCode = Nothing,
                            Spec.locationCity = Nothing,
                            Spec.locationCountry = Nothing,
                            Spec.locationGps = Utils.gpsToText (destinationGps destination),
                            Spec.locationState = Nothing
                          },
                    Spec.stopType = Just $ show Enums.END,
                    Spec.stopTime = Nothing,
                    Spec.stopId = Nothing,
                    Spec.stopParentStopId = Nothing
                  }
            )
              <$> mbDestination
          ]
          <> (map (\(location, order) -> mkIntermediateStop location order (order - 1)) $ zip intermediateStops [1 ..])

type IsValueAddNP = Bool

-- common for on_update & on_status
mkFulfillmentV2 ::
  (MonadFlow m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Maybe SP.Person ->
  Maybe DDriverStats.DriverStats ->
  DRide.Ride ->
  DBooking.Booking ->
  Maybe DVeh.Vehicle ->
  Maybe Text ->
  Maybe [Spec.TagGroup] ->
  Maybe [Spec.TagGroup] ->
  Bool ->
  Bool ->
  Maybe Payment.AccountId ->
  Maybe Text ->
  IsValueAddNP ->
  Maybe Text ->
  Bool ->
  Int ->
  m Spec.Fulfillment
mkFulfillmentV2 mbDriver mbDriverStats ride booking mbVehicle mbImage _mbTags _mbPersonTags isDriverBirthDay isFreeRide driverAccountId mbEvent isValueAddNP riderPhone isAlreadyFav favCount = do
  mbDInfo <- driverInfo
  let rideOtp = fromMaybe ride.otp ride.endOtp
  -- Look up encoded polyline for ROUTE_INFO tags
  mbSearchReq <- B.runInReplica $ QSR.findByTransactionIdAndMerchantId booking.transactionId booking.providerId
  let mbPolyline = mbSearchReq >>= (.encodedPolyline)
      routeInfoTags = mkRouteInfoTagsFromPolyline mbPolyline
      -- Only ROUTE_INFO in fulfillment tags for ONDC spec compliance
      allTags = routeInfoTags
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just $ getBookingFulfillmentId booking,
        fulfillmentStops = mkStopsOUS booking ride rideOtp mbEvent,
        fulfillmentType = Just $ Utils.tripCategoryToFulfillmentType booking.tripCategory,
        fulfillmentAgent =
          Just $
            Spec.Agent
              { agentContact =
                  mbDInfo >>= \dInfo ->
                    Just $ Spec.Contact {contactPhone = Just dInfo.mobileNumber},
                agentPerson =
                  Just $
                    emptyPerson
                      { Spec.personImage =
                          mbImage <&> \mbImage' ->
                            emptyImage {Spec.imageUrl = Just mbImage'},
                        Spec.personName = mbDInfo >>= Just . (.name),
                        Spec.personTags = Nothing -- ONDC spec: no person tags in agent
                      }
              },
        fulfillmentVehicle =
          mbVehicle >>= \vehicle -> do
            let (category, variant) = castVariant vehicle.variant
            Just $
              emptyVehicle
                { Spec.vehicleColor = Just vehicle.color,
                  Spec.vehicleModel = Just vehicle.model,
                  Spec.vehicleRegistration = Just vehicle.registrationNo,
                  Spec.vehicleCategory = Just category,
                  Spec.vehicleVariant = Just variant,
                  Spec.vehicleCapacity = vehicle.capacity
                },
        fulfillmentCustomer = tfCustomer riderPhone booking.riderName,
        fulfillmentState = mkFulfillmentStateCode <$> mbEvent,
        fulfillmentTags = allTags
      }
  where
    driverInfo = forM (liftM2 (,) mbDriver mbDriverStats) $ \(driver, driverStats) -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dAlternatePhoneNum <- SP.getPersonAlternateNumber driver
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId ride.trackingUrl dAlternatePhoneNum isAlreadyFav favCount booking.isSafetyPlus
      pure $
        DriverInfo
          { mobileNumber = dPhoneNum,
            alternateMobileNumber = dAlternatePhoneNum,
            name = dName,
            tags = if isValueAddNP then dTags else Nothing
          }

tfCustomer :: Maybe Text -> Maybe Text -> Maybe Spec.Customer
tfCustomer riderPhone riderName =
  Just
    Spec.Customer
      { customerContact =
          Just Spec.Contact {contactPhone = riderPhone},
        customerPerson =
          Just $ emptyPerson {Spec.personName = riderName}
      }

mkDriverDetailsTags :: SP.Person -> DDriverStats.DriverStats -> Bool -> Bool -> Maybe Payment.AccountId -> BaseUrl -> Maybe Text -> Bool -> Int -> Bool -> Maybe [Spec.TagGroup]
mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId driverTrackingUrl driverAlternateNumber isAlreadyFav favCount isSafetyPlus =
  Tags.buildTagGroups
    [ Tags.REGISTERED_AT ~= show driver.createdAt,
      Tags.RATING ~=? (show <$> driverStats.rating),
      Tags.IS_DRIVER_BIRTHDAY ~=| (isDriverBirthDay, show isDriverBirthDay),
      Tags.IS_FREE_RIDE ~=| (isFreeRide, show isFreeRide),
      Tags.DRIVER_ACCOUNT_ID ~=? driverAccountId,
      Tags.DRIVER_TRACKING_URL ~= showBaseUrl driverTrackingUrl,
      Tags.DRIVER_ALTERNATE_NUMBER ~=? driverAlternateNumber,
      Tags.IS_ALREADY_FAVOURITE ~=| (isAlreadyFav, show isAlreadyFav),
      Tags.FAVOURITE_COUNT ~= show favCount,
      Tags.IS_SAFETY_PLUS ~=| (isSafetyPlus, show isSafetyPlus)
    ]

mkLocationTagGroupV2 :: Maybe Maps.LatLong -> Maybe [Spec.TagGroup]
mkLocationTagGroupV2 location' =
  location' >>= \location ->
    Tags.buildTagGroups
      [ Tags.CURRENT_LOCATION_LAT ~= show location.lat,
        Tags.CURRENT_LOCATION_LON ~= show location.lon
      ]

mkArrivalTimeTagGroupV2 :: Maybe UTCTime -> Maybe [Spec.TagGroup]
mkArrivalTimeTagGroupV2 = Tags.mkSingleTagGroup Tags.ARRIVAL_TIME

mkEstimatedEndTimeRangeTagGroupV2 :: Maybe DRide.EstimatedEndTimeRange -> Maybe [Spec.TagGroup]
mkEstimatedEndTimeRangeTagGroupV2 estimatedEndTimeRange' =
  estimatedEndTimeRange' >>= \estimatedEndTimeRange ->
    Tags.buildTagGroups
      [ Tags.ESTIMATED_END_TIME_RANGE_START ~= show estimatedEndTimeRange.start,
        Tags.ESTIMATED_END_TIME_RANGE_END ~= show estimatedEndTimeRange.end
      ]

mkParcelImageUploadedTag :: Maybe [Spec.TagGroup]
mkParcelImageUploadedTag =
  Tags.buildTagGroups
    [ Tags.PARCEL_IMAGE_UPLOADED ~= "True"
    ]

mkVehicleTags :: Maybe Double -> Maybe Bool -> Maybe [Spec.TagGroup]
mkVehicleTags vehicleServiceTierAirConditioned' isAirConditioned =
  vehicleServiceTierAirConditioned' >>= \vehicleServiceTierAirConditioned ->
    Tags.buildTagGroups
      [ Tags.IS_AIR_CONDITIONED ~= show vehicleServiceTierAirConditioned,
        Tags.IS_AIR_CONDITIONED_VEHICLE ~=? (show <$> isAirConditioned)
      ]

-- | Build ROUTE_INFO tags from pre-encoded polyline (for on_select/on_init/on_confirm/on_status)
mkRouteInfoTagsFromPolyline :: Maybe Text -> Maybe [Spec.TagGroup]
mkRouteInfoTagsFromPolyline Nothing = Nothing
mkRouteInfoTagsFromPolyline (Just polyline) =
  let waypoints = KEPP.decode polyline
      waypointsJson = TL.toStrict (TLE.decodeUtf8 (A.encode waypoints))
   in Tags.buildTagGroups
        [ Tags.ENCODED_POLYLINE ~= polyline,
          Tags.WAYPOINTS ~= waypointsJson
        ]

mkRouteInfoTags :: Maybe [Maps.LatLong] -> Maybe [Spec.TagGroup]
mkRouteInfoTags Nothing = Nothing
mkRouteInfoTags (Just points) =
  Tags.buildTagGroups
    [ Tags.ENCODED_POLYLINE ~= KEPP.encode points,
      Tags.WAYPOINTS ~= TL.toStrict (TLE.decodeUtf8 (A.encode points))
    ]

mkOdometerTagGroupV2 :: Maybe Centesimal -> Maybe [Spec.TagGroup]
mkOdometerTagGroupV2 = Tags.mkSingleTagGroup Tags.START_ODOMETER_READING

mkTollConfidenceTagGroupV2 :: Maybe Confidence -> Maybe [Spec.TagGroup]
mkTollConfidenceTagGroupV2 = Tags.mkSingleTagGroup Tags.TOLL_CONFIDENCE

mkRideDetailsTagGroup :: Maybe Bool -> Maybe [Spec.TagGroup]
mkRideDetailsTagGroup = Tags.mkSingleTagGroup Tags.IS_VALID_RIDE

mkVehicleAgeTagGroupV2 :: Maybe Months -> Maybe [Spec.TagGroup]
mkVehicleAgeTagGroupV2 = Tags.mkSingleTagGroup Tags.VEHICLE_AGE

buildAddressFromText :: MonadFlow m => Text -> m OS.Address
buildAddressFromText fullAddress = do
  let splitedAddress = T.splitOn ", " fullAddress
      totalAddressComponents = List.length splitedAddress
  logDebug $ "Search Address:-" <> fullAddress
  if totalAddressComponents == 1
    then do
      let addr = OS.Address {area_code = Nothing, building = Nothing, city = Nothing, country = Nothing, door = Nothing, locality = Nothing, state = Nothing, street = Nothing, ward = Just fullAddress}
      logDebug $ "Parsed Single-Component Address Entity : " <> show addr
      pure addr
    else do
      let area_code_ = Nothing
          building_ = splitedAddress !? (totalAddressComponents - 6)
          city_ = splitedAddress !? (totalAddressComponents - 3)
          country_ = splitedAddress !? (totalAddressComponents - 1)
          door_ =
            if totalAddressComponents > 7
              then splitedAddress !? 0 <> Just ", " <> splitedAddress !? 1
              else splitedAddress !? 0
          locality_ = splitedAddress !? (totalAddressComponents - 4)
          state_ = splitedAddress !? (totalAddressComponents - 2)
          street_ = splitedAddress !? (totalAddressComponents - 5)
          building = replaceEmpty building_
          street = replaceEmpty street_
          locality = replaceEmpty locality_
          ward_ = Just $ T.intercalate ", " $ catMaybes [locality, street, building]
          ward = if ward_ == Just "" then city_ else ward_
          addr =
            OS.Address
              { area_code = area_code_,
                building = building_,
                city = city_,
                country = country_,
                door = door_,
                locality = locality_,
                state = state_,
                street = street_,
                ward = ward
              }
      logDebug $ "Parsed Address Entity: " <> show addr
      pure addr

(!?) :: [a] -> Int -> Maybe a
(!?) xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just $ xs List.!! i

replaceEmpty :: Maybe Text -> Maybe Text
replaceEmpty string = if string == Just "" then Nothing else string

mapRideStatus :: Maybe DRide.RideStatus -> Enums.FulfillmentState
mapRideStatus rideStatus =
  case rideStatus of
    Just DRide.UPCOMING -> Enums.SCHEDULED_RIDE_ASSIGNED
    Just DRide.NEW -> Enums.RIDE_ASSIGNED
    Just DRide.INPROGRESS -> Enums.RIDE_STARTED
    Just DRide.COMPLETED -> Enums.RIDE_ENDED
    Just DRide.CANCELLED -> Enums.RIDE_CANCELLED
    Nothing -> Enums.RIDE_ASSIGNED

tfCancellationFee :: Maybe Common.PriceAPIEntity -> Maybe Spec.Fee
tfCancellationFee Nothing =
  Just
    Spec.Fee
      { feeAmount =
          Just
            emptyPrice
              { Spec.priceCurrency = Just "INR",
                Spec.priceValue = Just "0"
              },
        feePercentage = Just "0"
      }
tfCancellationFee (Just price) = do
  Just
    Spec.Fee
      { feeAmount = mkPrice,
        feePercentage = Nothing
      }
  where
    mkPrice =
      Just
        emptyPrice
          { Spec.priceCurrency = Just $ show price.currency,
            Spec.priceValue = Just $ encodeToText price.amount
          }

tfFulfillmentState :: Enums.FulfillmentState -> Maybe Spec.FulfillmentState
tfFulfillmentState = Just . mkFulfillmentState

tfQuotation :: DBooking.Booking -> Maybe Spec.Quotation
tfQuotation booking =
  Just
    emptyQuotation
      { Spec.quotationBreakup = mkQuotationBreakup booking.estimatedFare booking.fareParams,
        Spec.quotationPrice = tfQuotationPrice (HighPrecMoney $ toRational booking.estimatedFare) booking.currency
      }

tfQuotationSU :: DFParams.FareParameters -> HighPrecMoney -> Maybe Spec.Quotation
tfQuotationSU fareParams estimatedFare =
  Just
    emptyQuotation
      { Spec.quotationBreakup = mkQuotationBreakup estimatedFare fareParams,
        Spec.quotationPrice = tfQuotationPrice estimatedFare fareParams.currency
      }

tfQuotationPrice :: HighPrecMoney -> Currency -> Maybe Spec.Price
tfQuotationPrice estimatedFare currency =
  let roundedFare = Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just currency) estimatedFare
   in Just
        emptyPrice
          { Spec.priceCurrency = Just $ show currency,
            Spec.priceOfferedValue = Nothing,
            Spec.priceValue = Just roundedFare
          }

mkQuotationBreakup :: HighPrecMoney -> DFParams.FareParameters -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup estimatedFare fareParams =
  let fareParameters = mkFareParamsBreakups (\x -> x) (\title money -> (title, money)) fareParams
      normalized = mapMaybe normalizeMoneyBreakup fareParameters
   in Just $ aggregateBreakupsWithTotal (Just estimatedFare) fareParams.currency normalized
  where
    normalizeMoneyBreakup :: (Text, HighPrecMoney) -> Maybe (Text, HighPrecMoney)
    normalizeMoneyBreakup (title, money) = do
      normalizedTitle <- normalizeBreakupTitle title
      Just (normalizedTitle, money)

-- | Aggregate breakup items by title, adjust BASE_FARE so total matches expectedTotal
aggregateBreakups :: Currency -> [(Text, HighPrecMoney)] -> [Spec.QuotationBreakupInner]
aggregateBreakups = aggregateBreakupsWithTotal Nothing

-- | Aggregate with optional expected total — adjusts BASE_FARE to match
aggregateBreakupsWithTotal :: Maybe HighPrecMoney -> Currency -> [(Text, HighPrecMoney)] -> [Spec.QuotationBreakupInner]
aggregateBreakupsWithTotal mbExpectedTotal currency items =
  let grouped = Data.Map.Strict.fromListWith (+) items
      breakupTotal = Data.Map.Strict.foldl' (+) 0 grouped
      adjusted = case mbExpectedTotal of
        Just expectedTotal | expectedTotal /= breakupTotal ->
          let diff = expectedTotal - breakupTotal
           in Data.Map.Strict.adjust (+ diff) "BASE_FARE" grouped
        _ -> grouped
   in map (mkBreakupFromAggregate currency) (Data.Map.Strict.toList adjusted)

mkBreakupFromAggregate :: Currency -> (Text, HighPrecMoney) -> Spec.QuotationBreakupInner
mkBreakupFromAggregate currency (title, money) =
  let rounded = Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just currency) money
   in Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice =
            Just
              emptyPrice
                { Spec.priceCurrency = Just $ show currency,
                  Spec.priceOfferedValue = Nothing,
                  Spec.priceValue = Just rounded
                },
          quotationBreakupInnerTitle = Just title
        }

type MerchantShortId = Text

-- | Map TripCategory to ONDC 2.1.0 category descriptor code
tripCategoryToCategoryCode :: DT.TripCategory -> Text
tripCategoryToCategoryCode = tripCategoryToCategoryCodeWithSchedule False

tripCategoryToCategoryCodeWithSchedule :: Bool -> DT.TripCategory -> Text
tripCategoryToCategoryCodeWithSchedule isScheduled = \case
  DT.Rental _ -> if isScheduled then "SCHEDULED_RENTAL" else "ON_DEMAND_RENTAL"
  _ -> if isScheduled then "SCHEDULED_TRIP" else "ON_DEMAND_TRIP"

tfItems :: DBooking.Booking -> MerchantShortId -> Maybe Meters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> Maybe [Spec.Item]
tfItems booking shortId _estimatedDistance mbFarePolicy mbPaymentId =
  let farePolicyTags = maybeToList (mkSpecFarePolicyTagsFromPolicy mbFarePolicy)
      infoTags = fromMaybe [] (mkInfoTagGroup booking.distanceToPickup booking.dqDurationToPickup)
      featureList = mkFeatureListTags booking.isAirConditioned
      combinedTags = Just $ farePolicyTags <> infoTags <> [featureList]
   in Just
        [ emptyItem
            { Spec.itemDescriptor = tfItemDescriptor booking,
              Spec.itemFulfillmentIds = Just [getBookingFulfillmentId booking],
              Spec.itemId = Just $ maybe (Common.mkItemId shortId booking.vehicleServiceTier) getId (booking.estimateId),
              Spec.itemCategoryIds = Just [tripCategoryToCategoryCode booking.tripCategory],
              Spec.itemPaymentIds = tfPaymentId mbPaymentId,
              Spec.itemPrice = tfItemPrice booking.estimatedFare booking.currency,
              Spec.itemTags = combinedTags
            }
        ]

tfItemsSoftUpdate :: DBooking.Booking -> MerchantShortId -> Maybe HighPrecMeters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> DBUR.BookingUpdateRequest -> Text -> Maybe [Spec.Item]
tfItemsSoftUpdate booking shortId estimatedDistance mbFarePolicy mbPaymentId updatedBooking rideId = do
  let estimatedDistance' = maybe Nothing (\dist -> Just $ highPrecMetersToMeters dist) estimatedDistance
  Just
    [ emptyItem
        { Spec.itemDescriptor = tfItemDescriptor booking,
          Spec.itemFulfillmentIds = Just [rideId],
          Spec.itemId = Just $ Common.mkItemId shortId booking.vehicleServiceTier,
          Spec.itemCategoryIds = Just [tripCategoryToCategoryCode booking.tripCategory],
          Spec.itemPaymentIds = tfPaymentId mbPaymentId,
          Spec.itemPrice = tfItemPrice updatedBooking.estimatedFare booking.currency,
          Spec.itemTags = mkRateCardTag estimatedDistance' booking.fareParams.customerCancellationDues Nothing booking.estimatedFare booking.fareParams.congestionChargeViaDp mbFarePolicy Nothing Nothing Nothing
        }
    ]

tfPaymentId :: Maybe Text -> Maybe [Text]
tfPaymentId mbPaymentId = do
  paymentId <- mbPaymentId
  Just [paymentId]

tfItemPrice :: HighPrecMoney -> Currency -> Maybe Spec.Price
tfItemPrice estimatedFare currency =
  Just
    emptyPrice
      { Spec.priceCurrency = Just $ show currency,
        Spec.priceOfferedValue = Nothing,
        Spec.priceValue = Just $ Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice Nothing estimatedFare -- Sending Nothing here becuase priceCurrency in hardcoded to INR, TODO: make this logic dynamic based on country
      }

-- | Build INFO tag group with DISTANCE_TO_NEAREST_DRIVER_METER and ETA_TO_NEAREST_DRIVER_MIN
mkInfoTagGroup :: Maybe Meters -> Maybe Seconds -> Maybe [Spec.TagGroup]
mkInfoTagGroup mbDistanceToPickup mbDurationToPickup =
  let distanceMeters = maybe "0" (show . (.getMeters)) mbDistanceToPickup
      etaMinutes = maybe "0" (\d -> show (d.getSeconds `div` 60)) mbDurationToPickup
   in Just
        [ Tags.getFullTagGroup
            Tags.GENERAL_INFO
            [ Tags.getFullTag Tags.DISTANCE_TO_NEAREST_DRIVER_METER (Just distanceMeters),
              Tags.getFullTag Tags.ETA_TO_NEAREST_DRIVER_MIN (Just etaMinutes)
            ]
        ]

-- | Build FEATURE_LIST tag group for items.tags
-- AC/NON_AC is dynamic based on vehicle air conditioning
mkFeatureListTags :: Maybe Bool -> Spec.TagGroup
mkFeatureListTags = mkFeatureListTagsWithAC

mkFeatureListTagsForVariant :: Variant.VehicleVariant -> Spec.TagGroup
mkFeatureListTagsForVariant variant =
  let isAC = case variant of
        Variant.AUTO_RICKSHAW -> False
        Variant.BIKE -> False
        _ -> True -- CAB variants (SEDAN, SUV, HATCHBACK, etc.) are AC
   in mkFeatureListTagsWithAC (Just isAC)

mkFeatureListTagsWithAC :: Maybe Bool -> Spec.TagGroup
mkFeatureListTagsWithAC mbIsAirConditioned =
  let acTag = case mbIsAirConditioned of
        Just True -> "AC"
        _ -> "NON_AC"
      mkValueTag val = Spec.Tag
        { tagDescriptor = Nothing,
          tagDisplay = Nothing,
          tagValue = Just val
        }
   in Spec.TagGroup
        { tagGroupDescriptor = Just Spec.Descriptor {descriptorCode = Just "FEATURE_LIST", descriptorName = Just "Feature list", descriptorShortDesc = Nothing},
          tagGroupDisplay = Just False,
          tagGroupList = Just
            [ mkValueTag acTag,
              mkValueTag "LOW_PRICE",
              mkValueTag "DOORSTEP_PICK_UP"
            ]
        }

-- | Build disability tag groups for on_search items.tags (ONDC TRV10 purple tags)
-- These advertise the BPP's disability support capabilities
mkDisabilityTagGroups :: [Spec.TagGroup]
mkDisabilityTagGroups =
  let mkTagGroup code name tags =
        Spec.TagGroup
          { tagGroupDescriptor = Just Spec.Descriptor {descriptorCode = Just code, descriptorName = Just name, descriptorShortDesc = Nothing},
            tagGroupDisplay = Just False,
            tagGroupList = Just tags
          }
      mkTag code desc value =
        Spec.Tag
          { tagDescriptor = Just Spec.Descriptor {descriptorCode = Just code, descriptorName = Just desc, descriptorShortDesc = Nothing},
            tagDisplay = Just False,
            tagValue = Just value
          }
   in [ mkTagGroup "DISABILITY_VIS" "Vision Impairment"
          [ mkTag "VIS_LEVEL" "Level of Disability" "LOW",
            mkTag "VIS_SCREEN_READER_USAGE" "Screen Reader Usage" "OTHERS",
            mkTag "VIS_CANE_USAGE" "Cane Usage" "YES",
            mkTag "VIS_SPECIAL_REQUIREMENT" "Special Requirement" "None"
          ],
        mkTagGroup "DISABILITY_HEA" "Hearing Impairment"
          [ mkTag "HEA_READING_ABILITY" "Reading Ability" "FULL",
            mkTag "HEA_SIGN_LANGUAGE_PREFERENCE" "Sign Language Preference" "OTHERS",
            mkTag "HEA_SPECIAL_REQUIREMENT" "Special Requirement" "None"
          ],
        mkTagGroup "DISABILITY_MOB" "Mobility Impairment"
          [ mkTag "MOB_DISABILITY" "Disability Type" "LOCOMOTOR",
            mkTag "MOB_WHEELCHAIR_USAGE" "Wheelchair Usage" "YES",
            mkTag "MOB_CRUTCHES_USAGE" "Crutches Usage" "YES",
            mkTag "MOB_SPECIAL_REQUIREMENT" "Special Requirement" "None"
          ]
      ]

-- | Extract disability tags from incoming request items.tags and return them as-is for passthrough
extractDisabilityTags :: Maybe [Spec.TagGroup] -> [Spec.TagGroup]
extractDisabilityTags Nothing = []
extractDisabilityTags (Just tagGroups) =
  filter isDisabilityTagGroup tagGroups
  where
    isDisabilityTagGroup tg = case tg.tagGroupDescriptor >>= (.descriptorCode) of
      Just code -> T.isPrefixOf "DISABILITY_" code || code == "MENTAL"
      Nothing -> False

-- | Build FARE_POLICY tag group with only ONDC spec-required tags from FarePolicy
mkSpecFarePolicyTagsFromPolicy :: Maybe FarePolicyD.FarePolicy -> Maybe Spec.TagGroup
mkSpecFarePolicyTagsFromPolicy Nothing = Nothing
mkSpecFarePolicyTagsFromPolicy (Just fp) =
  let mkTag code value = Tags.getFullTag code (Just value)
      (minFare, minFareDistKm, perKmCharge, pickupCharge, waitingChargePerMin, nightMultiplier, nightStart, nightEnd) = case fp.farePolicyDetails of
        FarePolicyD.ProgressiveDetails det ->
          let perKm = case Data.List.NonEmpty.toList det.perExtraKmRateSections of
                (sec : _) -> show sec.perExtraKmRate
                _ -> "0"
              wc = case det.waitingChargeInfo of
                Just wci -> case wci.waitingCharge of
                  FarePolicyD.PerMinuteWaitingCharge amt -> show amt
                  FarePolicyD.ConstantWaitingCharge amt -> show amt
                Nothing -> "0"
              (nm, ns, ne) = case det.nightShiftCharge of
                Just (FarePolicyD.ProgressiveNightShiftCharge multiplier) ->
                  ( show multiplier,
                    maybe "00:00" (show . (.nightShiftStart)) fp.nightShiftBounds,
                    maybe "00:00" (show . (.nightShiftEnd)) fp.nightShiftBounds
                  )
                _ -> ("1", "00:00", "00:00")
           in ( show det.baseFare,
                show (det.baseDistance.getMeters `div` 1000),
                perKm,
                show det.deadKmFare,
                wc,
                nm,
                ns,
                ne
              )
        _ -> ("0", "0", "0", "0", "0", "1", "00:00", "00:00")
   in Just $
        Tags.getFullTagGroup
          Tags.FARE_POLICY
          [ mkTag Tags.MIN_FARE minFare,
            mkTag Tags.MIN_FARE_DISTANCE_KM minFareDistKm,
            mkTag Tags.PER_KM_CHARGE perKmCharge,
            mkTag Tags.PICKUP_CHARGE pickupCharge,
            mkTag Tags.WAITING_CHARGE_PER_MIN waitingChargePerMin,
            mkTag Tags.NIGHT_CHARGE_MULTIPLIER nightMultiplier,
            mkTag Tags.NIGHT_SHIFT_START_TIME nightStart,
            mkTag Tags.NIGHT_SHIFT_END_TIME nightEnd
          ]

tfItemDescriptor :: DBooking.Booking -> Maybe Spec.Descriptor
tfItemDescriptor booking =
  Just
    Spec.Descriptor
      { descriptorCode = Just "RIDE",
        descriptorShortDesc = Just $ show booking.vehicleServiceTier,
        descriptorName = Just $ show booking.vehicleServiceTier
      }

convertEstimateToPricing :: Maybe Text -> (DEst.Estimate, DVST.VehicleServiceTier, Maybe NearestDriverInfo, Maybe BaseUrl) -> Pricing
convertEstimateToPricing _specialLocationName (DEst.Estimate {..}, serviceTier, mbDriverLocations, vehicleIconUrl) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = maxFare,
      pricingMinFare = minFare,
      fulfillmentType = Utils.tripCategoryToFulfillmentType tripCategory,
      serviceTierName = serviceTier.name,
      serviceTierDescription = serviceTier.shortDescription,
      vehicleVariant = fromMaybe (Variant.castServiceTierToVariant vehicleServiceTier) (listToMaybe serviceTier.defaultForVehicleVariant), -- ideally this should not be empty
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      vehicleServiceTierSeatingCapacity = serviceTier.seatingCapacity,
      vehicleServiceTierAirConditioned = serviceTier.airConditionedThreshold,
      isAirConditioned = serviceTier.isAirConditioned,
      qar = mbActualQARFromLocGeohashDistance <|> mbActualQARFromLocGeohash <|> mbActualQARCity,
      ..
    }

convertQuoteToPricing :: Maybe Text -> (DQuote.Quote, DVST.VehicleServiceTier, Maybe NearestDriverInfo, Maybe BaseUrl) -> Pricing
convertQuoteToPricing specialLocationName (DQuote.Quote {..}, serviceTier, mbDriverLocations, vehicleIconUrl) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = estimatedFare,
      pricingMinFare = estimatedFare,
      estimatedDistance = distance,
      fareParams = Just fareParams,
      fulfillmentType = Utils.tripCategoryToFulfillmentType tripCategory,
      serviceTierName = serviceTier.name,
      serviceTierDescription = serviceTier.shortDescription,
      vehicleVariant = fromMaybe (Variant.castServiceTierToVariant vehicleServiceTier) (listToMaybe serviceTier.defaultForVehicleVariant), -- ideally this should not be empty
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      vehicleServiceTierSeatingCapacity = serviceTier.seatingCapacity,
      vehicleServiceTierAirConditioned = serviceTier.airConditionedThreshold,
      isAirConditioned = serviceTier.isAirConditioned,
      smartTipSuggestion = Nothing,
      smartTipReason = Nothing,
      tipOptions = Nothing,
      qar = Nothing,
      businessDiscount = fareParams.businessDiscount,
      personalDiscount = fareParams.personalDiscount,
      ..
    }

convertBookingToPricing :: DVST.VehicleServiceTier -> DBooking.Booking -> Pricing
convertBookingToPricing serviceTier DBooking.Booking {..} =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = estimatedFare,
      pricingMinFare = estimatedFare,
      tripCategory = tripCategory,
      fareParams = Just fareParams,
      farePolicy = Nothing,
      fulfillmentType = Utils.tripCategoryToFulfillmentType tripCategory,
      serviceTierName = serviceTier.name,
      serviceTierDescription = serviceTier.shortDescription,
      vehicleVariant = fromMaybe (Variant.castServiceTierToVariant vehicleServiceTier) (listToMaybe serviceTier.defaultForVehicleVariant), -- ideally this should not be empty
      distanceToNearestDriver = Nothing,
      isCustomerPrefferedSearchRoute = Nothing,
      isBlockedRoute = Nothing,
      specialLocationName = Nothing,
      vehicleIconUrl = Nothing,
      smartTipSuggestion = Nothing,
      smartTipReason = Nothing,
      tipOptions = Nothing,
      qar = Nothing,
      businessDiscount = fareParams.businessDiscount,
      personalDiscount = fareParams.personalDiscount,
      ..
    }

mkGeneralInfoTagGroup :: Pricing -> Bool -> Maybe Spec.TagGroup
mkGeneralInfoTagGroup pricing isValueAddNP =
  let guardVNP val = if isValueAddNP then val else Nothing
      mkOptTag tag val = Tags.getFullTag tag val <$ val
      tags =
        catMaybes
          [ mkOptTag Tags.SPECIAL_LOCATION_TAG pricing.specialLocationTag,
            mkOptTag Tags.SPECIAL_LOCATION_NAME pricing.specialLocationName,
            mkOptTag Tags.BUSINESS_DISCOUNT (guardVNP (show <$> pricing.businessDiscount)),
            mkOptTag Tags.PERSONAL_DISCOUNT (guardVNP (show <$> pricing.personalDiscount)),
            mkOptTag Tags.DISTANCE_TO_NEAREST_DRIVER_METER (show . double2Int . realToFrac <$> pricing.distanceToNearestDriver),
            mkOptTag Tags.IS_CUSTOMER_PREFFERED_SEARCH_ROUTE (guardVNP (show <$> pricing.isCustomerPrefferedSearchRoute)),
            mkOptTag Tags.IS_BLOCKED_SEARCH_ROUTE (guardVNP (show <$> pricing.isBlockedRoute)),
            mkOptTag Tags.TOLL_NAMES (guardVNP (show <$> pricing.tollNames)),
            mkOptTag Tags.TIP_OPTIONS (guardVNP (show <$> pricing.tipOptions)),
            mkOptTag Tags.DURATION_TO_NEAREST_DRIVER_MINUTES (guardVNP (getDuration pricing.distanceToNearestDriver 25)),
            mkOptTag Tags.SMART_TIP_SUGGESTION (guardVNP (show <$> pricing.smartTipSuggestion)),
            mkOptTag Tags.SMART_TIP_REASON (guardVNP pricing.smartTipReason),
            mkOptTag Tags.QAR (guardVNP (show <$> pricing.qar))
          ]
   in case tags of
        [] -> Nothing
        _ -> Just $ Tags.getFullTagGroup Tags.GENERAL_INFO tags
  where
    getDuration :: Maybe Meters -> Int -> Maybe Text
    getDuration distance avgSpeed
      | avgSpeed <= 0 = Nothing
      | distance == Just 0 = Just "60"
      | otherwise = do
        distance' <- distance
        let distanceInMeters = realToFrac @_ @Double distance'
            avgSpeedInMetersPerSec = realToFrac @_ @Double (avgSpeed * 5) / 18
            estimatedTimeTakenInSeconds :: Int = ceiling $ (distanceInMeters / avgSpeedInMetersPerSec)
        Just $ show estimatedTimeTakenInSeconds

mkRateCardTag :: Maybe Meters -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> Maybe FarePolicyD.FarePolicy -> Maybe Bool -> Maybe Params.FareParameters -> Maybe Double -> Maybe [Spec.TagGroup]
mkRateCardTag estimatedDistance mbCancellationCharge tollCharges estimatedFare congestionChargeViaDp farePolicy fareParametersInRateCard fareParams mbGovtChargesRate = do
  let farePolicyBreakups = maybe [] (mkFarePolicyBreakups Prelude.id mkRateCardBreakupItem estimatedDistance mbCancellationCharge tollCharges estimatedFare congestionChargeViaDp mbGovtChargesRate) farePolicy
      fareParamsBreakups =
        case fareParametersInRateCard of
          Just True -> maybe [] (mkFareParamsBreakups (\price -> show price) mkRateCardFareParamsBreakupItem) fareParams
          _ -> []
      filteredFareParamsBreakups = filter (not . findDup farePolicyBreakups) fareParamsBreakups
      combainedParams = farePolicyBreakups <> filteredFareParamsBreakups
      farePolicyBreakupsTags = buildRateCardTags <$> combainedParams
  Just [Tags.getFullTagGroup Tags.FARE_POLICY farePolicyBreakupsTags]
  where
    findDup :: [RateCardBreakupItem] -> RateCardBreakupItem -> Bool
    findDup [] _ = False
    findDup (farePolicyCard : nextFarePolicy) fareParameter =
      if farePolicyCard.title == fareParameter.title then True else findDup nextFarePolicy fareParameter

mkVehicleIconTag :: Maybe BaseUrl -> Maybe [Spec.TagGroup]
mkVehicleIconTag mbBaseUrl =
  mbBaseUrl <&> \baseUrl ->
    [ ( Tags.getFullTagGroup
          Tags.VEHICLE_INFO
          [ Tags.mkTag Tags.VEHICLE_ICON_URL (Just $ showBaseUrl baseUrl)
          ]
      )
        { Spec.tagGroupDisplay = Just False
        }
    ]

mkRateCardBreakupItem :: Text -> Text -> RateCardBreakupItem
mkRateCardBreakupItem = RateCardBreakupItem

mkRateCardFareParamsBreakupItem :: Text -> Text -> RateCardBreakupItem
mkRateCardFareParamsBreakupItem title value = RateCardBreakupItem {title = title <> "_FARE_PARAM", ..}

buildRateCardTags :: RateCardBreakupItem -> Spec.Tag
buildRateCardTags RateCardBreakupItem {..} =
  Spec.Tag
    { tagDisplay = Just False,
      tagDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just title,
              descriptorName = Just title,
              descriptorShortDesc = Nothing
            },
      tagValue = Just value
    }

tfCancellationTerms :: Maybe Common.PriceAPIEntity -> Maybe Enums.FulfillmentState -> [Spec.CancellationTerm]
tfCancellationTerms _cancellationFee _state =
  let mkAmountFee cur val = Just $ Spec.Fee {feeAmount = Just emptyPrice {Spec.priceCurrency = Just cur, Spec.priceValue = Just val}, feePercentage = Nothing}
   in [ Spec.CancellationTerm
          { cancellationTermCancellationFee = Just $ Spec.Fee {feeAmount = Nothing, feePercentage = Just "0"},
            cancellationTermFulfillmentState = Just $ mkFulfillmentState Enums.RIDE_ASSIGNED,
            cancellationTermReasonRequired = Just True
          },
        Spec.CancellationTerm
          { cancellationTermCancellationFee = mkAmountFee "INR" "30",
            cancellationTermFulfillmentState = Just $ mkFulfillmentState Enums.RIDE_ENROUTE_PICKUP,
            cancellationTermReasonRequired = Just True
          },
        Spec.CancellationTerm
          { cancellationTermCancellationFee = mkAmountFee "INR" "50",
            cancellationTermFulfillmentState = Just $ mkFulfillmentState Enums.RIDE_ARRIVED_PICKUP,
            cancellationTermReasonRequired = Just True
          },
        Spec.CancellationTerm
          { cancellationTermCancellationFee = Just $ Spec.Fee {feeAmount = Nothing, feePercentage = Just "100"},
            cancellationTermFulfillmentState = Just $ mkFulfillmentState Enums.RIDE_STARTED,
            cancellationTermReasonRequired = Just True
          }
      ]

tfPayments :: DBooking.Booking -> DM.Merchant -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments booking transporter bppConfig = do
  let mPrice = Just $ Common.mkPrice (Just booking.currency) booking.estimatedFare
  let mkParams :: Maybe DT.BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . List.singleton $ mkPayment (show transporter.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice booking.paymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

tfProvider :: DBC.BecknConfig -> Maybe Spec.Provider
tfProvider becknConfig =
  return $
    Spec.Provider
      { providerCategories = Nothing, -- populated at call site if categories are known
        providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just $ becknConfig.subscriberId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing,
        providerTags = Nothing
      }

-- | Build BPP_TERMS tag group with settlement details for order.tags / catalog.tags
-- For on_search (catalog level), use without settlement amount
-- For on_select/on_init (order level), includes SETTLEMENT_AMOUNT but no BAP_TERMS
-- For on_confirm/on_status/on_update, includes both BAP_TERMS and BPP_TERMS
mkBppTermsTags :: DBC.BecknConfig -> Maybe [Spec.TagGroup]
mkBppTermsTags = mkBppTermsTagsWithAmount Nothing

-- | For on_confirm/on_status/on_update — includes cached BAP_TERMS + BPP_TERMS
getCachedBapTerms :: (CacheFlow m r) => Text -> m (Maybe Spec.TagGroup)
getCachedBapTerms transactionId = Hedis.safeGet (bapTermsCacheKey transactionId)

-- | Build order tags with BAP_TERMS (pass cached BAP terms) + BPP_TERMS
mkOrderTagsWithBapTerms :: Maybe Spec.TagGroup -> Maybe HighPrecMoney -> DBC.BecknConfig -> Maybe [Spec.TagGroup]
mkOrderTagsWithBapTerms mbBapTerms mbOrderAmount becknConfig =
  let bppTerms = mkBppTermsTagsWithAmount mbOrderAmount becknConfig
   in case mbBapTerms of
        Just bapTerms -> Just $ [bapTerms] <> fromMaybe [] bppTerms
        Nothing -> bppTerms

bapTermsCacheKey :: Text -> Text
bapTermsCacheKey transactionId = "Driver:BAPTerms:TransactionId-" <> transactionId

mkBppTermsTagsWithAmount :: Maybe HighPrecMoney -> DBC.BecknConfig -> Maybe [Spec.TagGroup]
mkBppTermsTagsWithAmount mbOrderAmount becknConfig =
  let staticTermsValue = maybe "https://api.example-bpp.com/booking/terms" showBaseUrl becknConfig.staticTermsUrl
      settlementWindow = fromMaybe "P1D" becknConfig.settlementWindow
      settlementType = fromMaybe "neft" becknConfig.settlementType
      buyerFinderFee = fromMaybe "0" becknConfig.buyerFinderFee
      settlementAmount = case mbOrderAmount of
        Just amount ->
          let bffPct = fromMaybe (0 :: Double) (readMaybe $ T.unpack buyerFinderFee)
              amtDouble = fromRational (toRational amount) :: Double
              settleAmt = amtDouble * bffPct `safeDiv` 100
           in Just $ T.pack $ printf "%.2f" settleAmt
        Nothing -> Nothing
      safeDiv _ 0 = 0
      safeDiv a b = a / b
      mkTag code value =
        Spec.Tag
          { tagDescriptor = Just Spec.Descriptor {descriptorCode = Just code, descriptorName = Nothing, descriptorShortDesc = Nothing},
            tagDisplay = Just False,
            tagValue = Just value
          }
      bppTermsGroup =
        Spec.TagGroup
          { tagGroupDescriptor =
              Just
                Spec.Descriptor
                  { descriptorCode = Just "BPP_TERMS",
                    descriptorName = Just "BPP Terms of Engagement",
                    descriptorShortDesc = Nothing
                  },
            tagGroupDisplay = Just False,
            tagGroupList =
              Just $
                [ mkTag "BUYER_FINDER_FEES_PERCENTAGE" buyerFinderFee,
                  mkTag "DELAY_INTEREST" "5",
                  mkTag "SETTLEMENT_WINDOW" settlementWindow,
                  mkTag "SETTLEMENT_TYPE" settlementType,
                  mkTag "SETTLEMENT_BASIS" "DELIVERY",
                  mkTag "MANDATORY_ARBITRATION" "true",
                  mkTag "COURT_JURISDICTION" "Bengaluru",
                  mkTag "STATIC_TERMS" staticTermsValue
                ]
                  <> maybe [] (\amt -> [mkTag "SETTLEMENT_AMOUNT" amt]) settlementAmount
          }
   in Just [bppTermsGroup]

mkFulfillmentV2SoftUpdate ::
  (MonadFlow m, EncFlow m r) =>
  Maybe SP.Person ->
  Maybe DDriverStats.DriverStats ->
  DRide.Ride ->
  DBooking.Booking ->
  Maybe DVeh.Vehicle ->
  Maybe Text ->
  Maybe [Spec.TagGroup] ->
  Maybe [Spec.TagGroup] ->
  Bool ->
  Bool ->
  Maybe Payment.AccountId ->
  Maybe Text ->
  IsValueAddNP ->
  DLoc.Location ->
  Bool ->
  Int ->
  m Spec.Fulfillment
mkFulfillmentV2SoftUpdate mbDriver mbDriverStats ride booking mbVehicle mbImage mbTags _mbPersonTags isDriverBirthDay isFreeRide driverAccountId mbEvent isValueAddNP newDestination isAlreadyFav favCount = do
  mbDInfo <- driverInfo
  let rideOtp = fromMaybe ride.otp ride.endOtp
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just $ getBookingFulfillmentId booking,
        fulfillmentStops = mkStops' booking.fromLocation (Just newDestination) booking.stops (Just rideOtp),
        fulfillmentType = Just $ Utils.tripCategoryToFulfillmentType booking.tripCategory,
        fulfillmentAgent =
          Just $
            Spec.Agent
              { agentContact =
                  mbDInfo >>= \dInfo ->
                    Just $
                      Spec.Contact
                        { contactPhone = Just dInfo.mobileNumber
                        },
                agentPerson =
                  Just $
                    emptyPerson
                      { Spec.personImage =
                          mbImage <&> \mbImage' ->
                            emptyImage {Spec.imageUrl = Just mbImage'},
                        Spec.personName = mbDInfo >>= Just . (.name),
                        Spec.personTags = Nothing -- ONDC spec: no person tags in agent
                      }
              },
        fulfillmentVehicle =
          mbVehicle >>= \vehicle -> do
            let (category, variant) = castVariant vehicle.variant
            Just $
              emptyVehicle
                { Spec.vehicleColor = Just vehicle.color,
                  Spec.vehicleModel = Just vehicle.model,
                  Spec.vehicleRegistration = Just vehicle.registrationNo,
                  Spec.vehicleCategory = Just category,
                  Spec.vehicleVariant = Just variant,
                  Spec.vehicleCapacity = vehicle.capacity
                },
        fulfillmentCustomer = Nothing,
        fulfillmentState = mkFulfillmentStateCode <$> mbEvent,
        fulfillmentTags = mbTags
      }
  where
    driverInfo = forM (liftM2 (,) mbDriver mbDriverStats) $ \(driver, driverStats) -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dAlternatePhoneNum <- SP.getPersonAlternateNumber driver
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId ride.trackingUrl dAlternatePhoneNum isAlreadyFav favCount booking.isSafetyPlus
      pure $
        DriverInfo
          { mobileNumber = dPhoneNum,
            alternateMobileNumber = dAlternatePhoneNum,
            name = dName,
            tags = if isValueAddNP then dTags else Nothing
          }

buildLocation' :: MonadFlow m => Id DM.Merchant -> Spec.Stop -> m DL.Location'
buildLocation' merchantId stop = do
  location <- stop.stopLocation & fromMaybeM (InvalidRequest "Location not present")
  guid <- generateGUID
  now <- getCurrentTime
  gps <- parseLatLong =<< (location.locationGps & fromMaybeM (InvalidRequest "Location GPS not present"))
  mbAddress <- parseAddress location
  let address = fromMaybe emptyLocationAddress mbAddress
  return $
    DL.Location'
      { DL.id = guid,
        createdAt = now,
        updatedAt = now,
        lat = gps.lat,
        lon = gps.lon,
        address,
        merchantId = Just merchantId
      }
  where
    emptyLocationAddress =
      DL.LocationAddress
        { area = Nothing,
          areaCode = Nothing,
          building = Nothing,
          city = Nothing,
          country = Nothing,
          door = Nothing,
          extras = Nothing,
          fullAddress = Nothing,
          instructions = Nothing,
          state = Nothing,
          street = Nothing
        }

castPaymentCollector :: MonadFlow m => Text -> m DMPM.PaymentCollector
castPaymentCollector "BAP" = return DMPM.BAP
castPaymentCollector "BPP" = return DMPM.BPP
castPaymentCollector _ = throwM $ InvalidRequest "Unknown Payment Collector"

castPaymentType :: MonadFlow m => Text -> m DMPM.PaymentType
castPaymentType "ON_ORDER" = return DMPM.ON_FULFILLMENT
castPaymentType "ON_FULFILLMENT" = return DMPM.POSTPAID
castPaymentType _ = throwM $ InvalidRequest "Unknown Payment Type"

mkIsSafetyPlusTagGroupV2 :: Bool -> Maybe [Spec.TagGroup]
mkIsSafetyPlusTagGroupV2 isSafetyPlus =
  Tags.buildTagGroups
    [ Tags.IS_SAFETY_PLUS ~=| (isSafetyPlus, show isSafetyPlus)
    ]

mkForwardBatchTagGroupV2 :: Maybe Maps.LatLong -> Maybe [Spec.TagGroup]
mkForwardBatchTagGroupV2 previousRideDropLocation' =
  previousRideDropLocation' >>= \previousRideDropLocation ->
    Tags.buildTagGroups
      [ Tags.PREVIOUS_RIDE_DROP_LOCATION_LAT ~= show previousRideDropLocation.lat,
        Tags.PREVIOUS_RIDE_DROP_LOCATION_LON ~= show previousRideDropLocation.lon
      ]

getShouldFavouriteDriver :: Spec.Rating -> Maybe Bool
getShouldFavouriteDriver req = do
  let tagGroups = req.ratingTag
      tagValue = Utils.getTagV2 Tags.RATING_TAGS Tags.SHOULD_FAVOURITE_DRIVER tagGroups
   in readMaybe . T.unpack =<< tagValue

getRiderPhoneNumber :: Spec.Rating -> Maybe Text
getRiderPhoneNumber req = do
  let tagGroups = req.ratingTag
      tagValue = Utils.getTagV2 Tags.RATING_TAGS Tags.RIDER_PHONE_NUMBER tagGroups
   in tagValue

getFilePath :: Spec.Rating -> Maybe Text
getFilePath req = do
  let tagGroups = req.ratingTag
      tagValue = Utils.getTagV2 Tags.RATING_TAGS Tags.MEDIA_FILE_PATH tagGroups
   in tagValue

getRiderName :: Spec.Rating -> Maybe Text
getRiderName req = do
  let tagGroups = req.ratingTag
      tagValue = Utils.getTagV2 Tags.RATING_TAGS Tags.RIDER_NAME tagGroups
   in tagValue

getCancellationReason :: Spec.CancelReq -> Maybe Text
getCancellationReason req = req.cancelReqMessage.cancelReqMessageDescriptor >>= (.descriptorShortDesc)

mkFulfillmentState :: Enums.FulfillmentState -> Spec.FulfillmentState
mkFulfillmentState = mkFulfillmentStateCode . show

mkFulfillmentStateCode :: Text -> Spec.FulfillmentState
mkFulfillmentStateCode code =
  Spec.FulfillmentState
    { fulfillmentStateDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just code,
              descriptorShortDesc = Nothing,
              descriptorName = Nothing
            }
    }

mkDestinationReachedTimeTagGroupV2 :: Maybe UTCTime -> Maybe [Spec.TagGroup]
mkDestinationReachedTimeTagGroupV2 = Tags.mkSingleTagGroup Tags.DRIVER_REACHED_DESTINATION

validateSearchContext :: (HasFlowEnv m r '["_version" ::: Text], MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Spec.Context -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m ()
validateSearchContext context merchantId merchantOperatingCityId = do
  ContextUtils.validateContext Context.SEARCH context
  bapId <- getContextBapId context
  validateSubscriber bapId merchantId merchantOperatingCityId

validateSubscriber :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m ()
validateSubscriber subscriberId merchantId merchantOperatingCityId = do
  totalSubIds <- QWhiteList.countTotalSubscribers merchantId merchantOperatingCityId
  void $
    if totalSubIds == 0
      then do
        checkBlacklisted subscriberId merchantId merchantOperatingCityId
      else do
        checkWhitelisted subscriberId merchantId merchantOperatingCityId

checkBlacklisted :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m ()
checkBlacklisted subscriberId merchantId merchantOperatingCityId = do
  whenM (isBlackListed subscriberId Domain.MOBILITY merchantId merchantOperatingCityId) . throwError . InvalidRequest $
    "It is a Blacklisted subscriber " <> subscriberId

isBlackListed :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Domain.Domain -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m Bool
isBlackListed subscriberId domain merchantId merchantOperatingCityId = isJust <$> QBlackList.findBySubscriberIdDomainMerchantIdAndMerchantOperatingCityId (ShortId subscriberId) domain merchantId merchantOperatingCityId

checkWhitelisted :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m ()
checkWhitelisted subscriberId merchantId merchantOperatingCityId = do
  whenM (isNotWhiteListed subscriberId Domain.MOBILITY merchantId merchantOperatingCityId) . throwError . InvalidRequest $
    "It is not a whitelisted subscriber " <> subscriberId

isNotWhiteListed :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Domain.Domain -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m Bool
isNotWhiteListed subscriberId domain merchantId merchantOperatingCityId = isNothing <$> QWhiteList.findBySubscriberIdDomainMerchantIdAndMerchantOperatingCityId (ShortId subscriberId) domain merchantId merchantOperatingCityId
