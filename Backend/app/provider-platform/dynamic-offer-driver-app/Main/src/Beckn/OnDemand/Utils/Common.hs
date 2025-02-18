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
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.OnDemand.Utils.Context as ContextUtils
import BecknV2.OnDemand.Utils.Payment
import qualified BecknV2.Utils as Utils
import Control.Lens
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as T
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
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as Variant
import EulerHS.Prelude hiding (id, state, view, whenM, (%~), (^?))
import qualified EulerHS.Prelude as Prelude
import GHC.Float (double2Int)
import qualified Kernel.External.Maps as Maps
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
    smartTipReason :: Maybe Text
  }

data RateCardBreakupItem = RateCardBreakupItem
  { title :: Text,
    value :: Text
  }

mkStops :: Maps.LatLong -> Maybe Maps.LatLong -> [Maps.LatLong] -> Maybe [Spec.Stop]
mkStops origin mbDestination intermediateStops = do
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps destination = Gps.Gps {lat = destination.lat, lon = destination.lon}
  Just $
    catMaybes
      [ Just $
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = Nothing, -- For start and end in on_search, we send address as nothing
                      locationAreaCode = Nothing,
                      locationCity = Nothing,
                      locationCountry = Nothing,
                      locationGps = Utils.gpsToText originGps,
                      locationState = Nothing,
                      locationId = Nothing,
                      locationUpdatedAt = Nothing
                    },
              stopType = Just $ show Enums.START,
              stopAuthorization = Nothing,
              stopTime = Nothing,
              stopId = Just "0",
              stopParentStopId = Nothing
            },
        ( \destination ->
            Spec.Stop
              { stopLocation =
                  Just $
                    Spec.Location
                      { locationAddress = Nothing, -- For start and end in on_search, we send address as nothing
                        locationAreaCode = Nothing,
                        locationCity = Nothing,
                        locationCountry = Nothing,
                        locationGps = Utils.gpsToText $ destinationGps destination,
                        locationState = Nothing,
                        locationId = Nothing,
                        locationUpdatedAt = Nothing
                      },
                stopType = Just $ show Enums.END,
                stopAuthorization = Nothing,
                stopTime = Nothing,
                stopId = Just $ show (length intermediateStops + 1),
                stopParentStopId = Just $ show (length intermediateStops)
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
  (Just "CAB", Just "HERITAGE_CAB") -> Just Variant.HERITAGE_CAB
  (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just Variant.AUTO_RICKSHAW
  (Just "AUTO_RICKSHAW", Just "EV_AUTO_RICKSHAW") -> Just Variant.EV_AUTO_RICKSHAW
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
  _ -> Nothing

parseAddress :: MonadFlow m => Spec.Location -> m (Maybe DL.LocationAddress)
parseAddress loc@Spec.Location {..} = do
  let areaCode = locationAreaCode
  let city' = locationCity >>= (.cityName)
  let state' = locationState >>= (.stateName)
  let country' = locationCountry >>= (.countryName)
  locationAddress' <- locationAddress & fromMaybeM (InvalidRequest $ "Missing locationAddress:-" <> show loc)
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
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = Just $ mkAddress origin.address,
                          locationAreaCode = origin.address.areaCode,
                          locationCity = Just $ Spec.City Nothing origin.address.city,
                          locationCountry = Just $ Spec.Country Nothing origin.address.country,
                          locationGps = Utils.gpsToText originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.START,
                  stopAuthorization = mAuthorization >>= mkAuthorization,
                  stopTime = Nothing,
                  stopId = Just "0",
                  stopParentStopId = Nothing
                },
            ( \destination ->
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Just $ mkAddress destination.address,
                            locationAreaCode = destination.address.areaCode,
                            locationCity = Just $ Spec.City Nothing destination.address.city,
                            locationCountry = Just $ Spec.Country Nothing destination.address.country,
                            locationGps = Utils.gpsToText $ destinationGps destination,
                            locationState = Just $ Spec.State destination.address.state,
                            locationId = Nothing,
                            locationUpdatedAt = Nothing
                          },
                    stopType = Just $ show Enums.END,
                    stopAuthorization = Nothing,
                    stopTime = Nothing,
                    stopId = Just $ show (length intermediateStops + 1),
                    stopParentStopId = Just $ show (length intermediateStops)
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
          { authorizationToken = Just auth,
            authorizationType = Just $ show Enums.OTP
          }

mkAddress :: DLoc.LocationAddress -> Text
mkAddress DLoc.LocationAddress {..} =
  let res = map replaceEmpty [door, building, street, area, city, state, country]
   in T.intercalate ", " $ catMaybes res

mkIntermediateStop :: DLoc.Location -> Int -> Int -> Spec.Stop
mkIntermediateStop stop id parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Just $ mkAddress stop.address,
                  locationAreaCode = stop.address.areaCode,
                  locationCity = Just $ Spec.City Nothing stop.address.city,
                  locationCountry = Just $ Spec.Country Nothing stop.address.country,
                  locationGps = Utils.gpsToText gps,
                  locationState = Just $ Spec.State stop.address.state,
                  locationId = Just stop.id.getId,
                  locationUpdatedAt = Nothing
                },
          stopType = Just $ show Enums.INTERMEDIATE_STOP,
          stopAuthorization = Nothing,
          stopTime = Nothing,
          stopId = Just $ show id,
          stopParentStopId = Just $ show parentStopId
        }

mkIntermediateStopSearch :: Maps.LatLong -> Int -> Int -> Spec.Stop
mkIntermediateStopSearch stop id parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Nothing,
                  locationAreaCode = Nothing,
                  locationCity = Nothing,
                  locationCountry = Nothing,
                  locationGps = Utils.gpsToText gps,
                  locationState = Nothing,
                  locationId = Nothing,
                  locationUpdatedAt = Nothing
                },
          stopType = Just $ show Enums.INTERMEDIATE_STOP,
          stopAuthorization = Nothing,
          stopTime = Nothing,
          stopId = Just $ show id,
          stopParentStopId = Just $ show parentStopId
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
mkStopsOUS :: DBooking.Booking -> DRide.Ride -> Text -> Maybe [Spec.Stop]
mkStopsOUS booking ride rideOtp =
  let origin = booking.fromLocation
      mbDestination = booking.toLocation
      intermediateStops = booking.stops
      originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps dest = Gps.Gps {lat = dest.lat, lon = dest.lon}
   in Just $
        catMaybes
          [ Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = Just $ mkAddress origin.address,
                          locationAreaCode = origin.address.areaCode,
                          locationCity = Just $ Spec.City Nothing origin.address.city,
                          locationCountry = Just $ Spec.Country Nothing origin.address.country,
                          locationGps = Utils.gpsToText originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.START,
                  stopId = Just "0",
                  stopParentStopId = Nothing,
                  stopAuthorization =
                    Just $
                      Spec.Authorization
                        { authorizationToken = Just rideOtp,
                          authorizationType = Just $ show Enums.OTP
                        },
                  stopTime = ride.tripStartTime <&> \tripStartTime' -> Spec.Time {timeTimestamp = Just tripStartTime', timeDuration = Nothing}
                },
            Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = (\dest -> Just $ mkAddress dest.address) =<< mbDestination,
                          locationAreaCode = (\dest -> dest.address.areaCode) =<< mbDestination,
                          locationCity = (\dest -> Just $ Spec.City Nothing $ dest.address.city) =<< mbDestination,
                          locationCountry = (\dest -> Just $ Spec.Country Nothing $ dest.address.country) =<< mbDestination,
                          locationGps = (\dest -> Utils.gpsToText (destinationGps dest)) =<< mbDestination,
                          locationState = (\dest -> Just $ Spec.State dest.address.state) =<< mbDestination,
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.END,
                  stopAuthorization = Nothing,
                  stopTime = ride.tripEndTime <&> \tripEndTime' -> Spec.Time {timeTimestamp = Just tripEndTime', timeDuration = Nothing},
                  stopId = Just $ show (length intermediateStops + 1),
                  stopParentStopId = Just $ show (length intermediateStops)
                }
          ]
          <> (map (\(location, order) -> mkIntermediateStop location order (order - 1)) $ zip intermediateStops [1 ..])

type IsValueAddNP = Bool

-- common for on_update & on_status
mkFulfillmentV2 ::
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
  Maybe Text ->
  Bool ->
  Int ->
  m Spec.Fulfillment
mkFulfillmentV2 mbDriver mbDriverStats ride booking mbVehicle mbImage mbTags mbPersonTags isDriverBirthDay isFreeRide driverAccountId mbEvent isValueAddNP riderPhone isAlreadyFav favCount = do
  mbDInfo <- driverInfo
  let rideOtp = fromMaybe ride.otp ride.endOtp
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just ride.id.getId,
        fulfillmentStops = mkStopsOUS booking ride rideOtp,
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
                    Spec.Person
                      { personId = Nothing,
                        personImage =
                          mbImage <&> \mbImage' ->
                            Spec.Image
                              { imageHeight = Nothing,
                                imageSizeType = Nothing,
                                imageUrl = Just mbImage',
                                imageWidth = Nothing
                              },
                        personName = mbDInfo >>= Just . (.name),
                        personTags = mbDInfo >>= (.tags) & (mbPersonTags <>)
                      }
              },
        fulfillmentVehicle =
          mbVehicle >>= \vehicle -> do
            let (category, variant) = castVariant vehicle.variant
            Just $
              Spec.Vehicle
                { vehicleColor = Just vehicle.color,
                  vehicleModel = Just vehicle.model,
                  vehicleRegistration = Just vehicle.registrationNo,
                  vehicleCategory = Just category,
                  vehicleVariant = Just variant,
                  vehicleMake = Nothing,
                  vehicleCapacity = vehicle.capacity
                },
        fulfillmentCustomer = tfCustomer riderPhone booking.riderName,
        fulfillmentState =
          mbEvent
            >> ( Just $
                   Spec.FulfillmentState
                     { fulfillmentStateDescriptor =
                         Just $
                           Spec.Descriptor
                             { descriptorCode = mbEvent,
                               descriptorName = Nothing,
                               descriptorShortDesc = Nothing
                             }
                     }
               ),
        fulfillmentTags = mbTags
      }
  where
    driverInfo = forM (liftM2 (,) mbDriver mbDriverStats) $ \(driver, driverStats) -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dAlternatePhoneNum <- SP.getPersonAlternateNumber driver
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId ride.trackingUrl dAlternatePhoneNum isAlreadyFav favCount
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
          Just
            Spec.Contact
              { contactPhone = riderPhone
              },
        customerPerson = do
          Just $
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = riderName,
                personTags = Nothing
              }
      }

mkDriverDetailsTags :: SP.Person -> DDriverStats.DriverStats -> Bool -> Bool -> Maybe Payment.AccountId -> BaseUrl -> Maybe Text -> Bool -> Int -> Maybe [Spec.TagGroup]
mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId driverTrackingUrl driverAlternateNumber isAlreadyFav favCount =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.DRIVER_DETAILS,
                  descriptorName = Just "Driver Details",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              registeredAtSingleton
                ++ driverRatingSingleton
                ++ isDriverBirthDaySingleton
                ++ isFreeRideSingleton
                ++ driverAccountIdSingleton
                ++ driverTrackingUrlSingleton
                ++ driverAlternateNumberSingleton
                ++ isAlreadyFavSingleton
                ++ favCountSingleton
        }
    ]
  where
    registeredAtSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.REGISTERED_AT,
                    descriptorName = Just "Registered At",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show driver.createdAt
          }

    driverRatingSingleton
      | isNothing driverStats.rating = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.RATING,
                      descriptorName = Just "rating",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = show <$> driverStats.rating
            }

    isDriverBirthDaySingleton
      | not isDriverBirthDay = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_DRIVER_BIRTHDAY,
                      descriptorName = Just "Is Driver BirthDay",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show isDriverBirthDay
            }

    driverAccountIdSingleton
      | isNothing driverAccountId = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DRIVER_ACCOUNT_ID,
                      descriptorName = Just "Driver Account Id",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = driverAccountId
            }

    isFreeRideSingleton
      | not isFreeRide = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_FREE_RIDE,
                      descriptorName = Just "Is Free Ride",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show isFreeRide
            }

    isAlreadyFavSingleton
      | not isAlreadyFav = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_ALREADY_FAVOURITE,
                      descriptorName = Just "Is already favourite",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show isAlreadyFav
            }

    favCountSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.FAVOURITE_COUNT,
                    descriptorName = Just "Favourite Count",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show favCount
          }

    driverTrackingUrlSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.DRIVER_TRACKING_URL,
                    descriptorName = Just "Driver Tracking Url",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ showBaseUrl driverTrackingUrl
          }
    driverAlternateNumberSingleton
      | isNothing driverAlternateNumber = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DRIVER_ALTERNATE_NUMBER,
                      descriptorName = Just "Driver Alternate Number",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = driverAlternateNumber
            }

mkLocationTagGroupV2 :: Maybe Maps.LatLong -> Maybe [Spec.TagGroup]
mkLocationTagGroupV2 location' =
  location' <&> \location ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.CURRENT_LOCATION,
                  descriptorName = Just "Current Location",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.CURRENT_LOCATION_LAT,
                            descriptorName = Just "Current Location Lat",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show location.lat
                  },
                Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.CURRENT_LOCATION_LON,
                            descriptorName = Just "Current Location Lon",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show location.lon
                  }
              ]
        }
    ]

mkArrivalTimeTagGroupV2 :: Maybe UTCTime -> Maybe [Spec.TagGroup]
mkArrivalTimeTagGroupV2 arrivalTime' =
  arrivalTime' <&> \arrivalTime ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.DRIVER_ARRIVED_INFO,
                  descriptorName = Just "Driver Arrived Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.ARRIVAL_TIME,
                            descriptorName = Just "Arrival Time",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show arrivalTime
                  }
              ]
        }
    ]

mkEstimatedEndTimeRangeTagGroupV2 :: Maybe DRide.EstimatedEndTimeRange -> Maybe [Spec.TagGroup]
mkEstimatedEndTimeRangeTagGroupV2 estimatedEndTimeRange' =
  estimatedEndTimeRange' <&> \estimatedEndTimeRange ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.ESTIMATED_END_TIME_RANGE,
                  descriptorName = Just "Estimated End Time Range",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.ESTIMATED_END_TIME_RANGE_START,
                            descriptorName = Just "Estimated End Time Range Start",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show $ estimatedEndTimeRange.start
                  },
                Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.ESTIMATED_END_TIME_RANGE_END,
                            descriptorName = Just "Estimated End Time Range End",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show $ estimatedEndTimeRange.end
                  }
              ]
        }
    ]

mkParcelImageUploadedTag :: Maybe [Spec.TagGroup]
mkParcelImageUploadedTag =
  Just
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.DELIVERY,
                  descriptorName = Just "Delivery Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.PARCEL_IMAGE_UPLOADED,
                            descriptorName = Just "Parcel file uploaded info",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just "True"
                  }
              ]
        }
    ]

mkVehicleTags :: Maybe Double -> Maybe Bool -> Maybe [Spec.TagGroup]
mkVehicleTags vehicleServiceTierAirConditioned' isAirConditioned =
  vehicleServiceTierAirConditioned' <&> \vehicleServiceTierAirConditioned ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just True,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.VEHICLE_INFO,
                  descriptorName = Just "Vehicle Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just True,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.IS_AIR_CONDITIONED,
                            descriptorName = Just "isAirConditioned",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show vehicleServiceTierAirConditioned
                  },
                Spec.Tag
                  { tagDisplay = Just True,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.IS_AIR_CONDITIONED_VEHICLE,
                            descriptorName = Just "isAirConditionedVehicle",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = show <$> isAirConditioned
                  }
              ]
        }
    ]

mkOdometerTagGroupV2 :: Maybe Centesimal -> Maybe [Spec.TagGroup]
mkOdometerTagGroupV2 startOdometerReading' =
  startOdometerReading' <&> \startOdometerReading ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.RIDE_ODOMETER_DETAILS,
                  descriptorName = Just "Ride Odometer Details",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.START_ODOMETER_READING,
                            descriptorName = Just "Start Odometer Reading",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show startOdometerReading
                  }
              ]
        }
    ]

mkTollConfidenceTagGroupV2 :: Maybe Confidence -> Maybe [Spec.TagGroup]
mkTollConfidenceTagGroupV2 tollConfidence' =
  tollConfidence' <&> \tollConfidence ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.TOLL_CONFIDENCE_INFO,
                  descriptorName = Just "Toll Confidence Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.TOLL_CONFIDENCE,
                            descriptorName = Just "Toll Confidence (Sure/Unsure/Neutral)",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show tollConfidence
                  }
              ]
        }
    ]

mkRideDetailsTagGroup :: Maybe Bool -> Maybe [Spec.TagGroup]
mkRideDetailsTagGroup rideDetails' =
  rideDetails' <&> \rideDetails ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.RIDE_DETAILS_INFO,
                  descriptorName = Just "Ride Details",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.IS_VALID_RIDE,
                            descriptorName = Just "Is Valid Ride",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show rideDetails
                  }
              ]
        }
    ]

mkVehicleAgeTagGroupV2 :: Maybe Months -> Maybe [Spec.TagGroup]
mkVehicleAgeTagGroupV2 vehicleAge' =
  vehicleAge' <&> \vehicleAge ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.VEHICLE_AGE_INFO,
                  descriptorName = Just "Vehicle Age Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.VEHICLE_AGE,
                            descriptorName = Just "Vehicle Age",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show vehicleAge
                  }
              ]
        }
    ]

buildAddressFromText :: MonadFlow m => Text -> m OS.Address
buildAddressFromText fullAddress = do
  let splitedAddress = T.splitOn ", " fullAddress
      totalAddressComponents = List.length splitedAddress
  logDebug $ "Search Address:-" <> fullAddress
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
  pure $ OS.Address {area_code = area_code_, building = building_, city = city_, country = country_, door = door_, locality = locality_, state = state_, street = street_, ward = ward}

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
tfCancellationFee Nothing = Nothing
tfCancellationFee (Just price) = do
  Just
    Spec.Fee
      { feeAmount = mkPrice,
        feePercentage = Nothing
      }
  where
    mkPrice =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just $ show price.currency,
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just $ encodeToText price.amount
          }

tfFulfillmentState :: Enums.FulfillmentState -> Maybe Spec.FulfillmentState
tfFulfillmentState state =
  Just $
    Spec.FulfillmentState
      { fulfillmentStateDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just $ show state,
                descriptorName = Nothing,
                descriptorShortDesc = Nothing
              }
      }

tfQuotation :: DBooking.Booking -> Maybe Spec.Quotation
tfQuotation booking =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup booking.fareParams,
        quotationPrice = tfQuotationPrice $ HighPrecMoney $ toRational booking.estimatedFare,
        quotationTtl = Nothing
      }

tfQuotationSU :: DFParams.FareParameters -> HighPrecMoney -> Maybe Spec.Quotation
tfQuotationSU fareParams estimatedFare =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup fareParams,
        quotationPrice = tfQuotationPrice estimatedFare,
        quotationTtl = Nothing
      }

tfQuotationPrice :: HighPrecMoney -> Maybe Spec.Price
tfQuotationPrice estimatedFare =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText estimatedFare,
        priceValue = Just $ encodeToText estimatedFare
      }

mkQuotationBreakup :: DFParams.FareParameters -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup fareParams =
  let fareParameters = mkFareParamsBreakups mkPrice mkQuotationBreakupInner fareParams
   in Just $ filter (filterRequiredBreakups $ DFParams.getFareParametersType fareParams) fareParameters -- TODO: Remove after roll out
  where
    mkPrice :: HighPrecMoney -> Maybe Spec.Price
    mkPrice money =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just "INR",
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Just $ encodeToText money,
            priceValue = Just $ encodeToText money
          }

    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }

    filterRequiredBreakups :: DFParams.FareParametersType -> Spec.QuotationBreakupInner -> Bool
    filterRequiredBreakups fParamsType breakup = do
      case fParamsType of
        DFParams.Progressive ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DISTANCE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
        DFParams.Slab ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PLATFORM_FEE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SGST)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CGST)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.FIXED_GOVERNMENT_RATE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_CHARGES)
        DFParams.Rental ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DISTANCE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TIME_BASED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
        _ -> True

type MerchantShortId = Text

tfItems :: DBooking.Booking -> MerchantShortId -> Maybe Meters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> Maybe [Spec.Item]
tfItems booking shortId estimatedDistance mbFarePolicy mbPaymentId =
  Just
    [ Spec.Item
        { itemDescriptor = tfItemDescriptor booking,
          itemFulfillmentIds = Just [booking.quoteId],
          itemId = Just $ maybe (Common.mkItemId shortId booking.vehicleServiceTier) getId (booking.estimateId),
          itemLocationIds = Nothing,
          itemPaymentIds = tfPaymentId mbPaymentId,
          itemPrice = tfItemPrice $ booking.estimatedFare,
          itemTags = mkRateCardTag estimatedDistance Nothing booking.estimatedFare booking.fareParams.congestionChargeViaDp mbFarePolicy Nothing Nothing
        }
    ]

tfItemsSoftUpdate :: DBooking.Booking -> MerchantShortId -> Maybe HighPrecMeters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> DBUR.BookingUpdateRequest -> Text -> Maybe [Spec.Item]
tfItemsSoftUpdate booking shortId estimatedDistance mbFarePolicy mbPaymentId updatedBooking rideId = do
  let estimatedDistance' = maybe Nothing (\dist -> Just $ highPrecMetersToMeters dist) estimatedDistance
  Just
    [ Spec.Item
        { itemDescriptor = tfItemDescriptor booking,
          itemFulfillmentIds = Just [rideId],
          itemId = Just $ Common.mkItemId shortId booking.vehicleServiceTier,
          itemLocationIds = Nothing,
          itemPaymentIds = tfPaymentId mbPaymentId,
          itemPrice = tfItemPrice updatedBooking.estimatedFare,
          itemTags = mkRateCardTag estimatedDistance' Nothing booking.estimatedFare booking.fareParams.congestionChargeViaDp mbFarePolicy Nothing Nothing
        }
    ]

tfPaymentId :: Maybe Text -> Maybe [Text]
tfPaymentId mbPaymentId = do
  paymentId <- mbPaymentId
  Just [paymentId]

tfItemPrice :: HighPrecMoney -> Maybe Spec.Price
tfItemPrice estimatedFare =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice Nothing estimatedFare, -- TODO : Remove this and make non mandatory on BAP side
        priceValue = Just $ Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice Nothing estimatedFare -- Sending Nothing here becuase priceCurrency in hardcoded to INR, TODO: make this logic dynamic based on country
      }

tfItemDescriptor :: DBooking.Booking -> Maybe Spec.Descriptor
tfItemDescriptor booking =
  Just
    Spec.Descriptor
      { descriptorCode = Just "RIDE",
        descriptorShortDesc = Just $ show booking.vehicleServiceTier,
        descriptorName = Just $ show booking.vehicleServiceTier
      }

convertEstimateToPricing :: Maybe Text -> (DEst.Estimate, DVST.VehicleServiceTier, Maybe NearestDriverInfo, Maybe BaseUrl) -> Pricing
convertEstimateToPricing specialLocationName (DEst.Estimate {..}, serviceTier, mbDriverLocations, vehicleIconUrl) =
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
      ..
    }

mkGeneralInfoTagGroup :: DTC.TransporterConfig -> Pricing -> Bool -> Maybe Spec.TagGroup
mkGeneralInfoTagGroup transporterConfig pricing isValueAddNP =
  Just $
    Spec.TagGroup
      { tagGroupDisplay = Just False,
        tagGroupDescriptor =
          Just
            Spec.Descriptor
              { descriptorCode = Just $ show Tags.INFO,
                descriptorName = Just "Information",
                descriptorShortDesc = Nothing
              },
        tagGroupList =
          specialLocationTagSingleton pricing.specialLocationTag
            <> specialLocationNameTag pricing.specialLocationName
            <> distanceToNearestDriverTagSingleton pricing.distanceToNearestDriver
            <> isCustomerPrefferedSearchRouteSingleton pricing.isCustomerPrefferedSearchRoute
            <> isBlockedRouteSingleton pricing.isBlockedRoute
            <> tollNamesSingleton pricing.tollNames
            <> tipOptionSingleton pricing.tipOptions
            <> durationToNearestDriverTagSingleton
            <> smartTipSuggestionTagSingleton
            <> smartTipReasonTagSingleton
      }
  where
    smartTipSuggestionTagSingleton
      | isNothing pricing.smartTipSuggestion || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.SMART_TIP_SUGGESTION,
                      descriptorName = Just "Smart Tip Suggestion",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> pricing.smartTipSuggestion
            }
    smartTipReasonTagSingleton
      | isNothing pricing.smartTipReason || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just True,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.SMART_TIP_REASON,
                      descriptorName = Just "Smart Tip Reason",
                      descriptorShortDesc = Nothing
                    },
              tagValue = pricing.smartTipReason
            }
    specialLocationTagSingleton specialLocationTag
      | isNothing specialLocationTag = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just True,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.SPECIAL_LOCATION_TAG,
                      descriptorName = Just "Special Location Tag",
                      descriptorShortDesc = Nothing
                    },
              tagValue = specialLocationTag
            }
    specialLocationNameTag specialLocationName
      | isNothing specialLocationName = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.SPECIAL_LOCATION_NAME,
                      descriptorName = Just "Special Location Name",
                      descriptorShortDesc = Nothing
                    },
              tagValue = specialLocationName
            }
    distanceToNearestDriverTagSingleton distanceToNearestDriver
      | isNothing distanceToNearestDriver = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DISTANCE_TO_NEAREST_DRIVER_METER,
                      descriptorName = Just "Distance To Nearest Driver Meter",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show . double2Int . realToFrac <$> distanceToNearestDriver
            }
    isCustomerPrefferedSearchRouteSingleton isCustomerPrefferedSearchRoute
      | isNothing isCustomerPrefferedSearchRoute || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_CUSTOMER_PREFFERED_SEARCH_ROUTE,
                      descriptorName = Just "Is Customer Preffered Search Route",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> isCustomerPrefferedSearchRoute
            }
    isBlockedRouteSingleton isBlockedRoute
      | isNothing isBlockedRoute || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_BLOCKED_SEARCH_ROUTE,
                      descriptorName = Just "Is Blocked Search Route",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> isBlockedRoute
            }
    tollNamesSingleton tollNames
      | isNothing tollNames || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.TOLL_NAMES,
                      descriptorName = Just "Toll Names",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> tollNames
            }
    tipOptionSingleton tipOptions
      | isNothing tipOptions || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.TIP_OPTIONS,
                      descriptorName = Just "Tip Options",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> tipOptions
            }
    durationToNearestDriverTagSingleton
      | isNothing pricing.distanceToNearestDriver || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DURATION_TO_NEAREST_DRIVER_MINUTES,
                      descriptorName = Just $ show pricing.vehicleVariant,
                      descriptorShortDesc = Nothing
                    },
              tagValue = getDurationToNearestDriver (DTC.avgSpeedOfVehicle transporterConfig)
            }
      where
        getDurationToNearestDriver :: Maybe DTC.AvgSpeedOfVechilePerKm -> Maybe Text
        getDurationToNearestDriver avgSpeedOfVehicle = do
          avgSpeed <- avgSpeedOfVehicle
          let variantSpeed = case pricing.vehicleVariant of
                Variant.SEDAN -> avgSpeed.sedan.getKilometers
                Variant.SUV -> avgSpeed.suv.getKilometers
                Variant.HATCHBACK -> avgSpeed.hatchback.getKilometers
                Variant.AUTO_RICKSHAW -> avgSpeed.autorickshaw.getKilometers
                Variant.BIKE -> avgSpeed.bike.getKilometers
                Variant.DELIVERY_BIKE -> avgSpeed.bike.getKilometers
                Variant.TAXI -> avgSpeed.taxi.getKilometers
                Variant.TAXI_PLUS -> avgSpeed.ambulance.getKilometers
                Variant.PREMIUM_SEDAN -> avgSpeed.premiumsedan.getKilometers
                Variant.BLACK -> avgSpeed.black.getKilometers
                Variant.BLACK_XL -> avgSpeed.blackxl.getKilometers
                Variant.AMBULANCE_TAXI -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_TAXI_OXY -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_AC -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_AC_OXY -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_VENTILATOR -> avgSpeed.ambulance.getKilometers
                Variant.SUV_PLUS -> avgSpeed.suvplus.getKilometers
                Variant.HERITAGE_CAB -> avgSpeed.heritagecab.getKilometers
                Variant.EV_AUTO_RICKSHAW -> avgSpeed.evautorickshaw.getKilometers
                Variant.DELIVERY_LIGHT_GOODS_VEHICLE -> avgSpeed.deliveryLightGoodsVehicle.getKilometers
                Variant.DELIVERY_TRUCK_MINI -> avgSpeed.deliveryLightGoodsVehicle.getKilometers
                Variant.DELIVERY_TRUCK_SMALL -> avgSpeed.deliveryLightGoodsVehicle.getKilometers
                Variant.DELIVERY_TRUCK_MEDIUM -> avgSpeed.deliveryLightGoodsVehicle.getKilometers
                Variant.DELIVERY_TRUCK_LARGE -> avgSpeed.deliveryLightGoodsVehicle.getKilometers
                Variant.DELIVERY_TRUCK_ULTRA_LARGE -> avgSpeed.deliveryLightGoodsVehicle.getKilometers
                Variant.BUS_NON_AC -> avgSpeed.busNonAc.getKilometers
                Variant.BUS_AC -> avgSpeed.busAc.getKilometers

          getDuration pricing.distanceToNearestDriver variantSpeed

        getDuration :: Maybe Meters -> Int -> Maybe Text
        getDuration distance avgSpeed
          | avgSpeed <= 0 = Nothing
          -- lets return 60 seconds in case distance is 0
          | distance == Just 0 = Just "60"
          | otherwise = do
            distance' <- distance
            let distanceInMeters = realToFrac @_ @Double distance'
                avgSpeedInMetersPerSec = realToFrac @_ @Double (avgSpeed * 5) / 18
                estimatedTimeTakenInSeconds :: Int = ceiling $ (distanceInMeters / avgSpeedInMetersPerSec)
            Just $ show estimatedTimeTakenInSeconds

mkRateCardTag :: Maybe Meters -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> Maybe FarePolicyD.FarePolicy -> Maybe Bool -> Maybe Params.FareParameters -> Maybe [Spec.TagGroup]
mkRateCardTag estimatedDistance tollCharges estimatedFare congestionChargeViaDp farePolicy fareParametersInRateCard fareParams = do
  let farePolicyBreakups = maybe [] (mkFarePolicyBreakups Prelude.id mkRateCardBreakupItem estimatedDistance tollCharges estimatedFare congestionChargeViaDp) farePolicy
      fareParamsBreakups =
        case fareParametersInRateCard of
          Just True -> maybe [] (mkFareParamsBreakups (\price -> show price) mkRateCardBreakupItem) fareParams
          _ -> []
      filteredFareParamsBreakups = filter (not . findDup farePolicyBreakups) fareParamsBreakups
      combainedParams = farePolicyBreakups <> filteredFareParamsBreakups
      farePolicyBreakupsTags = buildRateCardTags <$> combainedParams
  Just
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.FARE_POLICY,
                  descriptorName = Just "Fare Policy",
                  descriptorShortDesc = Nothing
                },
          tagGroupList = Just farePolicyBreakupsTags
        }
    ]
  where
    findDup :: [RateCardBreakupItem] -> RateCardBreakupItem -> Bool
    findDup [] _ = False
    findDup (farePolicyCard : nextFarePolicy) fareParameter =
      if farePolicyCard.title == fareParameter.title then True else findDup nextFarePolicy fareParameter

mkVehicleIconTag :: Maybe BaseUrl -> Maybe [Spec.TagGroup]
mkVehicleIconTag mbBaseUrl =
  case mbBaseUrl of
    Just baseUrl ->
      Just $
        [ Spec.TagGroup
            { tagGroupDisplay = Just False,
              tagGroupDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.VEHICLE_INFO,
                      descriptorName = Just "Vehicle Icon",
                      descriptorShortDesc = Nothing
                    },
              tagGroupList =
                Just
                  [ Spec.Tag
                      { tagDisplay = Just False,
                        tagDescriptor =
                          Just
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.VEHICLE_ICON_URL,
                                descriptorName = Just "Vehicle Icon URL",
                                descriptorShortDesc = Nothing
                              },
                        tagValue = Just $ showBaseUrl baseUrl
                      }
                  ]
            }
        ]
    Nothing -> Nothing

mkRateCardBreakupItem :: Text -> Text -> RateCardBreakupItem
mkRateCardBreakupItem = RateCardBreakupItem

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
tfCancellationTerms cancellationFee state =
  List.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = tfCancellationFee cancellationFee,
        cancellationTermFulfillmentState = mkFulfillmentState <$> state,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }

tfPayments :: DBooking.Booking -> DM.Merchant -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments booking transporter bppConfig = do
  let mPrice = Just $ Common.mkPrice (Just booking.currency) booking.estimatedFare
  let mkParams :: Maybe DT.BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . List.singleton $ mkPayment (show transporter.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice booking.paymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

tfProvider :: DBC.BecknConfig -> Maybe Spec.Provider
tfProvider becknConfig =
  return $
    Spec.Provider
      { providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just $ becknConfig.subscriberId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing
      }

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
mkFulfillmentV2SoftUpdate mbDriver mbDriverStats ride booking mbVehicle mbImage mbTags mbPersonTags isDriverBirthDay isFreeRide driverAccountId mbEvent isValueAddNP newDestination isAlreadyFav favCount = do
  mbDInfo <- driverInfo
  let rideOtp = fromMaybe ride.otp ride.endOtp
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just ride.id.getId,
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
                    Spec.Person
                      { personId = Nothing,
                        personImage =
                          mbImage <&> \mbImage' ->
                            Spec.Image
                              { imageHeight = Nothing,
                                imageSizeType = Nothing,
                                imageUrl = Just mbImage',
                                imageWidth = Nothing
                              },
                        personName = mbDInfo >>= Just . (.name),
                        personTags = mbDInfo >>= (.tags) & (mbPersonTags <>)
                      }
              },
        fulfillmentVehicle =
          mbVehicle >>= \vehicle -> do
            let (category, variant) = castVariant vehicle.variant
            Just $
              Spec.Vehicle
                { vehicleColor = Just vehicle.color,
                  vehicleModel = Just vehicle.model,
                  vehicleRegistration = Just vehicle.registrationNo,
                  vehicleCategory = Just category,
                  vehicleVariant = Just variant,
                  vehicleMake = Nothing,
                  vehicleCapacity = vehicle.capacity
                },
        fulfillmentCustomer = Nothing,
        fulfillmentState =
          mbEvent
            >> ( Just $
                   Spec.FulfillmentState
                     { fulfillmentStateDescriptor =
                         Just $
                           Spec.Descriptor
                             { descriptorCode = mbEvent,
                               descriptorName = Nothing,
                               descriptorShortDesc = Nothing
                             }
                     }
               ),
        fulfillmentTags = mbTags
      }
  where
    driverInfo = forM (liftM2 (,) mbDriver mbDriverStats) $ \(driver, driverStats) -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dAlternatePhoneNum <- SP.getPersonAlternateNumber driver
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId ride.trackingUrl dAlternatePhoneNum isAlreadyFav favCount
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
  address <- parseAddress location >>= fromMaybeM (InvalidRequest "Location Address not present")
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

castPaymentCollector :: MonadFlow m => Text -> m DMPM.PaymentCollector
castPaymentCollector "BAP" = return DMPM.BAP
castPaymentCollector "BPP" = return DMPM.BPP
castPaymentCollector _ = throwM $ InvalidRequest "Unknown Payment Collector"

castPaymentType :: MonadFlow m => Text -> m DMPM.PaymentType
castPaymentType "ON_ORDER" = return DMPM.ON_FULFILLMENT
castPaymentType "ON_FULFILLMENT" = return DMPM.POSTPAID
castPaymentType _ = throwM $ InvalidRequest "Unknown Payment Type"

mkForwardBatchTagGroupV2 :: Maybe Maps.LatLong -> Maybe [Spec.TagGroup]
mkForwardBatchTagGroupV2 previousRideDropLocation' =
  previousRideDropLocation' <&> \previousRideDropLocation ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.FORWARD_BATCHING_REQUEST_INFO,
                  descriptorName = Just "Forward Batching Request Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.PREVIOUS_RIDE_DROP_LOCATION_LAT,
                            descriptorName = Just "Current Location Lat",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show previousRideDropLocation.lat
                  },
                Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.PREVIOUS_RIDE_DROP_LOCATION_LON,
                            descriptorName = Just "Current Location Lon",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show previousRideDropLocation.lon
                  }
              ]
        }
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
mkFulfillmentState stateCode =
  Spec.FulfillmentState
    { fulfillmentStateDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show stateCode,
              descriptorShortDesc = Nothing,
              descriptorName = Nothing
            }
    }

mkDestinationReachedTimeTagGroupV2 :: Maybe UTCTime -> Maybe [Spec.TagGroup]
mkDestinationReachedTimeTagGroupV2 destinationArrivalTime' =
  destinationArrivalTime' <&> \destinationArrivalTime ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.DRIVER_REACHED_DESTINATION_INFO,
                  descriptorName = Just "Driver Reached Destination Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.DRIVER_REACHED_DESTINATION,
                            descriptorName = Just "Destination Reached Time",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show destinationArrivalTime
                  }
              ]
        }
    ]

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
