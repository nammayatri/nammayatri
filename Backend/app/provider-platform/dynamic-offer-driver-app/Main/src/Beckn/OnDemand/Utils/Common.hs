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
import BecknV2.OnDemand.Utils.Payment
import Control.Lens
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as T
import Domain.Action.Beckn.Search
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DCT
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FarePolicy as Policy
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id, state, view, (%~), (^?))
import qualified EulerHS.Prelude as Prelude
import GHC.Float (double2Int)
import Kernel.External.Maps as Maps
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common hiding (mkPrice)
import Kernel.Utils.Common hiding (mkPrice)
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import Tools.Error

data Pricing = Pricing
  { pricingId :: Text,
    pricingMaxFare :: Money,
    pricingMinFare :: Money,
    vehicleVariant :: Variant.Variant,
    tripCategory :: DTC.TripCategory,
    fareParams :: Maybe Params.FareParameters,
    farePolicy :: Maybe Policy.FarePolicy,
    estimatedDistance :: Maybe Meters,
    specialLocationTag :: Maybe Text,
    fulfillmentType :: Text,
    distanceToNearestDriver :: Maybe Meters
  }

data RateCardBreakupItem = RateCardBreakupItem
  { title :: Text,
    value :: Text
  }

firstStop :: [Spec.Stop] -> Maybe Spec.Stop
firstStop = find (\stop -> Spec.stopType stop == Just (show Enums.START))

lastStop :: [Spec.Stop] -> Maybe Spec.Stop
lastStop = find (\stop -> Spec.stopType stop == Just (show Enums.END))

mkStops :: LatLong -> Maybe LatLong -> Maybe [Spec.Stop]
mkStops origin mbDestination = do
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
              stopTime = Nothing
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
                stopTime = Nothing
              }
        )
          <$> mbDestination
      ]

parseLatLong :: MonadFlow m => Text -> m Maps.LatLong
parseLatLong a =
  case T.splitOn "," a of
    [latStr, longStr] ->
      let lat = fromMaybe 0.0 $ readMaybe $ T.unpack latStr
          lon = fromMaybe 0.0 $ readMaybe $ T.unpack longStr
       in return $ LatLong lat lon
    _ -> throwError . InvalidRequest $ "Unable to parse LatLong"

getTransactionId :: MonadFlow m => Spec.Context -> m Text
getTransactionId context = do
  transactionUuid <- context.contextTransactionId & fromMaybeM (InvalidRequest "Missing transaction_id")
  pure $ T.pack $ show transactionUuid

getMessageId :: MonadFlow m => Spec.Context -> m Text
getMessageId context = do
  messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  pure $ T.pack $ show messageUuid

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

castVariant :: Variant.Variant -> (Text, Text)
castVariant Variant.SEDAN = (show Enums.CAB, "SEDAN")
castVariant Variant.HATCHBACK = (show Enums.CAB, "HATCHBACK")
castVariant Variant.SUV = (show Enums.CAB, "SUV")
castVariant Variant.AUTO_RICKSHAW = (show Enums.AUTO_RICKSHAW, "AUTO_RICKSHAW")
castVariant Variant.TAXI = (show Enums.CAB, "TAXI")
castVariant Variant.TAXI_PLUS = (show Enums.CAB, "TAXI_PLUS")

mkFulfillmentType :: DCT.TripCategory -> Text
mkFulfillmentType = \case
  DCT.OneWay DCT.OneWayRideOtp -> show Enums.RIDE_OTP
  DCT.RoundTrip DCT.RideOtp -> show Enums.RIDE_OTP
  DCT.RideShare DCT.RideOtp -> show Enums.RIDE_OTP
  DCT.Rental _ -> show Enums.RENTAL
  DCT.InterCity _ -> show Enums.INTER_CITY
  _ -> show Enums.DELIVERY

rationaliseMoney :: Money -> Text
rationaliseMoney = OS.valueToString . OS.DecimalValue . toRational

castDPaymentType :: DMPM.PaymentType -> Text
castDPaymentType DMPM.ON_FULFILLMENT = show Enums.ON_FULFILLMENT
castDPaymentType DMPM.POSTPAID = show Enums.ON_FULFILLMENT

parseVehicleVariant :: Maybe Text -> Maybe Text -> Maybe Variant.Variant
parseVehicleVariant mbCategory mbVariant = case (mbCategory, mbVariant) of
  (Just "CAB", Just "SEDAN") -> Just Variant.SEDAN
  (Just "CAB", Just "SUV") -> Just Variant.SUV
  (Just "CAB", Just "HATCHBACK") -> Just Variant.HATCHBACK
  (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just Variant.AUTO_RICKSHAW
  (Just "CAB", Just "TAXI") -> Just Variant.TAXI
  (Just "CAB", Just "TAXI_PLUS") -> Just Variant.TAXI_PLUS
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

mkStops' :: DLoc.Location -> Maybe DLoc.Location -> Maybe Text -> Maybe [Spec.Stop]
mkStops' origin mbDestination mAuthorization =
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
                  stopTime = Nothing
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
                    stopTime = Nothing
                  }
            )
              <$> mbDestination
          ]
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

data DriverInfo = DriverInfo
  { mobileNumber :: Text,
    name :: Text,
    tags :: Maybe [Spec.TagGroup]
  }

showVariant :: DVeh.Variant -> Maybe Text
showVariant = A.decode . A.encode

-- common for on_update & on_status
mkStopsOUS :: DBooking.Booking -> DRide.Ride -> Text -> Maybe [Spec.Stop]
mkStopsOUS booking ride rideOtp =
  let origin = booking.fromLocation
      mbDestination = booking.toLocation
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
                  stopAuthorization =
                    Just $
                      Spec.Authorization
                        { authorizationToken = Just rideOtp,
                          authorizationType = Just $ show Enums.OTP
                        },
                  stopTime = ride.tripStartTime <&> \tripStartTime' -> Spec.Time {timeTimestamp = Just tripStartTime', timeDuration = Nothing}
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
                    stopTime = ride.tripEndTime <&> \tripEndTime' -> Spec.Time {timeTimestamp = Just tripEndTime', timeDuration = Nothing}
                  }
            )
              <$> mbDestination
          ]

type IsValueAddNP = Bool

-- common for on_update & on_status
mkFulfillmentV2 ::
  (MonadFlow m, EncFlow m r) =>
  Maybe SP.Person ->
  DRide.Ride ->
  DBooking.Booking ->
  Maybe DVeh.Vehicle ->
  Maybe Text ->
  Maybe [Spec.TagGroup] ->
  Maybe [Spec.TagGroup] ->
  Bool ->
  Bool ->
  Maybe Text ->
  IsValueAddNP ->
  m Spec.Fulfillment
mkFulfillmentV2 mbDriver ride booking mbVehicle mbImage mbTags mbPersonTags isDriverBirthDay isFreeRide mbEvent isValueAddNP = do
  mbDInfo <- driverInfo
  let rideOtp = fromMaybe ride.otp ride.endOtp
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just ride.id.getId,
        fulfillmentStops = mkStopsOUS booking ride rideOtp,
        fulfillmentType = Just $ mkFulfillmentType booking.tripCategory,
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
                  vehicleMake = Nothing
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
    driverInfo = forM mbDriver $ \driver -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = mkDriverDetailsTags driver isDriverBirthDay isFreeRide
      pure $
        DriverInfo
          { mobileNumber = dPhoneNum,
            name = dName,
            tags = if isValueAddNP then dTags else Nothing
          }

mkDriverDetailsTags :: SP.Person -> Bool -> Bool -> Maybe [Spec.TagGroup]
mkDriverDetailsTags driver isDriverBirthDay isFreeRide =
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
      | isNothing driver.rating = []
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
              tagValue = show <$> driver.rating
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

mapVariantToVehicle :: Variant.Variant -> VehicleCategory
mapVariantToVehicle variant =
  case variant of
    Variant.SEDAN -> CAB
    Variant.HATCHBACK -> CAB
    Variant.TAXI -> CAB
    Variant.SUV -> CAB
    Variant.TAXI_PLUS -> CAB
    Variant.AUTO_RICKSHAW -> AUTO_RICKSHAW

mapRideStatus :: Maybe DRide.RideStatus -> Enums.FulfillmentState
mapRideStatus rideStatus =
  case rideStatus of
    Just DRide.NEW -> Enums.RIDE_ASSIGNED
    Just DRide.INPROGRESS -> Enums.RIDE_STARTED
    Just DRide.COMPLETED -> Enums.RIDE_ENDED
    Just DRide.CANCELLED -> Enums.RIDE_CANCELLED
    Nothing -> Enums.RIDE_ASSIGNED

tfCancellationFee :: Maybe Int -> Maybe Spec.Fee
tfCancellationFee Nothing = Nothing
tfCancellationFee amount = do
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
            priceCurrency = Just "INR",
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just $ encodeToText amount
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
      { quotationBreakup = mkQuotationBreakup booking,
        quotationPrice = tfQuotationPrice booking,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DBooking.Booking -> Maybe Spec.Price
tfQuotationPrice booking =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText booking.estimatedFare,
        priceValue = Just $ encodeToText booking.estimatedFare
      }

mkQuotationBreakup :: DBooking.Booking -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup booking =
  let fareParams = mkFareParamsBreakups mkPrice mkQuotationBreakupInner booking.fareParams
   in Just $ filter (filterRequiredBreakups $ DFParams.getFareParametersType booking.fareParams) fareParams -- TODO: Remove after roll out
  where
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

type MerchantShortId = Text

tfItems :: DBooking.Booking -> MerchantShortId -> Maybe Meters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> Maybe [Spec.Item]
tfItems booking shortId estimatedDistance mbFarePolicy mbPaymentId =
  Just
    [ Spec.Item
        { itemDescriptor = tfItemDescriptor booking,
          itemFulfillmentIds = Just [booking.quoteId],
          itemId = Just $ Common.mkItemId shortId booking.vehicleVariant,
          itemLocationIds = Nothing,
          itemPaymentIds = tfPaymentId mbPaymentId,
          itemPrice = tfItemPrice booking,
          itemTags = mkRateCardTag estimatedDistance mbFarePolicy
        }
    ]

tfPaymentId :: Maybe Text -> Maybe [Text]
tfPaymentId mbPaymentId = do
  paymentId <- mbPaymentId
  Just [paymentId]

tfItemPrice :: DBooking.Booking -> Maybe Spec.Price
tfItemPrice booking =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText booking.estimatedFare, -- TODO : Remove this and make non mandatory on BAP side
        priceValue = Just $ encodeToText booking.estimatedFare
      }

tfItemDescriptor :: DBooking.Booking -> Maybe Spec.Descriptor
tfItemDescriptor booking =
  Just
    Spec.Descriptor
      { descriptorCode = Just "RIDE",
        descriptorShortDesc = Just $ show booking.vehicleVariant,
        descriptorName = Just $ show booking.vehicleVariant
      }

convertEstimateToPricing :: (DEst.Estimate, Maybe NearestDriverInfo) -> Pricing
convertEstimateToPricing (DEst.Estimate {..}, mbDriverLocations) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = maxFare,
      pricingMinFare = minFare,
      fulfillmentType = show Enums.DELIVERY,
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      ..
    }

convertQuoteToPricing :: (DQuote.Quote, Maybe NearestDriverInfo) -> Pricing
convertQuoteToPricing (DQuote.Quote {..}, mbDriverLocations) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = estimatedFare,
      pricingMinFare = estimatedFare,
      estimatedDistance = distance,
      fareParams = Just fareParams,
      fulfillmentType = mapToFulfillmentType tripCategory,
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      ..
    }
  where
    mapToFulfillmentType (DTC.OneWay DTC.OneWayRideOtp) = show Enums.RIDE_OTP
    mapToFulfillmentType (DTC.RoundTrip DTC.RideOtp) = show Enums.RIDE_OTP
    mapToFulfillmentType (DTC.RideShare DTC.RideOtp) = show Enums.RIDE_OTP
    mapToFulfillmentType (DTC.Rental _) = show Enums.RENTAL
    mapToFulfillmentType (DTC.InterCity _) = show Enums.INTER_CITY
    mapToFulfillmentType _ = show Enums.RIDE_OTP -- backward compatibility

convertBookingToPricing :: DBooking.Booking -> Pricing
convertBookingToPricing DBooking.Booking {..} =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = estimatedFare,
      pricingMinFare = estimatedFare,
      vehicleVariant = vehicleVariant,
      tripCategory = tripCategory,
      fareParams = Just fareParams,
      farePolicy = Nothing,
      fulfillmentType = show Enums.DELIVERY,
      distanceToNearestDriver = Nothing,
      ..
    }

mkGeneralInfoTagGroup :: Pricing -> Maybe Spec.TagGroup
mkGeneralInfoTagGroup pricing
  | isNothing pricing.specialLocationTag && isNothing pricing.distanceToNearestDriver = Nothing
  | otherwise =
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
          tagGroupList = specialLocationTagSingleton pricing.specialLocationTag <> distanceToNearestDriverTagSingleton pricing.distanceToNearestDriver
        }
  where
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

mkRateCardTag :: Maybe Meters -> Maybe FarePolicyD.FarePolicy -> Maybe [Spec.TagGroup]
mkRateCardTag estimatedDistance farePolicy = do
  let farePolicyBreakups = maybe [] (mkFarePolicyBreakups Prelude.id mkRateCardBreakupItem estimatedDistance) farePolicy
      farePolicyBreakupsTags = buildRateCardTags <$> farePolicyBreakups
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

tfCancellationTerms :: DBC.BecknConfig -> [Spec.CancellationTerm]
tfCancellationTerms becknConfig =
  List.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = tfCancellationFee becknConfig.cancellationFeeAmount,
        cancellationTermFulfillmentState = Nothing,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }

tfPayments :: DBooking.Booking -> DM.Merchant -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments booking transporter bppConfig = do
  let mPrice = Just $ mkPriceFromMoney booking.estimatedFare -- FIXME
  let mkParams :: Maybe BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . List.singleton $ mkPayment (show transporter.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

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
