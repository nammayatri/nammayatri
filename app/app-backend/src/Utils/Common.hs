{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common where

import App.Types
import Beckn.Types.App
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Location
import Beckn.Types.Core.Person
import Beckn.Types.Core.Price
import Beckn.Types.Core.Provider
import Beckn.Types.Core.Tracking
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Traveller
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (encodeToText')
import qualified Beckn.Utils.Extra as Utils
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Data.ByteString.Lazy as BSL
import Data.Text as DT
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client.Core.ClientError
import Servant.Client.Core.Response
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Test.RandomStrings as RS
import qualified Types.API.Search as API
import qualified Types.Common as API

-- | Performs simple token verification.
type TokenAuth = TokenAuth' "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPerson :: RegToken -> Flow Person.Person
verifyPerson token = do
  sr <- Utils.Common.verifyToken token
  Person.findById (PersonId $ SR._EntityId sr)
    >>= fromMaybeM500 "Could not find user"

verifyPersonAction :: VerificationAction VerifyToken AppEnv
verifyPersonAction = VerificationAction Utils.Common.verifyPerson

verifyToken :: RegToken -> Flow SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token
    >>= fromMaybeM400 "INVALID_TOKEN"
    >>= validateToken

validateToken :: SR.RegistrationToken -> Flow SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ _tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal _updatedAt
  when expired (L.throwException $ err400 {errBody = "TOKEN_EXPIRED"})
  return sr

fromMaybeM :: ServerError -> Maybe a -> Flow a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a) = return a

fromMaybeM400, fromMaybeM500, fromMaybeM503 :: BSL.ByteString -> Maybe a -> Flow a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

generateShortId :: Flow Text
generateShortId = T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

-- TODO: figure out a way to extract the url directly from EulerClient
callAPI baseUrl req serviceName = do
  endTracking <- L.runUntracedIO $ Metrics.startTracking (encodeToText' baseUrl) serviceName
  res <- L.callAPI baseUrl req
  let status = case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"
  _ <- L.runUntracedIO $ endTracking status
  return res

mkContext :: Text -> Text -> UTCTime -> Maybe Text -> Context
mkContext action rtid utcTime acId =
  Context
    { _domain = "MOBILITY",
      _country = Nothing,
      _city = Nothing,
      _action = action,
      _core_version = Just "0.8.0",
      _domain_version = Nothing,
      _ac_id = acId,
      _transaction_id = rtid,
      _message_id = rtid,
      _timestamp = utcTime
    }

mkIntent :: API.SearchReq -> Intent
mkIntent req =
  Intent
    { _query_string = Nothing,
      _provider_id = Nothing,
      _category_id = Nothing,
      _item_id = Nothing,
      _tags = [],
      _origin = fromAPIStopToStop $ req ^. #origin,
      _destination = fromAPIStopToStop $ req ^. #destination,
      _stops = [],
      _vehicle = fromAPIVehicleToVehicle $ req ^. #vehicle,
      _payload = Payload Nothing Nothing [] Nothing,
      _transfer = Nothing,
      _fare = mkPrice $ req ^. #fare
    }

mkPrice :: API.DecimalValue -> Price
mkPrice value =
  Price
    { _currency = "INR",
      _value = Just $ DecimalValue (value ^. #integral) (value ^. #fractional),
      _estimated_value = Nothing,
      _computed_value = Nothing,
      _listed_value = Nothing,
      _offered_value = Nothing,
      _minimum_value = Nothing,
      _maximum_value = Nothing
    }

fromVehicleCategoryToText :: API.VehicleCategory -> Text
fromVehicleCategoryToText category =
  case category of
    API.CAR -> "CAR"
    API.MOTORCYCLE -> "MOTORCYCLE"
    API.BICYCLE -> "BICYCLE"
    API.OTHER -> "OTHER"
    API.TRUCK -> "TRUCK"

fromTextToVehiceCategory :: Text -> API.VehicleCategory
fromTextToVehiceCategory category =
  case category of
    "CAR" -> API.CAR
    "MOTORCYCLE" -> API.MOTORCYCLE
    "BICYCLE" -> API.BICYCLE
    "OTHER" -> API.OTHER
    "TRUCK" -> API.TRUCK
    _ -> API.OTHER

fromAPIVehicleToVehicle :: API.Vehicle -> Vehicle
fromAPIVehicleToVehicle vehicle =
  Vehicle
    { category = fromVehicleCategoryToText <$> vehicle ^. #category,
      capacity = vehicle ^. #capacity,
      make = Nothing,
      model = vehicle ^. #model,
      size = Nothing,
      variant = vehicle ^. #variant,
      color = Nothing,
      energy_type = Nothing,
      registration = Nothing
    }

fromVehicleToAPIVehicle :: Vehicle -> API.Vehicle
fromVehicleToAPIVehicle vehicle =
  API.Vehicle
    { category = fromTextToVehiceCategory <$> vehicle ^. #category,
      capacity = vehicle ^. #capacity,
      model = vehicle ^. #model,
      variant = vehicle ^. #variant,
      registrationNumber = number <$> (vehicle ^. #registration)
    }

fromAPIStopToStop :: API.Stop -> Stop
fromAPIStopToStop stop =
  Stop
    { _id = "",
      _descriptor = Nothing,
      _location = fromAPILocToLocation $ stop ^. #location,
      _arrival_time = StopTime (stop ^. #arrivalTime . #estimated) (stop ^. #arrivalTime . #actual),
      _departure_time = StopTime (stop ^. #departureTime . #estimated) (stop ^. #departureTime . #actual),
      _transfers = []
    }

fromStopToAPIStop :: Stop -> API.Stop
fromStopToAPIStop stop =
  API.Stop
    { location = fromLocationToAPILocation $ stop ^. #_location,
      arrivalTime = API.StopTime (stop ^. #_arrival_time . #_est) (stop ^. #_arrival_time . #_act),
      departureTime = API.StopTime (stop ^. #_departure_time . #_est) (stop ^. #_departure_time . #_act)
    }

fromLocationToAPILocation :: Location -> API.Location
fromLocationToAPILocation loc =
  API.Location
    { locType = loc ^. #_type,
      gps = fromGPSToAPIGPS <$> loc ^. #_gps,
      address = fromAddressToAPIAddr <$> loc ^. #_address,
      areaCode = loc ^. #_area_code,
      city = fromCityToAPICity <$> loc ^. #_city
    }

fromAPILocToLocation :: API.Location -> Location
fromAPILocToLocation loc =
  Location
    { _type = loc ^. #locType,
      _gps = fromAPIGPSToGPS <$> loc ^. #gps,
      _address = fromAPIAddrToAddress <$> loc ^. #address,
      _station_code = Nothing,
      _area_code = loc ^. #areaCode,
      _city = fromAPICityToCity <$> loc ^. #city,
      _country = Nothing,
      _circle = Nothing,
      _polygon = Nothing,
      _3dspace = Nothing
    }

fromGPSToAPIGPS :: GPS -> API.GPS
fromGPSToAPIGPS gps =
  API.GPS
    { lat = gps ^. #lat,
      lon = gps ^. #lon
    }

fromAPIGPSToGPS :: API.GPS -> GPS
fromAPIGPSToGPS gps =
  GPS
    { lat = gps ^. #lat,
      lon = gps ^. #lon
    }

fromAPICityToCity :: API.City -> City
fromAPICityToCity (API.City cityName) =
  City
    { name = cityName,
      code = ""
    }

fromCityToAPICity :: City -> API.City
fromCityToAPICity city =
  API.City $ city ^. #name

fromAPIAddrToAddress :: API.Address -> Address
fromAPIAddrToAddress addr =
  Address
    { door = addr ^. #door,
      building = addr ^. #door,
      street = addr ^. #street,
      area = addr ^. #area,
      city = addr ^. #city,
      country = addr ^. #country,
      area_code = addr ^. #areaCode
    }

fromAddressToAPIAddr :: Address -> API.Address
fromAddressToAPIAddr addr =
  API.Address
    { door = addr ^. #door,
      building = addr ^. #building,
      street = addr ^. #street,
      area = addr ^. #area,
      city = addr ^. #city,
      country = addr ^. #country,
      areaCode = addr ^. #area_code
    }

fromTripToAPITrip :: Trip -> API.Trip
fromTripToAPITrip trip =
  let mbPrice = trip ^. #fare
      mbFare = case mbPrice of
        Nothing -> Nothing
        Just p -> p ^. #_value
   in API.Trip
        { id = trip ^. #id,
          origin = fromStopToAPIStop <$> trip ^. #origin,
          destination = fromStopToAPIStop <$> trip ^. #destination,
          vehicle = fromVehicleToAPIVehicle <$> trip ^. #vehicle,
          driver = fromDriverToAPIDriver <$> trip ^. #driver,
          travellers = [],
          fare = fromDecimalToAPIDecimal <$> mbFare
        }

fromAPITripToTrip :: API.Trip -> Trip
fromAPITripToTrip trip =
  Trip
    { id = trip ^. #id,
      origin = fromAPIStopToStop <$> trip ^. #origin,
      destination = fromAPIStopToStop <$> trip ^. #destination,
      vehicle = fromAPIVehicleToVehicle <$> trip ^. #vehicle,
      driver = fromAPIDriverToDriver <$> trip ^. #driver,
      payload = fromAPITravellerToPayload $ trip ^. #travellers,
      fare = fromAPIFareToPrice <$> trip ^. #fare,
      route = Nothing
    }

fromAPIFareToPrice :: API.DecimalValue -> Price
fromAPIFareToPrice value =
  Price
    { _currency = "INR",
      _value = Just $ fromAPIDecimalToDecimal value,
      _estimated_value = Nothing,
      _computed_value = Nothing,
      _listed_value = Nothing,
      _offered_value = Nothing,
      _minimum_value = Nothing,
      _maximum_value = Nothing
    }

fromAPITravellerToPayload :: [API.Traveller] -> Payload
fromAPITravellerToPayload travellers =
  Payload
    { _luggage = Nothing,
      _traveller_count = Nothing,
      _travellers = fromAPITravellerToTraveller <$> travellers,
      _travel_group = Nothing
    }

fromAPITravellerToTraveller :: API.Traveller -> Traveller
fromAPITravellerToTraveller traveller =
  Traveller
    { _name = Name Nothing Nothing (traveller ^. #name) Nothing Nothing Nothing,
      _image = Nothing,
      _dob = Nothing,
      _organization_name = Nothing,
      _gender = traveller ^. #gender,
      _email = Nothing,
      _phones = traveller ^. #phones,
      _origin_stop_id = "",
      _destination_stop_id = ""
    }

fromDriverToAPIDriver :: Driver -> API.Driver
fromDriverToAPIDriver driver =
  API.Driver
    { name = driver ^. #name . #_given_name,
      gender = driver ^. #gender,
      phones = driver ^. #phones
    }

fromAPIDriverToDriver :: API.Driver -> Driver
fromAPIDriverToDriver driver =
  Driver
    { name = Name Nothing Nothing (driver ^. #name) Nothing Nothing Nothing,
      image = Nothing,
      dob = Nothing,
      organization_name = Nothing,
      gender = driver ^. #gender,
      email = Nothing,
      phones = driver ^. #phones,
      experience = Nothing,
      rating = Nothing
    }

fromDecimalToAPIDecimal :: DecimalValue -> API.DecimalValue
fromDecimalToAPIDecimal value =
  API.DecimalValue
    { integral = value ^. #_integral,
      fractional = value ^. #_fractional
    }

fromAPIDecimalToDecimal :: API.DecimalValue -> DecimalValue
fromAPIDecimalToDecimal value =
  DecimalValue
    { _integral = value ^. #integral,
      _fractional = value ^. #fractional
    }

fromTrackingToAPITracking :: Tracking -> API.Tracking
fromTrackingToAPITracking tracking =
  API.Tracking
    { url = tracking ^. #_url
    }

fromAPITrackingToTracking :: API.Tracking -> Tracking
fromAPITrackingToTracking tracking =
  Tracking
    { _url = tracking ^. #url,
      _required_params = Nothing,
      _metadata = Nothing
    }

fromProviderToAPIProvider :: Provider -> API.Provider
fromProviderToAPIProvider provider =
  API.Provider
    { id = provider ^. #_id,
      name = provider ^. #_descriptor . #_name
    }
