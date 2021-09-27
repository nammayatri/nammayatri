{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Rating
import Beckn.Types.Id
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.SearchReqLocation as Location
import Storage.Queries.SearchRequest as SearchRequest
import Types.API.CustomerSupport as T
import Types.Common
import Types.Error
import Types.Storage.Person as SP
import qualified Types.Storage.RegistrationToken as SR
import qualified Types.Storage.SearchReqLocation as SSearchLoc
import Types.Storage.SearchRequest as C
import Utils.Common

login :: T.LoginReq -> FlowHandler T.LoginRes
login T.LoginReq {..} = withFlowHandlerAPI $ do
  person <- Person.findByEmailAndPassword email password >>= fromMaybeM PersonNotFound
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  token <- generateToken person
  pure $ T.LoginRes token "Logged in successfully"

generateToken :: DBFlow m r => SP.Person -> m Text
generateToken SP.Person {..} = do
  let personId = id
  regToken <- createSupportRegToken $ getId personId
  -- Clean Old Login Session
  DB.runSqlDBTransaction $ do
    RegistrationToken.deleteByPersonId personId
    RegistrationToken.create regToken
  pure $ regToken.token

logout :: Id SP.Person -> FlowHandler T.LogoutRes
logout personId = withFlowHandlerAPI $ do
  person <- Person.findById personId >>= fromMaybeM PersonNotFound
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  DB.runSqlDB (RegistrationToken.deleteByPersonId person.id)
  pure $ T.LogoutRes "Logged out successfully"

createSupportRegToken :: DBFlow m r => Text -> m SR.RegistrationToken
createSupportRegToken entityId = do
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = 1, -- Token
        authMedium = SR.EMAIL,
        authType = SR.PASSWORD,
        authValueHash = "CUSTOMER_SESSIONTOKEN",
        verified = False,
        authExpiry = 0,
        tokenExpiry = 30, -- Need to Make this Configuable
        entityId = entityId,
        entityType = SR.CUSTOMER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

listOrder :: Id SP.Person -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler [T.OrderResp]
listOrder personId mRequestId mMobile mlimit moffset = withFlowHandlerAPI $ do
  supportP <- Person.findById personId >>= fromMaybeM PersonNotFound
  unless (supportP.role == SP.CUSTOMER_SUPPORT) $
    throwError AccessDenied
  T.OrderInfo {person, searchRequests} <- case (mRequestId, mMobile) of
    (Just searchRequestId, _) -> getByRequestId searchRequestId
    (_, Just mobileNumber) -> getByMobileNumber mobileNumber
    (_, _) -> throwError $ InvalidRequest "You should pass SearchRequestId or mobile number."
  traverse (makeSearchRequestToOrder person) searchRequests
  where
    getByMobileNumber number = do
      let limit = maybe 10 (\x -> if x <= 10 then x else 10) mlimit
      person <-
        Person.findByRoleAndMobileNumberWithoutCC SP.USER number
          >>= fromMaybeM PersonDoesNotExist
      searchRequests <-
        SearchRequest.findAllByPersonIdLimitOffset (person.id) (Just limit) moffset
      return $ T.OrderInfo person searchRequests
    getByRequestId searchRequestId = do
      (searchRequest :: C.SearchRequest) <-
        SearchRequest.findById (Id searchRequestId)
          >>= fromMaybeM SearchRequestDoesNotExist
      let requestorId = searchRequest.requestorId
      person <-
        Person.findById requestorId
          >>= fromMaybeM PersonDoesNotExist
      return $ T.OrderInfo person [searchRequest]

makeSearchRequestToOrder :: (DBFlow m r, EncFlow m r) => SP.Person -> C.SearchRequest -> m T.OrderResp
makeSearchRequestToOrder SP.Person {fullName, mobileNumber} C.SearchRequest {..} = do
  (confiremedOrder :: Maybe C.SearchRequest) <- SearchRequest.findById id
  fromLocation <- Location.findLocationById fromLocationId
  toLocation <- Location.findLocationById toLocationId
  trip <- makeTripDetails confiremedOrder
  decMobNum <- decrypt mobileNumber
  --  Info: udf1 is vechicle variant
  let details =
        T.OrderDetails
          { id = getId id,
            createdAt = createdAt,
            updatedAt = createdAt,
            startTime = startTime,
            endTime = Nothing,
            fromLocation = SSearchLoc.makeSearchReqLocationAPIEntity <$> fromLocation,
            toLocation = SSearchLoc.makeSearchReqLocationAPIEntity <$> toLocation,
            travellerName = fullName,
            travellerPhone = decMobNum,
            vehicleVariant = Just $ show vehicleVariant, -- Note: UDF1 Contain vehicleVariant info
            trip = trip
          }
  pure $ T.OrderResp {order = details}

makeTripDetails :: DBFlow m r => Maybe C.SearchRequest -> m (Maybe T.TripDetails)
makeTripDetails mbSearchRequest = case mbSearchRequest of
  Nothing -> pure Nothing
  Just searchRequest -> do
    rideBooking <- QRB.findByRequestId (searchRequest.id) >>= fromMaybeM RideBookingNotFound
    org <- QOrg.findOrganizationById rideBooking.providerId >>= fromMaybeM OrgNotFound
    mbRide <- QRide.findByRBId rideBooking.id
    let provider =
          Provider
            { id = getId rideBooking.providerId,
              name = Just org.name,
              phones = [rideBooking.providerMobileNumber],
              info = Nothing
            }
        driver =
          mbRide <&> \ride ->
            Driver
              { name = ride.driverName,
                gender = "",
                phones = [ride.driverMobileNumber],
                rating =
                  ride.driverRating <&> \rating ->
                    Rating
                      { value = show rating,
                        unit = "U+2B50",
                        max_value = Nothing,
                        direction = Nothing
                      },
                registeredAt = ride.driverRegisteredAt
              }
        vehicle =
          mbRide <&> \ride ->
            Vehicle
              { category = Nothing,
                capacity = Nothing,
                make = Nothing,
                model = Just ride.vehicleModel,
                size = Nothing,
                variant = show searchRequest.vehicleVariant,
                color = Just ride.vehicleColor,
                registrationNumber = Just ride.vehicleNumber
              }
    pure $
      Just $
        T.TripDetails
          { id = getId rideBooking.id,
            status = rideBooking.status,
            driver = driver,
            price = Just rideBooking.price,
            provider = Just provider,
            vehicle = vehicle
          }
