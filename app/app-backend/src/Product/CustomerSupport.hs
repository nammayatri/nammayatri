{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.SearchReqLocation as SSearchLoc
import Domain.Types.SearchRequest as C
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Product.RideBooking (buildRideBookingStatusRes)
import Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.SearchReqLocation as Location
import Storage.Queries.SearchRequest as SearchRequest
import Types.API.CustomerSupport as T
import Types.Error
import Utils.Common

login :: T.LoginReq -> FlowHandler T.LoginRes
login T.LoginReq {..} = withFlowHandlerAPI $ do
  person <- Person.findByEmailAndPassword email password >>= fromMaybeM PersonNotFound
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  token <- generateToken person
  pure $ T.LoginRes token "Logged in successfully"

generateToken :: EsqDBFlow m r => SP.Person -> m Text
generateToken SP.Person {..} = do
  let personId = id
  regToken <- createSupportRegToken $ getId personId
  -- Clean Old Login Session
  DB.runTransaction $ do
    RegistrationToken.deleteByPersonId personId
    RegistrationToken.create regToken
  pure $ regToken.token

logout :: Id SP.Person -> FlowHandler T.LogoutRes
logout personId = withFlowHandlerAPI $ do
  person <- Person.findById personId >>= fromMaybeM PersonNotFound
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  DB.runTransaction (RegistrationToken.deleteByPersonId person.id)
  pure $ T.LogoutRes "Logged out successfully"

createSupportRegToken :: MonadFlow m => Text -> m SR.RegistrationToken
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
        verified = True,
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
      let requestorId = searchRequest.riderId
      person <-
        Person.findById requestorId
          >>= fromMaybeM PersonDoesNotExist
      return $ T.OrderInfo person [searchRequest]

makeSearchRequestToOrder :: (EsqDBFlow m r, EncFlow m r) => SP.Person -> C.SearchRequest -> m T.OrderResp
makeSearchRequestToOrder SP.Person {firstName, lastName, mobileNumber} C.SearchRequest {..} = do
  fromLocation <- Location.findById fromLocationId
  toLocation <- Location.findById toLocationId
  rideBooking <- QRB.findByRequestId id
  rbStatus <- buildRideBookingStatusRes `mapM` rideBooking
  decMobNum <- mapM decrypt mobileNumber
  let details =
        T.OrderDetails
          { id = getId id,
            createdAt = createdAt,
            updatedAt = createdAt,
            startTime = startTime,
            endTime = Nothing,
            fromLocation = SSearchLoc.makeSearchReqLocationAPIEntity <$> fromLocation,
            toLocation = SSearchLoc.makeSearchReqLocationAPIEntity <$> toLocation,
            travellerName = firstName <> lastName,
            travellerPhone = decMobNum,
            rideBooking = rbStatus
          }
  pure $ T.OrderResp {order = details}
