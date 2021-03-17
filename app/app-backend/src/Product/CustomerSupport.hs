{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import Storage.Queries.Person as Person
import Storage.Queries.ProductInstance as PI
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.CustomerSupport as T
import Types.ProductInfo as ProductInfo

login :: T.LoginReq -> FlowHandler T.LoginRes
login T.LoginReq {..} = withFlowHandler $ do
  personM <- Person.findByUsernameAndPassword _email _password
  case personM of
    Nothing -> throwError401 "INVALID_CREDENTIALS"
    Just person ->
      if person ^. #_status /= SP.ACTIVE && person ^. #_role /= SP.CUSTOMER_SUPPORT
        then throwError401 "INVALID_CREDENTIALS"
        else do
          token <- generateToken person
          pure $ T.LoginRes token "Logged in successfully"

generateToken :: SP.Person -> Flow Text
generateToken SP.Person {..} = do
  let personId = getId _id
  regToken <- createSupportRegToken personId
  -- Clean Old Login Session
  RegistrationToken.deleteByPersonId personId
  RegistrationToken.create regToken
  pure $ regToken ^. #_token

logout :: SP.Person -> FlowHandler T.LogoutRes
logout person =
  withFlowHandler $
    if person ^. #_role /= SP.CUSTOMER_SUPPORT
      then throwError401 "UNAUTHORIZED" -- Do we need this Check?
      else do
        RegistrationToken.deleteByPersonId (getId $ person ^. #_id)
        pure $ T.LogoutRes "Logged out successfully"

createSupportRegToken :: Text -> Flow SR.RegistrationToken
createSupportRegToken entityId = do
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrTime
  return $
    SR.RegistrationToken
      { _id = rtid,
        _token = token,
        _attempts = 1, -- Token
        _authMedium = SR.EMAIL,
        _authType = SR.PASSWORD,
        _authValueHash = "CUSTOMER_SESSIONTOKEN",
        _verified = False,
        _authExpiry = 0,
        _tokenExpiry = 30, -- Need to Make this Configuable
        _EntityId = entityId,
        _entityType = SR.CUSTOMER,
        _createdAt = now,
        _updatedAt = now,
        _info = Nothing
      }

listOrder :: SP.Person -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler [T.OrderResp]
listOrder supportP mCaseId mMobile mlimit moffset =
  withFlowHandler $
    if supportP ^. #_role /= SP.ADMIN && supportP ^. #_role /= SP.CUSTOMER_SUPPORT
      then throwError403 "FORBIDDEN"
      else do
        T.OrderInfo {person, searchcases} <- case (mCaseId, mMobile) of
          (Just caseId, _) -> getByCaseId caseId
          (_, Just mobileNumber) -> getByMobileNumber mobileNumber
          (_, _) -> throwError400 "INVALID_REQUEST"
        traverse (makeCaseToOrder person) searchcases
  where
    getByMobileNumber number = do
      let limit = maybe 10 (\x -> if x <= 10 then x else 10) mlimit
      person <-
        Person.findByRoleAndMobileNumberWithoutCC SP.USER number
          >>= fromMaybeM400 "INVALID_MOBILE_NUMBER"
      searchcases <-
        Case.findAllByTypeAndStatuses (person ^. #_id) C.RIDESEARCH [C.NEW, C.INPROGRESS, C.CONFIRMED, C.COMPLETED, C.CLOSED] (Just limit) moffset
          >>= either DB.throwDBError pure
      return $ T.OrderInfo person searchcases
    getByCaseId caseId = do
      (_case :: C.Case) <-
        Case.findByIdAndType (Id caseId) C.RIDESEARCH
          >>= either DB.throwDBError pure
          >>= fromMaybeM400 "INVALID_ORDER_ID"
      let personId = fromMaybe "_ID" (_case ^. #_requestor)
      person <-
        Person.findById (Id personId)
          >>= fromMaybeM400 "INVALID_CUSTOMER_ID"
      return $ T.OrderInfo person [_case]

makeCaseToOrder :: SP.Person -> C.Case -> Flow T.OrderResp
makeCaseToOrder SP.Person {_fullName, _mobileNumber} C.Case {..} = do
  (confiremedOrder :: Maybe C.Case) <-
    Case.findOneByParentIdAndCaseType _id C.RIDEORDER
      >>= either DB.throwDBError pure
  let (status :: Maybe CaseStatus) = ((\x -> Just $ x ^. #_status) =<< confiremedOrder) <|> Just _status
  fromLocation <- Location.findLocationById $ Id _fromLocationId
  toLocation <- Location.findLocationById $ Id _toLocationId
  trip <- makeTripDetails confiremedOrder
  --  Info: udf1 is vechicle variant
  let details =
        T.OrderDetails
          { _id = getId _id,
            _status = status,
            _createdAt = _createdAt,
            _updatedAt = _updatedAt,
            _startTime = _startTime,
            _endTime = _endTime,
            _fromLocation = fromLocation,
            _toLocation = toLocation,
            _travellerName = _fullName,
            _travellerPhone = _mobileNumber,
            _vehicleVariant = _udf1, -- Note: UDF1 Contain _vehicleVariant info
            _trip = trip
          }
  pure $ T.OrderResp {_order = details}

makeTripDetails :: Maybe C.Case -> Flow (Maybe T.TripDetails)
makeTripDetails caseM = case caseM of
  Nothing -> pure Nothing
  Just _case -> do
    -- Note: In case of Confirmed Order only one Product Instance will be Present
    ProductInstance.ProductInstance {_id, _status, _info, _price} <-
      head
        <$> ( PI.findAllByCaseId (_case ^. #_id)
                >>= either DB.throwDBError pure
            )
    let (mproductInfo :: Maybe ProductInfo) = decodeFromText =<< _info
        provider = (\x -> x ^. #_provider) =<< mproductInfo
        mtracker = (\x -> x ^. #_tracker) =<< mproductInfo
        mtrip = (\x -> Just $ x ^. #_trip) =<< mtracker
        driver = (\x -> x ^. #driver) =<< mtrip
        vehicle = (\x -> x ^. #vehicle) =<< mtrip
    pure $
      Just $
        T.TripDetails
          { _id = getId _id,
            _status = _status,
            _driver = driver,
            _price = _price,
            _provider = provider,
            _vehicle = vehicle
          }
