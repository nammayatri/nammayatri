{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
import EulerHS.Prelude
import Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import Storage.Queries.Person as Person
import Beckn.External.Encryption
import qualified EulerHS.Language as L
import Storage.Queries.ProductInstance as PI
import Types.API.CustomerSupport as T
import Types.ProductInfo as ProductInfo

listOrder :: SP.Person -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler [T.OrderResp]
listOrder supportP mCaseId mMobile mlimit moffset =
  withFlowHandler $ 
    if supportP ^. #_role /= SP.ADMIN && supportP ^. #_role /= SP.CUSTOMER_SUPPORT
      then throwError403 "Forbidden"
      else do
        T.OrderInfo {person, searchcases} <- case (mCaseId, mMobile) of
          (Just caseId, _) -> getByCaseId caseId
          (_, Just mobileNumber) -> getByMobileNumber mobileNumber
          (_, _) -> throwError400 "No CaseId or Mobile Number in Request"
        traverse (makeCaseToOrder person) searchcases
  where
    getByMobileNumber number = do
      --TODO: Limit max value should be 10
      person <-
        Person.findByRoleAndMobileNumberWithoutCC SP.USER number
          >>= fromMaybeM400 "Invalid MobileNumber"
      searchcases <-
        Case.findAllByTypeAndStatuses (person ^. #_id) C.RIDESEARCH [C.NEW, C.INPROGRESS, C.CONFIRMED, C.COMPLETED, C.CLOSED] mlimit moffset
          >>= either DB.throwDBError pure
      return $ T.OrderInfo person searchcases
    getByCaseId caseId = do
      (_case :: C.Case) <-
        Case.findByIdAndType (CaseId caseId) C.RIDESEARCH
          >>= either DB.throwDBError pure
          >>= fromMaybeM400 "Invalid OrderId"
      let personId = fromMaybe "_ID" (_case ^. #_requestor)
      person <-
        Person.findById (PersonId personId)
          >>= fromMaybeM400 "Invalid CustomerId"
      return $ T.OrderInfo person [_case]

makeCaseToOrder :: SP.Person -> C.Case -> Flow T.OrderResp
makeCaseToOrder SP.Person {_fullName, _mobileNumber} C.Case {..} = do
  (confiremedOrder :: Maybe C.Case) <-
    Case.findOneByParentIdAndCaseType _id C.RIDEORDER
      >>= either DB.throwDBError pure
  let (status :: Maybe CaseStatus) = maybe Nothing (\x -> Just $ x ^. #_status) confiremedOrder <|> (Just _status)
  fromLocation <- Location.findLocationById $ LocationId _fromLocationId
  toLocation <- Location.findLocationById $ LocationId _toLocationId
  trip <-  makeTripDetails confiremedOrder
  --  Info: udf1 is vechicle variant
  let details =
        T.OrderDetails
          { _id = (_getCaseId _id),
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
    pure $ Just $
      T.TripDetails
        { _id = _getProductInstanceId _id,
          _status = _status,
          _driver = driver,
          _price = _price,
          _provider = provider,
          _vehicle = vehicle
        }
