{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Storage.Queries.Case as Case
import Storage.Queries.Person as Person
import Storage.Queries.ProductInstance as PI
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Storage.Queries.SearchReqLocation as Location
import Types.API.CustomerSupport as T
import Types.Error
import Types.ProductInfo as ProductInfo
import Types.Storage.Case as C
import Types.Storage.Person as SP
import qualified Types.Storage.RegistrationToken as SR
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
      { id = rtid,
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
listOrder personId mCaseId mMobile mlimit moffset = withFlowHandlerAPI $ do
  supportP <- Person.findById personId >>= fromMaybeM PersonNotFound
  unless (supportP.role == SP.CUSTOMER_SUPPORT) $
    throwError AccessDenied
  T.OrderInfo {person, searchcases} <- case (mCaseId, mMobile) of
    (Just caseId, _) -> getByCaseId caseId
    (_, Just mobileNumber) -> getByMobileNumber mobileNumber
    (_, _) -> throwError $ InvalidRequest "You should pass CaseId or mobile number."
  traverse (makeCaseToOrder person) searchcases
  where
    getByMobileNumber number = do
      let limit = maybe 10 (\x -> if x <= 10 then x else 10) mlimit
      person <-
        Person.findByRoleAndMobileNumberWithoutCC SP.USER number
          >>= fromMaybeM PersonDoesNotExist
      searchcases <-
        Case.findAllByTypeAndStatuses (person.id) C.RIDESEARCH [C.NEW, C.INPROGRESS, C.CONFIRMED, C.COMPLETED, C.CLOSED] (Just limit) moffset
      return $ T.OrderInfo person searchcases
    getByCaseId caseId = do
      (_case :: C.Case) <-
        Case.findByIdAndType (Id caseId) C.RIDESEARCH
          >>= fromMaybeM CaseDoesNotExist
      let requestorId = fromMaybe "_ID" (_case.requestor)
      person <-
        Person.findById (Id requestorId)
          >>= fromMaybeM PersonDoesNotExist
      return $ T.OrderInfo person [_case]

makeCaseToOrder :: (DBFlow m r, EncFlow m r) => SP.Person -> C.Case -> m T.OrderResp
makeCaseToOrder SP.Person {fullName, mobileNumber} C.Case {..} = do
  (confiremedOrder :: Maybe C.Case) <-
    Case.findOneByParentIdAndCaseType id C.RIDEORDER
  let (status_ :: Maybe CaseStatus) = ((\x -> Just $ x.status) =<< confiremedOrder) <|> Just status
  fromLocation <- Location.findLocationById fromLocationId
  toLocation <- Location.findLocationById toLocationId
  trip <- makeTripDetails confiremedOrder
  decMobNum <- decrypt mobileNumber
  --  Info: udf1 is vechicle variant
  let details =
        T.OrderDetails
          { id = getId id,
            status = status_,
            createdAt = createdAt,
            updatedAt = updatedAt,
            startTime = startTime,
            endTime = endTime,
            fromLocation = fromLocation,
            toLocation = toLocation,
            travellerName = fullName,
            travellerPhone = decMobNum,
            vehicleVariant = udf1, -- Note: UDF1 Contain vehicleVariant info
            trip = trip
          }
  pure $ T.OrderResp {_order = details}

makeTripDetails :: DBFlow m r => Maybe C.Case -> m (Maybe T.TripDetails)
makeTripDetails caseM = case caseM of
  Nothing -> pure Nothing
  Just _case -> do
    productInstance <- findOneByCaseId (_case.id) >>= fromMaybeM PINotFound
    let (mProductInfo :: Maybe ProductInfo) = productInstance.info >>= decodeFromText
        provider = mProductInfo >>= (.provider)
        mTracker = mProductInfo >>= (.tracker)
        mTrip = mTracker <&> (.trip)
        driver = mTrip >>= (.driver)
        vehicle = mTrip >>= (.vehicle)
    pure $
      Just $
        T.TripDetails
          { id = getId productInstance.id,
            status = productInstance.status,
            driver = driver,
            price = productInstance.price,
            actualPrice = productInstance.actualPrice,
            provider = provider,
            vehicle = vehicle
          }
