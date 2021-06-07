{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import Storage.Queries.Person as Person
import Storage.Queries.ProductInstance as PI
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.CustomerSupport as T
import Types.Error
import Types.ProductInfo as ProductInfo
import Utils.Common

login :: T.LoginReq -> FlowHandler T.LoginRes
login T.LoginReq {..} = withFlowHandlerAPI $ do
  personM <- Person.findByUsernameAndPassword email password
  case personM of
    Nothing -> throwError Unauthorized
    Just person ->
      if person.status /= SP.ACTIVE && person.role /= SP.CUSTOMER_SUPPORT
        then throwError Unauthorized
        else do
          token <- generateToken person
          pure $ T.LoginRes token "Logged in successfully"

generateToken :: SP.Person -> Flow Text
generateToken SP.Person {..} = do
  let personId = getId id
  regToken <- createSupportRegToken personId
  -- Clean Old Login Session
  DB.runSqlDBTransaction $ do
    RegistrationToken.deleteByPersonId personId
    RegistrationToken.create regToken
  pure $ regToken.token

logout :: SP.Person -> FlowHandler T.LogoutRes
logout person =
  withFlowHandlerAPI $
    if person.role /= SP.CUSTOMER_SUPPORT
      then throwError Unauthorized -- Do we need this Check?
      else do
        DB.runSqlDB (RegistrationToken.deleteByPersonId (getId $ person.id))
        pure $ T.LogoutRes "Logged out successfully"

createSupportRegToken :: Text -> Flow SR.RegistrationToken
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

listOrder :: SP.Person -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler [T.OrderResp]
listOrder supportP mCaseId mMobile mlimit moffset =
  withFlowHandlerAPI $
    if supportP.role /= SP.ADMIN && supportP.role /= SP.CUSTOMER_SUPPORT
      then throwError AccessDenied
      else do
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
      let personId = fromMaybe "_ID" (_case.requestor)
      person <-
        Person.findById (Id personId)
          >>= fromMaybeM PersonDoesNotExist
      return $ T.OrderInfo person [_case]

makeCaseToOrder :: SP.Person -> C.Case -> Flow T.OrderResp
makeCaseToOrder SP.Person {fullName, mobileNumber} C.Case {..} = do
  (confiremedOrder :: Maybe C.Case) <-
    Case.findOneByParentIdAndCaseType id C.RIDEORDER
  let (status_ :: Maybe CaseStatus) = ((\x -> Just $ x.status) =<< confiremedOrder) <|> Just status
  fromLocation <- Location.findLocationById fromLocationId
  toLocation <- Location.findLocationById toLocationId
  trip <- makeTripDetails confiremedOrder
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
            travellerPhone = mobileNumber,
            vehicleVariant = udf1, -- Note: UDF1 Contain vehicleVariant info
            trip = trip
          }
  pure $ T.OrderResp {_order = details}

makeTripDetails :: Maybe C.Case -> Flow (Maybe T.TripDetails)
makeTripDetails caseM = case caseM of
  Nothing -> pure Nothing
  Just _case -> do
    -- Note: In case of Confirmed Order only one Product Instance will be Present
    ProductInstance.ProductInstance {id, status, info, price} <-
      head
        <$> PI.findAllByCaseId (_case.id)
    let (mproductInfo :: Maybe ProductInfo) = decodeFromText =<< info
        provider = (\x -> x.provider) =<< mproductInfo
        mtracker = (\x -> x.tracker) =<< mproductInfo
        mtrip = (\x -> Just $ x.trip) =<< mtracker
        driver = (\x -> x.driver) =<< mtrip
        vehicle = (\x -> x.vehicle) =<< mtrip
    pure $
      Just $
        T.TripDetails
          { id = getId id,
            status = status,
            driver = driver,
            price = price,
            provider = provider,
            vehicle = vehicle
          }
