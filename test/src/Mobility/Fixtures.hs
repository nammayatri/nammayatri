module Mobility.Fixtures where

import "app-backend" App.Routes as AbeRoutes
import "beckn-transport" App.Routes as TbeRoutes
import Beckn.External.FCM.Types
import Beckn.Types.App
import Beckn.Types.Core.Ack
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Payload
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Data.Time
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client
import qualified Types.API.Cancel as CancelAPI
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase
import qualified Types.API.Confirm as ConfirmAPI
import qualified "app-backend" Types.API.Feedback as AppFeedback
import qualified "beckn-transport" Types.API.Person as TbePerson
import qualified "app-backend" Types.API.ProductInstance as AppPI
import qualified "beckn-transport" Types.API.ProductInstance as TbePI
import qualified "app-backend" Types.API.Registration as Reg
import qualified "app-backend" Types.API.Search as AppBESearch
import qualified "app-backend" Types.API.Serviceability as AppServ
import qualified "app-backend" Types.Common as AppCommon

address :: AppCommon.Address
address =
  AppCommon.Address
    { door = "#817",
      building = "Juspay Apartments",
      street = "27th Main",
      area = "8th Block Koramangala",
      city = "Bangalore",
      country = "India",
      areaCode = "560047",
      state = "Karnataka"
    }

location :: AppCommon.GPS -> AppCommon.Location
location gps =
  AppCommon.Location
    { gps = Just gps,
      address = Just address,
      city = Nothing
    }

vehicle :: AppCommon.Vehicle
vehicle =
  AppCommon.Vehicle
    { category = Just AppCommon.CAR,
      capacity = Nothing,
      model = Nothing,
      variant = "SUV",
      registrationNumber = Nothing
    }

intentPayload :: Payload
intentPayload =
  Payload
    { _travellers = [],
      _traveller_count = Just 2,
      _luggage = Nothing,
      _travel_group = Nothing
    }

price :: Price
price =
  let amt = DecimalValue "800" Nothing
   in Price
        { _currency = "INR",
          _value = Just amt,
          _estimated_value = Just amt,
          _computed_value = Just amt,
          _listed_value = Just amt,
          _offered_value = Just amt,
          _minimum_value = Just amt,
          _maximum_value = Just amt
        }

getStop :: UTCTime -> AppCommon.GPS -> AppCommon.Stop
getStop stopTime gps =
  AppCommon.Stop
    { location = location gps,
      arrivalTime = AppCommon.StopTime stopTime (Just stopTime),
      departureTime = AppCommon.StopTime stopTime (Just stopTime)
    }

searchReq :: Text -> UTCTime -> UTCTime -> AppBESearch.SearchReq
searchReq tid utcTime futureTime =
  AppBESearch.SearchReq
    { transaction_id = tid,
      startTime = utcTime,
      origin = getStop futureTime $ AppCommon.GPS "10.0739" "76.2733",
      destination = getStop futureTime $ AppCommon.GPS "10.5449" "76.4356",
      vehicle = vehicle,
      travellers = [],
      fare = AppCommon.DecimalValue "360" $ Just "50"
    }

bppTransporterOrgId :: Text
bppTransporterOrgId = "70c76e36-f035-46fd-98a7-572dc8934323"

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

searchServices ::
  Text ->
  AppBESearch.SearchReq ->
  ClientM AppBESearch.AckResponse
searchServices :<|> _ = client (Proxy :: Proxy AbeRoutes.SearchAPI)

buildSearchReq :: Text -> IO AppBESearch.SearchReq
buildSearchReq guid = do
  futureTime <- getFutureTime
  utcTime <- getCurrentTime
  pure $ searchReq guid utcTime futureTime

cancelRide :: Text -> CancelAPI.CancelReq -> ClientM CancelAPI.CancelRes
cancelRide :<|> _ = client (Proxy :: Proxy AbeRoutes.CancelAPI)

buildAppCancelReq :: Text -> Text -> CancelAPI.Entity -> CancelAPI.CancelReq
buildAppCancelReq guid entityId entityType =
  CancelAPI.CancelReq
    { transaction_id = guid,
      message = CancelAPI.Cancel entityId entityType
    }

-- For the idea behind generating a client, when nested routes are involved,
-- see https://github.com/haskell-servant/servant/issues/335
newtype CaseAPIClient = CaseAPIClient {mkCaseClient :: Text -> CaseClient}

data CaseClient = CaseClient
  { getCaseListRes ::
      Case.CaseType ->
      [Case.CaseStatus] ->
      Maybe Integer ->
      Maybe Integer ->
      ClientM AppCase.CaseListRes,
    getCaseStatusRes :: CaseId -> ClientM AppCase.StatusRes
  }

getCase :: CaseAPIClient
getCase = CaseAPIClient {..}
  where
    caseAPIClient = client (Proxy :: Proxy AbeRoutes.CaseAPI)
    mkCaseClient regToken = CaseClient {..}
      where
        getCaseListRes :<|> getCaseStatusRes = caseAPIClient regToken

buildCaseListRes :: Text -> ClientM AppCase.CaseListRes
buildCaseListRes regToken = do
  let CaseAPIClient {..} = getCase
      CaseClient {..} = mkCaseClient regToken
  getCaseListRes Case.RIDESEARCH [Case.NEW] (Just 10) (Just 0)

buildCaseId :: Text -> CaseId
buildCaseId = CaseId

buildCaseStatusRes :: Text -> ClientM AppCase.StatusRes
buildCaseStatusRes caseId = do
  let CaseAPIClient {..} = getCase
      CaseClient {..} = mkCaseClient appRegistrationToken
      appCaseId = buildCaseId caseId
  getCaseStatusRes appCaseId

appConfirmRide :: Text -> ConfirmAPI.ConfirmReq -> ClientM AckResponse
appConfirmRide :<|> _ = client (Proxy :: Proxy AbeRoutes.ConfirmAPI)

appFeedback :: Text -> AppFeedback.FeedbackReq -> ClientM AckResponse
appFeedback = client (Proxy :: Proxy AbeRoutes.FeedbackAPI)

callAppFeedback :: Int -> ProductInstanceId -> CaseId -> ClientM AckResponse
callAppFeedback ratingValue productInstanceId caseId =
  let request =
        AppFeedback.FeedbackReq
          { caseId = _getCaseId caseId,
            productInstanceId = _getProductInstanceId productInstanceId,
            rating = ratingValue
          }
   in appFeedback appRegistrationToken request

buildAppConfirmReq :: Text -> Text -> ConfirmAPI.ConfirmReq
buildAppConfirmReq cid pid =
  ConfirmAPI.ConfirmReq
    { caseId = cid,
      productInstanceId = pid
    }

listLeads :: Text -> [Case.CaseStatus] -> Case.CaseType -> Maybe Int -> Maybe Int -> ClientM [TbeCase.CaseRes]
acceptOrDeclineRide :: Text -> Text -> TbeCase.UpdateCaseReq -> ClientM Case.Case
listLeads :<|> acceptOrDeclineRide = client (Proxy :: Proxy TbeRoutes.CaseAPI)

buildListLeads :: ClientM [TbeCase.CaseRes]
buildListLeads = listLeads appRegistrationToken [Case.NEW] Case.RIDESEARCH (Just 50) Nothing

listOrgRides :: Text -> [PI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> ClientM TbePI.ProductInstanceList
listDriverRides :: Text -> Text -> ClientM TbePI.RideListRes
listVehicleRides :: Text -> Text -> ClientM TbePI.RideListRes
listCasesByProductInstance :: Text -> Text -> Maybe Case.CaseType -> ClientM [TbeCase.CaseRes]
rideUpdate :: Text -> ProductInstanceId -> TbePI.ProdInstUpdateReq -> ClientM TbePI.ProdInstInfo
listOrgRides :<|> listDriverRides :<|> listVehicleRides :<|> listCasesByProductInstance :<|> rideUpdate = client (Proxy :: Proxy TbeRoutes.ProductInstanceAPI)

listPIs :: Text -> [PI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> ClientM AppPI.ProductInstanceList
listPIs = client (Proxy :: Proxy AbeRoutes.ProductInstanceAPI)

buildListPIs :: PI.ProductInstanceStatus -> ClientM AppPI.ProductInstanceList
buildListPIs status = listPIs appRegistrationToken [status] [Case.RIDEORDER] (Just 50) Nothing

createPerson :: Text -> TbePerson.CreatePersonReq -> ClientM TbePerson.UpdatePersonRes
listPerson :: Text -> [Person.Role] -> Maybe Integer -> Maybe Integer -> ClientM TbePerson.ListPersonRes
updatePerson :: Text -> Text -> TbePerson.UpdatePersonReq -> ClientM TbePerson.UpdatePersonRes
getPerson :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Person.IdentifierType -> ClientM TbePerson.PersonEntityRes
deletePerson :: Text -> Text -> ClientM TbePerson.DeletePersonRes
linkEntity :: Text -> Text -> TbePerson.LinkReq -> ClientM TbePerson.PersonEntityRes
createPerson
  :<|> listPerson
  :<|> updatePerson
  :<|> getPerson
  :<|> deletePerson
  :<|> linkEntity = client (Proxy :: Proxy TbeRoutes.PersonAPI)

callGetPerson :: PersonId -> ClientM TbePerson.PersonEntityRes
callGetPerson personId =
  getPerson
    appRegistrationToken
    (Just $ _getPersonId personId)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

buildUpdateCaseReq :: TbeCase.UpdateCaseReq
buildUpdateCaseReq =
  TbeCase.UpdateCaseReq
    { _quote = Just 150.50,
      _transporterChoice = "ACCEPTED"
    }

buildUpdatePIReq :: TbePI.ProdInstUpdateReq
buildUpdatePIReq =
  TbePI.ProdInstUpdateReq
    { _status = Just PI.TRIP_ASSIGNED,
      _personId = Just testDriverId,
      _vehicleId = Just testVehicleId,
      _otpCode = Nothing
    }

buildUpdateStatusReq :: PI.ProductInstanceStatus -> Maybe Text -> TbePI.ProdInstUpdateReq
buildUpdateStatusReq status otp =
  TbePI.ProdInstUpdateReq
    { _status = Just status,
      _personId = Nothing,
      _vehicleId = Nothing,
      _otpCode = otp
    }

originServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
originServiceability regToken = origin
  where
    origin :<|> _ :<|> _ = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

destinationServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
destinationServiceability regToken = destination
  where
    _ :<|> destination :<|> _ = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

rideServiceability :: RegToken -> AppServ.RideServiceabilityReq -> ClientM AppServ.RideServiceabilityRes
rideServiceability regToken = ride
  where
    _ :<|> _ :<|> ride = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

buildOrgRideReq :: PI.ProductInstanceStatus -> Case.CaseType -> ClientM TbePI.ProductInstanceList
buildOrgRideReq status csType = listOrgRides appRegistrationToken [status] [csType] (Just 50) Nothing

appRegistrationToken :: Text
appRegistrationToken = "ea37f941-427a-4085-a7d0-96240f166672"

testVehicleId :: Text
testVehicleId = "0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94"

testDriverId :: Text
testDriverId = "6bc4bc84-2c43-425d-8853-22f47bd06691"

appInitiateLogin :: Reg.InitiateLoginReq -> ClientM Reg.InitiateLoginRes
appVerifyLogin :: Text -> Reg.LoginReq -> ClientM Reg.LoginRes
appReInitiateLogin :: Text -> Reg.ReInitiateLoginReq -> ClientM Reg.InitiateLoginRes
appInitiateLogin :<|> appVerifyLogin :<|> appReInitiateLogin = client (Proxy :: Proxy AbeRoutes.RegistrationAPI)

buildInitiateLoginReq :: Reg.InitiateLoginReq
buildInitiateLoginReq =
  Reg.InitiateLoginReq
    { _medium = SR.SMS,
      __type = SR.OTP,
      _mobileNumber = "90000900000",
      _mobileCountryCode = "+91",
      _role = Just Person.USER,
      _deviceToken = Nothing
    }

buildLoginReq :: Reg.LoginReq
buildLoginReq =
  Reg.LoginReq
    { _medium = SR.SMS,
      __type = SR.OTP,
      _hash = "7891",
      _mobileNumber = "90000900000",
      _mobileCountryCode = "+91",
      _deviceToken = Just $ FCMRecipientToken "AN_DEV_TOKEN"
    }

initiateLoginReq :: ClientM Reg.InitiateLoginRes
initiateLoginReq = appInitiateLogin buildInitiateLoginReq

verifyLoginReq :: Text -> ClientM Reg.LoginRes
verifyLoginReq tokenId = appVerifyLogin tokenId buildLoginReq

getAppBaseUrl :: BaseUrl
getAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8013,
      baseUrlPath = "/v1"
    }

getTransporterBaseUrl :: BaseUrl
getTransporterBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8014,
      baseUrlPath = "/v1"
    }
