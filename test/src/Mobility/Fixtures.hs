module Mobility.Fixtures where

import qualified "app-backend" App as AppBE
import qualified "beckn-gateway" App as GatewayBE
import qualified "beckn-transport" App as TransporterBE
import "app-backend" App.Routes as AbeRoutes
import "beckn-transport" App.Routes as TbeRoutes
import Beckn.External.FCM.Types
import qualified Beckn.Types.API.Cancel as Cancel
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App
import Beckn.Types.Common as Common
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
import System.Environment (setEnv)
import qualified Types.API.Cancel as CancelAPI
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase
import qualified Types.API.Confirm as ConfirmAPI
import qualified "app-backend" Types.API.ProductInstance as AppPI
import qualified "beckn-transport" Types.API.ProductInstance as TbePI
import qualified "app-backend" Types.API.Registration as Reg
import qualified "app-backend" Types.API.Search as AppBESearch
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
      areaCode = "560047"
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
      origin = getStop futureTime $ AppCommon.GPS "12.9401108" "77.6206631",
      destination = getStop futureTime $ AppCommon.GPS "12.9401108" "77.6306631",
      vehicle = vehicle,
      travellers = [],
      fare = AppCommon.DecimalValue "360" $ Just "50"
    }

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

searchServices ::
  Text ->
  AppBESearch.SearchReq ->
  ClientM AppBESearch.AckResponse
onSearchServices ::
  Text ->
  Search.OnSearchReq ->
  ClientM Search.OnSearchRes
searchServices :<|> onSearchServices = client (Proxy :: Proxy AbeRoutes.SearchAPI)

buildSearchReq :: Text -> IO AppBESearch.SearchReq
buildSearchReq guid = do
  futureTime <- getFutureTime
  utcTime <- getCurrentTime
  pure $ searchReq guid utcTime futureTime

cancelRide :: Text -> CancelAPI.CancelReq -> ClientM CancelAPI.CancelRes
onCancelRide :: Cancel.OnCancelReq -> ClientM Cancel.OnCancelRes
cancelRide :<|> onCancelRide = client (Proxy :: Proxy AbeRoutes.CancelAPI)

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
appOnConfirmRide :: Confirm.OnConfirmReq -> ClientM Confirm.OnConfirmRes
appConfirmRide :<|> appOnConfirmRide = client (Proxy :: Proxy AbeRoutes.ConfirmAPI)

buildAppConfirmReq :: Text -> Text -> ConfirmAPI.ConfirmReq
buildAppConfirmReq cid pid =
  ConfirmAPI.ConfirmReq
    { caseId = cid,
      productInstanceId = pid
    }

listLeads :: Text -> [Case.CaseStatus] -> Case.CaseType -> Maybe Int -> Maybe Int -> Maybe Bool -> ClientM [TbeCase.CaseRes]
acceptOrDeclineRide :: Text -> Text -> TbeCase.UpdateCaseReq -> ClientM Case.Case
listLeads :<|> acceptOrDeclineRide = client (Proxy :: Proxy TbeRoutes.CaseAPI)

buildListLeads :: ClientM [TbeCase.CaseRes]
buildListLeads = listLeads appRegistrationToken [Case.NEW] Case.RIDESEARCH (Just 50) Nothing (Just True)

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

buildUpdateCaseReq :: TbeCase.UpdateCaseReq
buildUpdateCaseReq =
  TbeCase.UpdateCaseReq
    { _quote = Just 150.50,
      _transporterChoice = "ACCEPTED"
    }

buildUpdatePIReq :: TbePI.ProdInstUpdateReq
buildUpdatePIReq =
  TbePI.ProdInstUpdateReq
    { _status = Nothing,
      _personId = Just testDriverId,
      _vehicleId = Just testVehicleId
    }

buildUpdateStatusReq :: PI.ProductInstanceStatus -> TbePI.ProdInstUpdateReq
buildUpdateStatusReq status =
  TbePI.ProdInstUpdateReq
    { _status = Just status,
      _personId = Nothing,
      _vehicleId = Nothing
    }

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

startServers :: IO (ThreadId, ThreadId, ThreadId)
startServers = do
  setEnv "USE_FAKE_SMS" "7891"
  setEnv "GATEWAY_SELECTOR" "JUSPAY"
  appTid <- forkIO AppBE.runAppBackend
  tbeTid <- forkIO TransporterBE.runTransporterBackendApp
  gatewayTid <- forkIO GatewayBE.runGateway
  return (appTid, tbeTid, gatewayTid)

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
