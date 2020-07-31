module Fixtures where

import qualified "app-backend" App as AppBE
import qualified "beckn-gateway" App as GatewayBE
import qualified "beckn-transport" App as TransporterBE
import qualified "mock-app-backend" App as MockAppBE
import qualified "mock-provider-backend" App as MockProviderBE
import "app-backend" App.Routes as AbeRoutes
import "beckn-transport" App.Routes as TbeRoutes
import "mock-app-backend" App.Routes as MockAppRoutes
import Beckn.External.FCM.Types
import qualified Beckn.Types.API.Cancel as Cancel
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App
import Beckn.Types.Common as Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Location
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import qualified Beckn.Types.Mobility.Stop as Stop
import Beckn.Types.Mobility.Vehicle
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Data.Time
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified "mock-app-backend" Product.Trigger as MockAppTrigger
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

address :: Address
address =
  Address
    { door = "#817",
      building = "Juspay Apartments",
      street = "27th Main",
      area = "8th Block Koramangala",
      city = "Bangalore",
      country = "India",
      area_code = "560047"
    }

location :: Location
location =
  Location
    { _type = "address",
      _gps = Nothing,
      _address = Just address,
      _station_code = Nothing,
      _area_code = Nothing,
      _city = Nothing,
      _country = Nothing,
      _circle = Nothing,
      _polygon = Nothing,
      _3dspace = Nothing
    }

intentVehicle :: Vehicle
intentVehicle =
  Vehicle
    { category = Just "CAR",
      capacity = Nothing,
      make = Nothing,
      model = Nothing,
      size = Nothing,
      variant = "SUV",
      color = Nothing,
      energy_type = Nothing,
      registration = Nothing
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

getStop :: LocalTime -> Stop.Stop
getStop stopTime =
  Stop.Stop
    { _id = "05515eacc3b546e083489d9e24aba88d",
      _descriptor = Nothing,
      _location = location,
      _arrival_time = Stop.StopTime stopTime (Just stopTime),
      _departure_time = Stop.StopTime stopTime (Just stopTime),
      _transfers = []
    }

buildIntent :: LocalTime -> Intent
buildIntent localTime =
  Intent
    { _query_string = Nothing,
      _provider_id = Nothing,
      _category_id = Nothing,
      _item_id = Nothing,
      _origin = getStop localTime,
      _destination = getStop localTime,
      _stops = [],
      _vehicle = intentVehicle,
      _payload = intentPayload,
      _transfer = Nothing,
      _fare = price,
      _tags = []
    }

buildContext :: Text -> Text -> UTCTime -> Context
buildContext act tid utcTime =
  Context
    { _domain = "MOBILITY",
      _action = act,
      _country = Nothing,
      _city = Nothing,
      _core_version = Just "0.8.0",
      _domain_version = Just "0.8.0",
      _bap_id = Nothing,
      _bg_id = Nothing,
      _bpp_id = Nothing,
      _bap_nw_address = Nothing,
      _bg_nw_address = Nothing,
      _bpp_nw_address = Nothing,
      _request_transaction_id = tid,
      _timestamp = utcTime,
      _token = Nothing
    }

searchReq :: Text -> Text -> UTCTime -> LocalTime -> Search.SearchReq
searchReq act tid utcTime localTime =
  Search.SearchReq
    { context = buildContext act tid utcTime,
      message = Search.SearchIntent $ buildIntent localTime
    }

getFutureTime :: IO LocalTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  (zonedTimeToLocalTime . utcToZonedTime utc) . addUTCTime 7200 <$> getCurrentTime

searchServices ::
  Text ->
  Search.SearchReq ->
  ClientM AppBESearch.AckResponse
onSearchServices ::
  Text ->
  Search.OnSearchReq ->
  ClientM Search.OnSearchRes
searchServices :<|> onSearchServices = client (Proxy :: Proxy AbeRoutes.SearchAPI)

buildSearchReq :: Text -> IO Search.SearchReq
buildSearchReq guid = do
  localTme <- getFutureTime
  utcTime <- getCurrentTime
  pure $ searchReq "search" guid utcTime localTme

cancelRide :: Text -> CancelAPI.CancelReq -> ClientM CancelAPI.CancelRes
onCancelRide :: Cancel.OnCancelReq -> ClientM Cancel.OnCancelRes
cancelRide :<|> onCancelRide = client (Proxy :: Proxy AbeRoutes.CancelAPI)

buildAppCancelReq :: Text -> UTCTime -> Text -> CancelAPI.Entity -> CancelAPI.CancelReq
buildAppCancelReq guid utcTime entityId entityType =
  CancelAPI.CancelReq
    { context = buildContext "cancel" guid utcTime,
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

triggerSearchReq :: MockAppTrigger.TriggerFlow -> ClientM Common.AckResponse
triggerSearchReq = client (Proxy :: Proxy MockAppRoutes.TriggerAPI)

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

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient clientEnv x = runClientM x clientEnv

startServers :: IO (ThreadId, ThreadId, ThreadId, ThreadId, ThreadId)
startServers = do
  setEnv "USE_FAKE_SMS" "7891"
  appTid <- forkIO AppBE.runAppBackend
  tbeTid <- forkIO TransporterBE.runTransporterBackendApp
  gatewayTid <- forkIO GatewayBE.runGateway
  mockAppTid <- forkIO MockAppBE.runMockApp
  mockProvTid <- forkIO MockProviderBE.runMockProvider
  return (appTid, tbeTid, gatewayTid, mockAppTid, mockProvTid)

getLoggerCfg :: FilePath -> T.LoggerConfig
getLoggerCfg fName =
  T.defaultLoggerConfig
    { T._logToFile = True,
      T._logFilePath = "/tmp/" <> fName,
      T._isAsync = False
    }

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
