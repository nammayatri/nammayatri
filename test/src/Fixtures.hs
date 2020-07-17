module Fixtures where

import qualified "app-backend" App as AppBE
import qualified "beckn-transport" App as TransporterBE
import "app-backend" App.Routes as AbeRoutes
import "beckn-transport" App.Routes as TbeRoutes
import Beckn.External.FCM.Types
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App
import Beckn.Types.Common as Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Location
import Beckn.Types.Core.ScalarRange
import Beckn.Types.Mobility.Intent
import qualified Beckn.Types.Mobility.Stop as Stop
import Beckn.Types.Mobility.Vehicle
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Data.Time
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client (ClientEnv, ClientError, ClientM, client, runClientM)
import System.Environment (setEnv)
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase
import qualified Types.API.Confirm as ConfirmAPI
import qualified "app-backend" Types.API.Registration as Reg

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
    { _travellers = TravellerReqInfo {_count = 0},
      _luggage = Luggage {_count = 2, _weight_range = Nothing, _dimensions = Nothing}
    }

fareRange :: ScalarRange
fareRange = ScalarRange {_min = 10, _max = 1000}

getStop :: LocalTime -> Stop.Stop
getStop stopTime =
  Stop.Stop
    { _descriptor = Nothing,
      _location = location,
      _arrival_time = Stop.StopTime stopTime (Just stopTime),
      _departure_time = Stop.StopTime stopTime (Just stopTime)
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
      _transfer_attrs = Nothing,
      _fare_range = fareRange,
      _tags = []
    }

buildContext :: Text -> Text -> LocalTime -> Context
buildContext act tid localTime =
  Context
    { _domain = "MOBILITY",
      _action = act,
      _version = Just "0.8.0",
      _transaction_id = tid,
      _session_id = Nothing,
      _timestamp = localTime,
      _token = Nothing,
      _status = Nothing
    }

searchReq :: Text -> Text -> LocalTime -> Search.SearchReq
searchReq act tid localTime =
  Search.SearchReq
    { context = buildContext act tid localTime,
      message = Search.SearchIntent $ buildIntent localTime
    }

getFutureTime :: IO LocalTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  (zonedTimeToLocalTime . utcToZonedTime utc) . addUTCTime 7200 <$> getCurrentTime

searchServices ::
  Text ->
  Search.SearchReq ->
  ClientM Common.AckResponse
onSearchServices ::
  Text ->
  Search.OnSearchReq ->
  ClientM Search.OnSearchRes
searchServices :<|> onSearchServices = client (Proxy :: Proxy AbeRoutes.SearchAPI)

buildSearchReq :: Text -> IO Search.SearchReq
buildSearchReq guid = searchReq "search" guid <$> getFutureTime

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

buildUpdateCaseReq :: TbeCase.UpdateCaseReq
buildUpdateCaseReq =
  TbeCase.UpdateCaseReq
    { _quote = Just 150.50,
      _transporterChoice = "ACCEPTED"
    }

appRegistrationToken :: Text
appRegistrationToken = "ea37f941-427a-4085-a7d0-96240f166672"

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

startServers :: IO (ThreadId, ThreadId)
startServers = do
  setEnv "USE_FAKE_SMS" "7891"
  appTid <- forkIO AppBE.runAppBackend
  tbeTid <- forkIO TransporterBE.runTransporterBackendApp
  return (appTid, tbeTid)
