module Fixtures where

import "app-backend" App.Routes as AbeRoutes
import "beckn-transport" App.Routes as TbeRoutes
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App
import Beckn.Types.Common as Common
import Beckn.Types.Core.Ack as Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Location
import Beckn.Types.Core.Provider
import Beckn.Types.Core.Scalar
import Beckn.Types.Core.ScalarRange
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Products as Products
import Data.Time
import Data.Time.LocalTime (LocalTime)
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client (ClientM, client)
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase
import qualified Types.API.Confirm as ConfirmAPI

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
    { travellers = TravellerReqInfo {count = 0},
      luggage = Luggage {count = 2, weight_range = Nothing, dimensions = Nothing}
    }

fareRange :: ScalarRange
fareRange = ScalarRange {_min = 10, _max = 1000, _unit = "Rs"}

buildIntent :: LocalTime -> Intent
buildIntent localTime =
  Intent
    { domain = "MOBILITY",
      origin = location,
      destination = location,
      time = localTime,
      stops = [],
      vehicle = intentVehicle,
      providers = [],
      payload = intentPayload,
      transfer_attrs = Nothing,
      fare_range = fareRange,
      tags = []
    }

buildContext :: Text -> Text -> LocalTime -> Context
buildContext act tid localTime =
  Context
    { domain = "MOBILITY",
      action = act,
      version = Nothing,
      transaction_id = tid,
      message_id = Nothing,
      timestamp = localTime,
      dummy = "dummy"
    }

searchReq :: Text -> Text -> LocalTime -> Search.SearchReq
searchReq act tid localTime =
  Search.SearchReq
    { context = buildContext act tid localTime,
      message = buildIntent localTime
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

buildCaseListRes :: ClientM AppCase.CaseListRes
buildCaseListRes = do
  let CaseAPIClient {..} = getCase
      CaseClient {..} = mkCaseClient appRegistrationToken
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
