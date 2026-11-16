module Lib.BmsBackend.Transit.Fleet.API where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.BmsBackend.Transit.Common
import Lib.BmsBackend.Transit.Fleet.Types
import Servant

type TripActionV1API = Transit ("fleet-operator" :> "v1" :> "trip-action" :> ReqBody '[JSON] TripActionV1Req :> Scoped (Post '[JSON] TripActionV1Resp))

tripActionV1API :: Proxy TripActionV1API
tripActionV1API = Proxy

tripActionV1 :: Text -> TripActionV1Req -> Maybe Text -> Maybe Text -> ET.EulerClient TripActionV1Resp
tripActionV1 = ET.client tripActionV1API

type CurrentOperationV1API = Transit ("fleet-operator" :> "v1" :> "current-operation" :> ReqBody '[JSON] FleetAnchorReq :> Scoped (Post '[JSON] CurrentOperationV1))

currentOperationV1API :: Proxy CurrentOperationV1API
currentOperationV1API = Proxy

currentOperationV1 :: Text -> FleetAnchorReq -> Maybe Text -> Maybe Text -> ET.EulerClient CurrentOperationV1
currentOperationV1 = ET.client currentOperationV1API

type CurrentTripDetailsV1API = Transit ("fleet-operator" :> "v1" :> "current-trip-details" :> ReqBody '[JSON] CurrentTripDetailsV1Req :> Scoped (Post '[JSON] CurrentTripDetailsV1))

currentTripDetailsV1API :: Proxy CurrentTripDetailsV1API
currentTripDetailsV1API = Proxy

currentTripDetailsV1 :: Text -> CurrentTripDetailsV1Req -> Maybe Text -> Maybe Text -> ET.EulerClient CurrentTripDetailsV1
currentTripDetailsV1 = ET.client currentTripDetailsV1API

type ActiveTripV1API = Transit ("fleet-operator" :> "v1" :> "active-trip" :> ReqBody '[JSON] FleetAnchorReq :> Scoped (Post '[JSON] ActiveTripV1))

activeTripV1API :: Proxy ActiveTripV1API
activeTripV1API = Proxy

activeTripV1 :: Text -> FleetAnchorReq -> Maybe Text -> Maybe Text -> ET.EulerClient ActiveTripV1
activeTripV1 = ET.client activeTripV1API

type TripActionV2API = Transit ("fleet-operator" :> "v2" :> "trip-action" :> ReqBody '[JSON] TripActionV2Req :> Scoped (Post '[JSON] CurrentOperationV2))

tripActionV2API :: Proxy TripActionV2API
tripActionV2API = Proxy

tripActionV2 :: Text -> TripActionV2Req -> Maybe Text -> Maybe Text -> ET.EulerClient CurrentOperationV2
tripActionV2 = ET.client tripActionV2API

type CurrentOperationV2API = Transit ("fleet-operator" :> "v2" :> "current-operation" :> ReqBody '[JSON] AnchorReq :> Scoped (Post '[JSON] CurrentOperationV2))

currentOperationV2API :: Proxy CurrentOperationV2API
currentOperationV2API = Proxy

currentOperationV2 :: Text -> AnchorReq -> Maybe Text -> Maybe Text -> ET.EulerClient CurrentOperationV2
currentOperationV2 = ET.client currentOperationV2API

type ActiveTripV2API = Transit ("fleet-operator" :> "v2" :> "active-trip" :> ReqBody '[JSON] AnchorReq :> Scoped (Post '[JSON] ActiveTripV2))

activeTripV2API :: Proxy ActiveTripV2API
activeTripV2API = Proxy

activeTripV2 :: Text -> AnchorReq -> Maybe Text -> Maybe Text -> ET.EulerClient ActiveTripV2
activeTripV2 = ET.client activeTripV2API
