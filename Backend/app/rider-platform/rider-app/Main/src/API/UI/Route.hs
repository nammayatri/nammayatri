{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Route
  ( API,
    handler,
    DRoute.GetRoutesReq,
    DRoute.GetRoutesResp,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.UI.Route as DRoute
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRideStatus
import Environment
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.EtaRoute as CQEtaRoute
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Maps.GetRoutesReq
    :> Post '[JSON] Maps.GetRoutesResp
    :<|> "pickup"
      :> "route"
      :> TokenAuth
      :> ReqBody '[JSON] DRoute.GetPickupRoutesReq
      :> Post '[JSON] Maps.GetRoutesResp
    :<|> "trip"
      :> "route"
      :> TokenAuth
      :> ReqBody '[JSON] Maps.GetRoutesReq
      :> Post '[JSON] Maps.GetRoutesResp
    :<|> "instruction"
      :> "route"
      :> TokenAuth
      :> ReqBody '[JSON] Maps.GetRoutesReq
      :> Post '[JSON] Maps.GetRoutesResp

handler :: FlowServer API
handler = getRoute :<|> getPickupRoute :<|> getTripRoute :<|> getInstructionRoute

getRoute :: (Id Person.Person, Id Merchant.Merchant) -> Maps.GetRoutesReq -> FlowHandler Maps.GetRoutesResp
getRoute (personId, merchantId) = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId . DRoute.getRoutes (personId, merchantId) (Just personId.getId)

getPickupRoute :: (Id Person.Person, Id Merchant.Merchant) -> DRoute.GetPickupRoutesReq -> FlowHandler Maps.GetRoutesResp
getPickupRoute (personId, merchantId) req = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ do
  routeResp <- DRoute.getPickupRoutes (personId, merchantId) (Just personId.getId) req
  whenJust req.rideId $ \rid -> do
    case routeResp of
      (firstRoute : _) -> do
        mbRide <- B.runInReplica $ QRide.findById rid
        whenJust (mbRide >>= etaLegForStatus . DRide.status) $ \leg -> do
          unless (null firstRoute.points) $ do
            CQEtaRoute.cacheRoute leg rid firstRoute.points
          whenJust firstRoute.duration $ \duration -> do
            -- remove for non disha cities later
            now <- getCurrentTime
            CQEtaRoute.cacheEtaBasis leg rid duration now (Just $ NE.last req.waypoints)
      [] -> pure ()
  return routeResp
  where
    etaLegForStatus = \case
      DRideStatus.NEW -> Just CQEtaRoute.PickupLeg
      DRideStatus.INPROGRESS -> Just CQEtaRoute.DropLeg
      _ -> Nothing

getTripRoute :: (Id Person.Person, Id Merchant.Merchant) -> Maps.GetRoutesReq -> FlowHandler Maps.GetRoutesResp
getTripRoute (personId, merchantId) = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId . DRoute.getTripRoutes (personId, merchantId) (Just personId.getId)

getInstructionRoute :: (Id Person.Person, Id Merchant.Merchant) -> Maps.GetRoutesReq -> FlowHandler Maps.GetRoutesResp
getInstructionRoute (personId, merchantId) = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId . DRoute.getInstructionRoute (personId, merchantId) (Just personId.getId)
