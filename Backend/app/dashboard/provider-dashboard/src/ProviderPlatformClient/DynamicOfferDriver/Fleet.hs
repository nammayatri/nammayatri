{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ProviderPlatformClient.DynamicOfferDriver.Fleet
  ( callDynamicOfferDriverAppFleetApi,
  )
where

import "dynamic-offer-driver-app" API.Dashboard.Fleet as BPP
import qualified Dashboard.ProviderPlatform.Driver as Driver
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Fleet.Registration as Fleet
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client
import "lib-dashboard" Tools.Metrics

data FleetOperationsAPIs = FleetOperationsAPIs
  { addVehicleForFleet :: Text -> Maybe Text -> Text -> Driver.AddVehicleReq -> Euler.EulerClient APISuccess,
    getAllVehicleForFleet :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.ListVehicleRes,
    getAllDriverForFleet :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.FleetListDriverRes,
    fleetUnlinkVehicle :: Text -> Id Driver.Driver -> Text -> Euler.EulerClient APISuccess,
    fleetRemoveVehicle :: Text -> Text -> Euler.EulerClient APISuccess,
    fleetRemoveDriver :: Text -> Id Driver.Driver -> Euler.EulerClient APISuccess,
    fleetTotalEarning :: Text -> Euler.EulerClient Driver.FleetTotalEarningResponse,
    fleetVehicleEarning :: Text -> Text -> Maybe (Id Driver.Driver) -> Euler.EulerClient Driver.FleetEarningRes,
    fleetDriverEarning :: Text -> Id Driver.Driver -> Euler.EulerClient Driver.FleetEarningRes,
    getFleetDriverVehicleAssociation :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.DrivertoVehicleAssociationRes,
    getFleetDriverAssociation :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.DrivertoVehicleAssociationRes,
    getFleetVehicleAssociation :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.DrivertoVehicleAssociationRes,
    setVehicleDriverRcStatusForFleet :: Id Driver.Driver -> Text -> Driver.RCStatusReq -> Euler.EulerClient APISuccess
  }

data FleetRegistrationAPIs = FleetRegistrationAPIs
  { fleetOwnerLogin :: Fleet.FleetOwnerLoginReq -> Euler.EulerClient APISuccess,
    fleetOwnerVerify :: Fleet.FleetOwnerLoginReq -> Euler.EulerClient APISuccess
  }

data FleetAPIs = FleetAPIs
  { operations :: FleetOperationsAPIs,
    registration :: FleetRegistrationAPIs
  }

mkDynamicOfferDriverAppFleetAPIs :: CheckedShortId DM.Merchant -> City.City -> Text -> FleetAPIs
mkDynamicOfferDriverAppFleetAPIs merchantId city token = do
  let operations = FleetOperationsAPIs {..}
      registration = FleetRegistrationAPIs {..}
  FleetAPIs {..}
  where
    fleetOperationsClient
      :<|> fleetRegisterationClient = clientWithMerchantAndCity (Proxy :: Proxy BPP.API) merchantId city token

    addVehicleForFleet
      :<|> getAllVehicleForFleet
      :<|> getAllDriverForFleet
      :<|> fleetUnlinkVehicle
      :<|> fleetRemoveVehicle
      :<|> fleetRemoveDriver
      :<|> fleetTotalEarning
      :<|> fleetVehicleEarning
      :<|> fleetDriverEarning
      :<|> getFleetDriverVehicleAssociation
      :<|> getFleetDriverAssociation
      :<|> getFleetVehicleAssociation
      :<|> setVehicleDriverRcStatusForFleet = fleetOperationsClient

    fleetOwnerLogin
      :<|> fleetOwnerVerify = fleetRegisterationClient

callDynamicOfferDriverAppFleetApi ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI FleetAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  City.City ->
  (FleetAPIs -> b) ->
  c
callDynamicOfferDriverAppFleetApi merchantId city = callServerAPI @_ @m @r DRIVER_OFFER_BPP_MANAGEMENT (mkDynamicOfferDriverAppFleetAPIs merchantId city) "callDynamicOfferDriverAppFleetApi"
