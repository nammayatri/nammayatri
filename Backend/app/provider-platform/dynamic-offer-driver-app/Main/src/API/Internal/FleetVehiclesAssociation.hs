{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.FleetVehiclesAssociation where

import qualified Domain.Action.Internal.FleetVehiclesAssociation as Domain
import Environment
import Kernel.Prelude
import Kernel.Types.App (MandatoryQueryParam)
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

type API = GetFleetVehicleAssociationList :<|> GetFleetVehicleAssociationListV2

type GetFleetVehicleAssociationList =
  ( "fleet" :> "VehicleAssociation" :> "list"
      :> Header "token" Text
      :> MandatoryQueryParam "ticketPlaceId" Text
      :> QueryParam "limit" Int
      :> QueryParam "offset" Int
      :> QueryParam "searchString" Text
      :> Get '[JSON] Domain.BoatFleetVehicleListRes
  )

type GetFleetVehicleAssociationListV2 =
  ( "fleet" :> "VehicleAssociation" :> "list" :> "v2"
      :> Header "token" Text
      :> MandatoryQueryParam "ticketPlaceId" Text
      :> QueryParam "limit" Int
      :> QueryParam "offset" Int
      :> QueryParam "searchString" Text
      :> Get '[JSON] Domain.BoatFleetVehicleListRes
  )

handler :: FlowServer API
handler = getFleetVehicleAssociation :<|> getFleetVehicleAssociationV2

getFleetVehicleAssociation :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> FlowHandler Domain.BoatFleetVehicleListRes
getFleetVehicleAssociation apiKey placeId mbLimit mbOffset mbSearchString = withFlowHandlerAPI $ do
  Domain.getFleetVehicleAssociation apiKey placeId mbLimit mbOffset mbSearchString

getFleetVehicleAssociationV2 :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> FlowHandler Domain.BoatFleetVehicleListRes
getFleetVehicleAssociationV2 apiKey placeId mbLimit mbOffset mbSearchString = withFlowHandlerAPI $ do
  Domain.getFleetVehicleAssociationV2 apiKey placeId mbLimit mbOffset mbSearchString
