{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.AppManagement.Endpoints.VehicleSeatLayoutMapping where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Data.Text
import qualified Kernel.Types.Id
import qualified "this" Domain.Types.VehicleSeatLayoutMapping
import qualified "this" Domain.Types.SeatLayout
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Kernel.Types.HideSecrets



data VehicleSeatLayoutMappingItem
    = VehicleSeatLayoutMappingItem {gtfsId :: Data.Text.Text,
                                    id :: Kernel.Types.Id.Id Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping,
                                    seatLayoutId :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout,
                                    vehicleNo :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data VehicleSeatLayoutMappingUpsertReq
    = VehicleSeatLayoutMappingUpsertReq {gtfsId :: Data.Text.Text, seatLayoutId :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout, vehicleNo :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets VehicleSeatLayoutMappingUpsertReq
    where hideSecrets = Kernel.Prelude.identity
type API = ("vehicleSeatLayoutMapping" :> (ListVehicleSeatLayoutMapping :<|> UpsertVehicleSeatLayoutMapping :<|> DeleteVehicleSeatLayoutMapping))
type ListVehicleSeatLayoutMapping = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> MandatoryQueryParam "gtfsId" Data.Text.Text :> Get ('[JSON])
                                                                                                                                                                                     [VehicleSeatLayoutMappingItem])
type UpsertVehicleSeatLayoutMapping = ("upsert" :> ReqBody ('[JSON]) VehicleSeatLayoutMappingUpsertReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
type DeleteVehicleSeatLayoutMapping = (Capture "vehicleNo" Data.Text.Text :> Capture "gtfsId" Data.Text.Text :> "delete" :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess)
data VehicleSeatLayoutMappingAPIs
    = VehicleSeatLayoutMappingAPIs {listVehicleSeatLayoutMapping :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Data.Text.Text -> EulerHS.Types.EulerClient [VehicleSeatLayoutMappingItem]),
                                    upsertVehicleSeatLayoutMapping :: (VehicleSeatLayoutMappingUpsertReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
                                    deleteVehicleSeatLayoutMapping :: (Data.Text.Text -> Data.Text.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}
mkVehicleSeatLayoutMappingAPIs :: (Client EulerHS.Types.EulerClient API -> VehicleSeatLayoutMappingAPIs)
mkVehicleSeatLayoutMappingAPIs vehicleSeatLayoutMappingClient = (VehicleSeatLayoutMappingAPIs {..})
                                   where listVehicleSeatLayoutMapping :<|> upsertVehicleSeatLayoutMapping :<|> deleteVehicleSeatLayoutMapping = vehicleSeatLayoutMappingClient
data VehicleSeatLayoutMappingUserActionType
    = LIST_VEHICLE_SEAT_LAYOUT_MAPPING | UPSERT_VEHICLE_SEAT_LAYOUT_MAPPING | DELETE_VEHICLE_SEAT_LAYOUT_MAPPING
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''VehicleSeatLayoutMappingUserActionType)])

