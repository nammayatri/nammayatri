{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.FRFSTicket where

import qualified BecknV2.FRFS.Enums
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data FRFSEndStopsFareAPI = FRFSEndStopsFareAPI
  { amount :: Kernel.Types.Common.HighPrecMoney,
    code :: Data.Text.Text,
    currency :: Kernel.Types.Common.Currency,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    name :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteAPI = FRFSRouteAPI {code :: Data.Text.Text, endPoint :: Kernel.External.Maps.Types.LatLong, longName :: Data.Text.Text, shortName :: Data.Text.Text, startPoint :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteFareAPI = FRFSRouteFareAPI {code :: Data.Text.Text, fares :: [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStopFareMatrixAPI], longName :: Data.Text.Text, shortName :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteReq = FRFSRouteReq {endPoint :: Kernel.External.Maps.Types.LatLong, longName :: Data.Text.Text, shortName :: Data.Text.Text, startPoint :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FRFSRouteReq where
  hideSecrets = Kernel.Prelude.identity

data FRFSStartStopsAPI = FRFSStartStopsAPI {code :: Data.Text.Text, lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double, lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double, name :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI {address :: Kernel.Prelude.Maybe Data.Text.Text, code :: Data.Text.Text, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, name :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationReq = FRFSStationReq {address :: Kernel.Prelude.Maybe Data.Text.Text, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, name :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FRFSStationReq where
  hideSecrets = Kernel.Prelude.identity

data FRFSStopFareMatrixAPI = FRFSStopFareMatrixAPI {endStops :: [API.Types.RiderPlatform.Management.FRFSTicket.FRFSEndStopsFareAPI], startStop :: API.Types.RiderPlatform.Management.FRFSTicket.FRFSStartStopsAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UpsertRouteFareReq = UpsertRouteFareReq {file :: EulerHS.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertRouteFareReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("fRFSTicket" :> (GetFRFSTicketFrfsRoutes :<|> PostFRFSTicketFrfsRouteAdd :<|> PostFRFSTicketFrfsRouteDelete :<|> GetFRFSTicketFrfsRouteFareList :<|> PutFRFSTicketFrfsRouteFareUpsert :<|> GetFRFSTicketFrfsRouteStations :<|> PostFRFSTicketFrfsStationAdd :<|> PostFRFSTicketFrfsStationDelete))

type GetFRFSTicketFrfsRoutes =
  ( "frfs" :> "routes" :> MandatoryQueryParam "limit" Kernel.Prelude.Int :> MandatoryQueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get '[JSON] [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteAPI]
  )

type PostFRFSTicketFrfsRouteAdd =
  ( "frfs" :> "route" :> Capture "code" Data.Text.Text :> "add" :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostFRFSTicketFrfsRouteDelete =
  ( "frfs" :> "route" :> Capture "code" Data.Text.Text :> "delete" :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetFRFSTicketFrfsRouteFareList =
  ( "frfs" :> "route" :> "fare" :> Capture "routeCode" Data.Text.Text :> "list"
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get '[JSON] [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI]
  )

type PutFRFSTicketFrfsRouteFareUpsert =
  ( "frfs" :> "route" :> "fare" :> Capture "routeCode" Data.Text.Text :> "upsert"
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetFRFSTicketFrfsRouteStations =
  ( "frfs" :> "route" :> "stations" :> QueryParam "searchStr" Data.Text.Text :> MandatoryQueryParam "limit" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI]
  )

type PostFRFSTicketFrfsStationAdd =
  ( "frfs" :> "station" :> Capture "code" Data.Text.Text :> "add" :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostFRFSTicketFrfsStationDelete =
  ( "frfs" :> "station" :> Capture "code" Data.Text.Text :> "delete" :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data FRFSTicketAPIs = FRFSTicketAPIs
  { getFRFSTicketFrfsRoutes :: Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteAPI],
    postFRFSTicketFrfsRouteAdd :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFRFSTicketFrfsRouteDelete :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getFRFSTicketFrfsRouteFareList :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI],
    putFRFSTicketFrfsRouteFareUpsert ::
      Data.Text.Text ->
      BecknV2.FRFS.Enums.VehicleCategory ->
      ( Data.ByteString.Lazy.ByteString,
        API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq
      ) ->
      EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getFRFSTicketFrfsRouteStations :: Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI],
    postFRFSTicketFrfsStationAdd :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFRFSTicketFrfsStationDelete :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkFRFSTicketAPIs :: (Client EulerHS.Types.EulerClient API -> FRFSTicketAPIs)
mkFRFSTicketAPIs fRFSTicketClient = (FRFSTicketAPIs {..})
  where
    getFRFSTicketFrfsRoutes :<|> postFRFSTicketFrfsRouteAdd :<|> postFRFSTicketFrfsRouteDelete :<|> getFRFSTicketFrfsRouteFareList :<|> putFRFSTicketFrfsRouteFareUpsert :<|> getFRFSTicketFrfsRouteStations :<|> postFRFSTicketFrfsStationAdd :<|> postFRFSTicketFrfsStationDelete = fRFSTicketClient

data FRFSTicketEndpointDSL
  = GetFRFSTicketFrfsRoutesEndpoint
  | PostFRFSTicketFrfsRouteAddEndpoint
  | PostFRFSTicketFrfsRouteDeleteEndpoint
  | GetFRFSTicketFrfsRouteFareListEndpoint
  | PutFRFSTicketFrfsRouteFareUpsertEndpoint
  | GetFRFSTicketFrfsRouteStationsEndpoint
  | PostFRFSTicketFrfsStationAddEndpoint
  | PostFRFSTicketFrfsStationDeleteEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
