{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.FRFSTicket where

import qualified BecknV2.FRFS.Enums
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.StationType
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.TimeBound
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

data FRFSRouteFareAPI = FRFSRouteFareAPI {code :: Data.Text.Text, fares :: [FRFSStopFareMatrixAPI], longName :: Data.Text.Text, shortName :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteReq = FRFSRouteReq
  { color :: Kernel.Prelude.Maybe Data.Text.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    longName :: Data.Text.Text,
    polyline :: Kernel.Prelude.Maybe Data.Text.Text,
    shortName :: Data.Text.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    timeBounds :: Kernel.Types.TimeBound.TimeBound
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FRFSRouteReq where
  hideSecrets = Kernel.Prelude.identity

data FRFSStartStopsAPI = FRFSStartStopsAPI {code :: Data.Text.Text, lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double, lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double, name :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Kernel.Prelude.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    name :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationReq = FRFSStationReq
  { address :: Kernel.Prelude.Maybe Data.Text.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    name :: Data.Text.Text,
    possibleTypes :: Kernel.Prelude.Maybe [Domain.Types.StationType.StationType]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FRFSStationReq where
  hideSecrets = Kernel.Prelude.identity

data FRFSStopFareMatrixAPI = FRFSStopFareMatrixAPI {endStops :: [FRFSEndStopsFareAPI], startStop :: FRFSStartStopsAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UpsertRouteFareReq = UpsertRouteFareReq {file :: EulerHS.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertRouteFareReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertRouteFareResp = UpsertRouteFareResp {success :: Data.Text.Text, unprocessedRouteFares :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("fRFSTicket" :> (GetFRFSTicketFrfsRoutes :<|> PostFRFSTicketFrfsRouteAdd :<|> PostFRFSTicketFrfsRouteDelete :<|> GetFRFSTicketFrfsRouteFareList :<|> PutFRFSTicketFrfsRouteFareUpsert :<|> GetFRFSTicketFrfsRouteStations :<|> PostFRFSTicketFrfsStationAdd :<|> PostFRFSTicketFrfsStationDelete))

type GetFRFSTicketFrfsRoutes =
  ( "frfs" :> "routes" :> QueryParam "searchStr" Data.Text.Text :> MandatoryQueryParam "limit" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> Get '[JSON] [FRFSRouteAPI]
  )

type PostFRFSTicketFrfsRouteAdd =
  ( "frfs" :> "route" :> Capture "code" Data.Text.Text :> "add" :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           FRFSRouteReq
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
      :> Get '[JSON] FRFSRouteFareAPI
  )

type PutFRFSTicketFrfsRouteFareUpsert =
  ( "frfs" :> "route" :> "fare" :> Capture "routeCode" Data.Text.Text :> "upsert"
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp UpsertRouteFareReq
      :> Put
           '[JSON]
           UpsertRouteFareResp
  )

type GetFRFSTicketFrfsRouteStations =
  ( "frfs" :> "route" :> "stations" :> QueryParam "searchStr" Data.Text.Text :> MandatoryQueryParam "limit" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           [FRFSStationAPI]
  )

type PostFRFSTicketFrfsStationAdd =
  ( "frfs" :> "station" :> Capture "code" Data.Text.Text :> "add" :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           FRFSStationReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostFRFSTicketFrfsStationDelete =
  ( "frfs" :> "station" :> Capture "code" Data.Text.Text :> "delete" :> MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data FRFSTicketAPIs = FRFSTicketAPIs
  { getFRFSTicketFrfsRoutes :: Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [FRFSRouteAPI],
    postFRFSTicketFrfsRouteAdd :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> FRFSRouteReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFRFSTicketFrfsRouteDelete :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getFRFSTicketFrfsRouteFareList :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient FRFSRouteFareAPI,
    putFRFSTicketFrfsRouteFareUpsert :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> (Data.ByteString.Lazy.ByteString, UpsertRouteFareReq) -> EulerHS.Types.EulerClient UpsertRouteFareResp,
    getFRFSTicketFrfsRouteStations :: Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [FRFSStationAPI],
    postFRFSTicketFrfsStationAdd :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> FRFSStationReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFRFSTicketFrfsStationDelete :: Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkFRFSTicketAPIs :: (Client EulerHS.Types.EulerClient API -> FRFSTicketAPIs)
mkFRFSTicketAPIs fRFSTicketClient = (FRFSTicketAPIs {..})
  where
    getFRFSTicketFrfsRoutes :<|> postFRFSTicketFrfsRouteAdd :<|> postFRFSTicketFrfsRouteDelete :<|> getFRFSTicketFrfsRouteFareList :<|> putFRFSTicketFrfsRouteFareUpsert :<|> getFRFSTicketFrfsRouteStations :<|> postFRFSTicketFrfsStationAdd :<|> postFRFSTicketFrfsStationDelete = fRFSTicketClient

data FRFSTicketUserActionType
  = GET_FRFS_TICKET_FRFS_ROUTES
  | POST_FRFS_TICKET_FRFS_ROUTE_ADD
  | POST_FRFS_TICKET_FRFS_ROUTE_DELETE
  | GET_FRFS_TICKET_FRFS_ROUTE_FARE_LIST
  | PUT_FRFS_TICKET_FRFS_ROUTE_FARE_UPSERT
  | GET_FRFS_TICKET_FRFS_ROUTE_STATIONS
  | POST_FRFS_TICKET_FRFS_STATION_ADD
  | POST_FRFS_TICKET_FRFS_STATION_DELETE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FRFSTicketUserActionType])