{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.FRFSTicket where

import qualified BecknV2.FRFS.Enums
import qualified Dashboard.Common
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
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Servant
import Servant.Client

data FRFSDashboardRouteAPI = FRFSDashboardRouteAPI
  { code :: Data.Text.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    integratedBppConfigId :: Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig,
    longName :: Data.Text.Text,
    routeTag :: Kernel.Prelude.Maybe Data.Text.Text,
    shortName :: Data.Text.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

data FRFSGtfsFareAPI = FRFSGtfsFareAPI {code :: Data.Text.Text, name :: Kernel.Prelude.Maybe Data.Text.Text, providerCode :: Kernel.Prelude.Maybe Data.Text.Text, validityDuration :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSGtfsRes = FRFSGtfsRes
  { fares :: [FRFSGtfsFareAPI],
    integratedBppConfigId :: Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig,
    ready :: Kernel.Prelude.Bool,
    routeStops :: [FRFSGtfsRouteStopAPI],
    routes :: [FRFSGtfsRouteAPI],
    stops :: [FRFSGtfsStopAPI]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSGtfsRouteAPI = FRFSGtfsRouteAPI {code :: Data.Text.Text, shortName :: Data.Text.Text, variant :: Kernel.Prelude.Maybe Data.Text.Text, vehicleType :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSGtfsRouteStopAPI = FRFSGtfsRouteStopAPI {routeCode :: Data.Text.Text, sequenceNum :: Kernel.Prelude.Int, stopCode :: Data.Text.Text, stopType :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSGtfsStopAPI = FRFSGtfsStopAPI {code :: Data.Text.Text, lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double, lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double, name :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show, Eq)
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

data FRFSStageFareAPI = FRFSStageFareAPI
  { amount :: Kernel.Types.Common.HighPrecMoney,
    cessCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    routeTag :: Kernel.Prelude.Maybe Data.Text.Text,
    serviceTier :: BecknV2.FRFS.Enums.ServiceTierType,
    stage :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStartStopsAPI = FRFSStartStopsAPI {code :: Data.Text.Text, lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double, lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double, name :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Kernel.Prelude.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    hindiName :: Kernel.Prelude.Maybe Data.Text.Text,
    integratedBppConfigId :: Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    name :: Data.Text.Text,
    regionalName :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
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

newtype FRFSStatusUpdateReq = FRFSStatusUpdateReq {bookingIds :: [Kernel.Types.Id.Id Dashboard.Common.FRFSTicketBooking]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FRFSStatusUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data FRFSStopFareMatrixAPI = FRFSStopFareMatrixAPI {endStops :: [FRFSEndStopsFareAPI], startStop :: FRFSStartStopsAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelAction
  = CANCEL
  | NOT_AFFECTED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelBookingPreview = TripCancelBookingPreview
  { action :: TripCancelAction,
    bookingId :: Kernel.Types.Id.Id Dashboard.Common.FRFSTicketBooking,
    checkedIn :: Kernel.Prelude.Bool,
    fromStop :: TripCancelStop,
    multimodalOrder :: Kernel.Prelude.Bool,
    passTripsCredited :: Kernel.Prelude.Int,
    paymentMode :: TripCancelPaymentMode,
    quantity :: Kernel.Prelude.Int,
    refundAmount :: Kernel.Types.Common.HighPrecMoney,
    riderId :: Data.Text.Text,
    toStop :: TripCancelStop
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelBookingStatus = TripCancelBookingStatus
  { bookingId :: Kernel.Types.Id.Id Dashboard.Common.FRFSTicketBooking,
    bookingStatus :: Data.Text.Text,
    fromStop :: TripCancelStop,
    passTripsCredited :: Kernel.Prelude.Int,
    quantity :: Kernel.Prelude.Int,
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    refundStatus :: Kernel.Prelude.Maybe Data.Text.Text,
    toStop :: TripCancelStop
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelOperation = TripCancelOperation
  { actorId :: Kernel.Prelude.Maybe Data.Text.Text,
    cancelled :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    failed :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    operationId :: Data.Text.Text,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requested :: Kernel.Prelude.Int,
    status :: Data.Text.Text,
    stopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    tripId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelPaymentMode
  = ONLINE
  | PASS
  | PASS_PARTIAL
  | FREE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelPreviewResp = TripCancelPreviewResp
  { bookings :: [TripCancelBookingPreview],
    cancellations :: [TripCancelOperation],
    cutoffStop :: Kernel.Prelude.Maybe TripCancelStop,
    stopBreakdown :: [TripCancelStopBreakdown],
    summary :: TripCancelSummary,
    tripId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelReq = TripCancelReq {bookingIds :: [Kernel.Types.Id.Id Dashboard.Common.FRFSTicketBooking], reason :: Data.Text.Text, stopCode :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TripCancelReq where
  hideSecrets = Kernel.Prelude.identity

data TripCancelResp = TripCancelResp {accepted :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelStatusResp = TripCancelStatusResp
  { bookings :: [TripCancelBookingStatus],
    cancellations :: [TripCancelOperation],
    cancelled :: Kernel.Prelude.Int,
    refundFailed :: Kernel.Prelude.Int,
    refundPending :: Kernel.Prelude.Int,
    refunded :: Kernel.Prelude.Int,
    stillPending :: Kernel.Prelude.Int,
    tripId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelStop = TripCancelStop {code :: Data.Text.Text, name :: Kernel.Prelude.Maybe Data.Text.Text, seq :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelStopBreakdown = TripCancelStopBreakdown
  { bookings :: Kernel.Prelude.Int,
    passTripsCredited :: Kernel.Prelude.Int,
    passengers :: Kernel.Prelude.Int,
    refundAmount :: Kernel.Types.Common.HighPrecMoney,
    stop :: TripCancelStop
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripCancelSummary = TripCancelSummary
  { checkedInToCancel :: Kernel.Prelude.Int,
    notAffected :: Kernel.Prelude.Int,
    passTripsCredited :: Kernel.Prelude.Int,
    passengersToCancel :: Kernel.Prelude.Int,
    toCancel :: Kernel.Prelude.Int,
    totalBookings :: Kernel.Prelude.Int,
    totalRefundAmount :: Kernel.Types.Common.HighPrecMoney
  }
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

newtype UpsertStageFareReq = UpsertStageFareReq {file :: EulerHS.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertStageFareReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertStageFareResp = UpsertStageFareResp {success :: Data.Text.Text, unprocessedStageFares :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = "fRFSTicket" :> (GetFRFSTicketFrfsRoutes :<|> (GetFRFSTicketFrfsRouteFareList :<|> (PutFRFSTicketFrfsRouteFareUpsert :<|> (GetFRFSTicketFrfsStageFareList :<|> (PutFRFSTicketFrfsStageFareUpsert :<|> (GetFRFSTicketFrfsRouteStations :<|> (GetFRFSTicketFrfsGtfs :<|> (PostFRFSTicketFrfsStatusUpdateHelper :<|> (GetFRFSTicketFrfsTripCancelPreview :<|> (PostFRFSTicketFrfsTripCancelHelper :<|> (GetFRFSTicketFrfsTripCancellations :<|> GetFRFSTicketFrfsTripCancelStatus)))))))))))

type GetFRFSTicketFrfsRoutes =
  "frfs"
    :> ( "routes"
           :> ( QueryParam "searchStr" Data.Text.Text
                  :> ( MandatoryQueryParam "limit" Kernel.Prelude.Int
                         :> ( MandatoryQueryParam
                                "offset"
                                Kernel.Prelude.Int
                                :> ( MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
                                       :> Get
                                            '[JSON]
                                            [FRFSDashboardRouteAPI]
                                   )
                            )
                     )
              )
       )

type GetFRFSTicketFrfsRouteFareList =
  "frfs"
    :> ( "route"
           :> ( "fare"
                  :> ( Capture "routeCode" Data.Text.Text
                         :> ( "list"
                                :> ( MandatoryQueryParam
                                       "integratedBppConfigId"
                                       (Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig)
                                       :> ( MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
                                              :> Get
                                                   '[JSON]
                                                   FRFSRouteFareAPI
                                          )
                                   )
                            )
                     )
              )
       )

type PutFRFSTicketFrfsRouteFareUpsert =
  "frfs"
    :> ( "route"
           :> ( "fare"
                  :> ( Capture "routeCode" Data.Text.Text
                         :> ( "upsert"
                                :> ( MandatoryQueryParam
                                       "integratedBppConfigId"
                                       (Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig)
                                       :> ( MandatoryQueryParam
                                              "vehicleType"
                                              BecknV2.FRFS.Enums.VehicleCategory
                                              :> ( Kernel.ServantMultipart.MultipartForm
                                                     Kernel.ServantMultipart.Tmp
                                                     UpsertRouteFareReq
                                                     :> Put
                                                          '[JSON]
                                                          UpsertRouteFareResp
                                                 )
                                          )
                                   )
                            )
                     )
              )
       )

type GetFRFSTicketFrfsStageFareList = "frfs" :> ("stageFare" :> ("list" :> (MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory :> Get '[JSON] [FRFSStageFareAPI])))

type PutFRFSTicketFrfsStageFareUpsert =
  "frfs"
    :> ( "stageFare"
           :> ( "upsert"
                  :> ( MandatoryQueryParam
                         "integratedBppConfigId"
                         (Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig)
                         :> ( MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
                                :> ( Kernel.ServantMultipart.MultipartForm
                                       Kernel.ServantMultipart.Tmp
                                       UpsertStageFareReq
                                       :> Put
                                            '[JSON]
                                            UpsertStageFareResp
                                   )
                            )
                     )
              )
       )

type GetFRFSTicketFrfsRouteStations =
  "frfs"
    :> ( "route"
           :> ( "stations"
                  :> ( QueryParam "searchStr" Data.Text.Text
                         :> ( MandatoryQueryParam "limit" Kernel.Prelude.Int
                                :> ( MandatoryQueryParam
                                       "offset"
                                       Kernel.Prelude.Int
                                       :> ( MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
                                              :> Get
                                                   '[JSON]
                                                   [FRFSStationAPI]
                                          )
                                   )
                            )
                     )
              )
       )

type GetFRFSTicketFrfsGtfs =
  "frfs"
    :> ( "gtfs"
           :> ( QueryParam "integratedBppConfigId" (Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig)
                  :> ( QueryParam
                         "platformType"
                         Dashboard.Common.PlatformType
                         :> (MandatoryQueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory :> Get '[JSON] FRFSGtfsRes)
                     )
              )
       )

type PostFRFSTicketFrfsStatusUpdate = "frfs" :> ("statusUpdate" :> (ReqBody '[JSON] FRFSStatusUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess))

type PostFRFSTicketFrfsStatusUpdateHelper =
  "frfs"
    :> ( "statusUpdate"
           :> ( QueryParam "requestorId" Data.Text.Text
                  :> ( ReqBody '[JSON] FRFSStatusUpdateReq
                         :> Post
                              '[JSON]
                              Kernel.Types.APISuccess.APISuccess
                     )
              )
       )

type GetFRFSTicketFrfsTripCancelPreview =
  "frfs"
    :> ( "trip"
           :> ( Capture "tripId" Data.Text.Text
                  :> ( "cancel"
                         :> ( "preview"
                                :> ( QueryParam "stopCode" Data.Text.Text
                                       :> Get
                                            '[JSON]
                                            TripCancelPreviewResp
                                   )
                            )
                     )
              )
       )

type PostFRFSTicketFrfsTripCancel = "frfs" :> ("trip" :> (Capture "tripId" Data.Text.Text :> ("cancel" :> (ReqBody '[JSON] TripCancelReq :> Post '[JSON] TripCancelResp))))

type PostFRFSTicketFrfsTripCancelHelper =
  "frfs"
    :> ( "trip"
           :> ( Capture "tripId" Data.Text.Text
                  :> ( "cancel"
                         :> ( QueryParam "requestorId" Data.Text.Text
                                :> ( ReqBody
                                       '[JSON]
                                       TripCancelReq
                                       :> Post '[JSON] TripCancelResp
                                   )
                            )
                     )
              )
       )

type GetFRFSTicketFrfsTripCancellations =
  "frfs"
    :> ( "trip"
           :> ( "cancellations"
                  :> ( QueryParam "limit" Kernel.Prelude.Int
                         :> ( QueryParam "offset" Kernel.Prelude.Int
                                :> ( MandatoryQueryParam
                                       "fromDate"
                                       Kernel.Prelude.UTCTime
                                       :> ( MandatoryQueryParam "toDate" Kernel.Prelude.UTCTime
                                              :> Get
                                                   '[JSON]
                                                   [TripCancelOperation]
                                          )
                                   )
                            )
                     )
              )
       )

type GetFRFSTicketFrfsTripCancelStatus = "frfs" :> ("trip" :> (Capture "tripId" Data.Text.Text :> ("cancel" :> ("status" :> Get '[JSON] TripCancelStatusResp))))

data FRFSTicketAPIs = FRFSTicketAPIs
  { getFRFSTicketFrfsRoutes :: Kernel.Prelude.Maybe Data.Text.Text -> (Kernel.Prelude.Int -> (Kernel.Prelude.Int -> (BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [FRFSDashboardRouteAPI]))),
    getFRFSTicketFrfsRouteFareList :: Data.Text.Text -> (Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig -> (BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient FRFSRouteFareAPI)),
    putFRFSTicketFrfsRouteFareUpsert ::
      Data.Text.Text ->
      ( Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig ->
        ( BecknV2.FRFS.Enums.VehicleCategory ->
          ( ( Data.ByteString.Lazy.ByteString,
              UpsertRouteFareReq
            ) ->
            EulerHS.Types.EulerClient UpsertRouteFareResp
          )
        )
      ),
    getFRFSTicketFrfsStageFareList :: BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [FRFSStageFareAPI],
    putFRFSTicketFrfsStageFareUpsert ::
      Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig ->
      ( BecknV2.FRFS.Enums.VehicleCategory ->
        ( ( Data.ByteString.Lazy.ByteString,
            UpsertStageFareReq
          ) ->
          EulerHS.Types.EulerClient UpsertStageFareResp
        )
      ),
    getFRFSTicketFrfsRouteStations :: Kernel.Prelude.Maybe Data.Text.Text -> (Kernel.Prelude.Int -> (Kernel.Prelude.Int -> (BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [FRFSStationAPI]))),
    getFRFSTicketFrfsGtfs :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig) -> (Kernel.Prelude.Maybe Dashboard.Common.PlatformType -> (BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient FRFSGtfsRes)),
    postFRFSTicketFrfsStatusUpdate :: Kernel.Prelude.Maybe Data.Text.Text -> (FRFSStatusUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getFRFSTicketFrfsTripCancelPreview :: Data.Text.Text -> (Kernel.Prelude.Maybe Data.Text.Text -> EulerHS.Types.EulerClient TripCancelPreviewResp),
    postFRFSTicketFrfsTripCancel :: Data.Text.Text -> (Kernel.Prelude.Maybe Data.Text.Text -> (TripCancelReq -> EulerHS.Types.EulerClient TripCancelResp)),
    getFRFSTicketFrfsTripCancellations :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> (Kernel.Prelude.Maybe Kernel.Prelude.Int -> (Kernel.Prelude.UTCTime -> (Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient [TripCancelOperation]))),
    getFRFSTicketFrfsTripCancelStatus :: Data.Text.Text -> EulerHS.Types.EulerClient TripCancelStatusResp
  }

mkFRFSTicketAPIs :: Client EulerHS.Types.EulerClient API -> FRFSTicketAPIs
mkFRFSTicketAPIs fRFSTicketClient = (FRFSTicketAPIs {..})
  where
    getFRFSTicketFrfsRoutes :<|> getFRFSTicketFrfsRouteFareList :<|> putFRFSTicketFrfsRouteFareUpsert :<|> getFRFSTicketFrfsStageFareList :<|> putFRFSTicketFrfsStageFareUpsert :<|> getFRFSTicketFrfsRouteStations :<|> getFRFSTicketFrfsGtfs :<|> postFRFSTicketFrfsStatusUpdate :<|> getFRFSTicketFrfsTripCancelPreview :<|> postFRFSTicketFrfsTripCancel :<|> getFRFSTicketFrfsTripCancellations :<|> getFRFSTicketFrfsTripCancelStatus = fRFSTicketClient

data FRFSTicketUserActionType
  = GET_FRFS_TICKET_FRFS_ROUTES
  | GET_FRFS_TICKET_FRFS_ROUTE_FARE_LIST
  | PUT_FRFS_TICKET_FRFS_ROUTE_FARE_UPSERT
  | GET_FRFS_TICKET_FRFS_STAGE_FARE_LIST
  | PUT_FRFS_TICKET_FRFS_STAGE_FARE_UPSERT
  | GET_FRFS_TICKET_FRFS_ROUTE_STATIONS
  | GET_FRFS_TICKET_FRFS_GTFS
  | POST_FRFS_TICKET_FRFS_STATUS_UPDATE
  | GET_FRFS_TICKET_FRFS_TRIP_CANCEL_PREVIEW
  | POST_FRFS_TICKET_FRFS_TRIP_CANCEL
  | GET_FRFS_TICKET_FRFS_TRIP_CANCELLATIONS
  | GET_FRFS_TICKET_FRFS_TRIP_CANCEL_STATUS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FRFSTicketUserActionType])
