module API.UI.Issue where

import qualified Dashboard.ProviderPlatform.Ride as DRide
import Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.API.UI.Issue as IA
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueOption as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import Kernel.Beam.Functions
import qualified Kernel.External.Ticket.Interface.Types as TIT
import Kernel.External.Types (Language)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.External.LocationTrackingService.Types
import Storage.Beam.IssueManagement ()
import Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import Tools.Auth
import Tools.Error
import qualified Tools.Ticket as TT

type API =
  "issue" :> TokenAuth :> IA.IssueAPI

handler :: FlowServer API
handler = externalHandler
  where
    externalHandler (personId, merchantId) =
      createIssueReport (personId, merchantId)
        :<|> issueReportDriverList (personId, merchantId)
        :<|> issueMediaUpload (personId, merchantId)
        :<|> fetchMedia (personId, merchantId)
        :<|> getIssueCategory (personId, merchantId)
        :<|> getIssueOption (personId, merchantId)
        :<|> issueInfo (personId, merchantId)
        :<|> updateIssueOption (personId, merchantId)
        :<|> deleteIssue (personId, merchantId)
        :<|> updateIssueStatus (personId, merchantId)

driverIssueHandle :: Common.ServiceHandle Flow
driverIssueHandle =
  Common.ServiceHandle
    { findPersonById = castPersonById,
      findRideById = castRideById,
      getRideInfo = castRideInfo,
      findMerchant = castMerchantById,
      createTicket = castTicket
    }

castPersonById :: EsqDBReplicaFlow m r => Id Common.Person -> m (Maybe Common.Person)
castPersonById driverId = do
  person <- runInReplica $ QP.findById (cast driverId)
  return $ fmap castDriver person
  where
    castDriver person =
      Common.Person
        { id = cast person.id,
          language = person.language,
          firstName = Just person.firstName,
          lastName = person.lastName,
          middleName = person.middleName,
          mobileNumber = person.mobileNumber
        }

castRideById :: EsqDBReplicaFlow m r => Id Common.Ride -> m (Maybe Common.Ride)
castRideById rideId = do
  ride <- runInReplica $ QR.findById (cast rideId)
  return $ fmap castRide ride
  where
    castRide ride = Common.Ride (cast ride.id) (ShortId $ show ride.shortId) ride.createdAt

castRideInfo ::
  ( EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]
  ) =>
  ShortId Common.Merchant ->
  Id Common.Ride ->
  m Common.RideInfoRes
castRideInfo merchantShortId rideId = do
  rideInfoRes <- DRide.rideInfo (ShortId $ show merchantShortId) (cast rideId)
  return $ castRideInfoRes rideInfoRes
  where
    castRideInfoRes res =
      Common.RideInfoRes
        { customerName = res.customerName,
          customerPhoneNo = res.customerPhoneNo,
          customerPickupLocation = castLocationAPIEntity res.customerPickupLocation,
          customerDropLocation = castLocationAPIEntity <$> res.customerDropLocation,
          driverName = res.driverName,
          driverPhoneNo = res.driverPhoneNo,
          vehicleNo = res.vehicleNo,
          actualFare = res.actualFare,
          bookingStatus = Just $ castBookingStatus res.bookingStatus
        }

    castBookingStatus :: DRide.BookingStatus -> Common.BookingStatus
    castBookingStatus = \case
      DRide.UPCOMING -> Common.UPCOMING
      DRide.UPCOMING_6HRS -> Common.UPCOMING_6HRS
      DRide.ONGOING -> Common.ONGOING
      DRide.ONGOING_6HRS -> Common.ONGOING_6HRS
      DRide.COMPLETED -> Common.COMPLETED
      DRide.CANCELLED -> Common.CANCELLED

    castLocationAPIEntity ent =
      Common.LocationAPIEntity
        { lat = ent.lat,
          lon = ent.lon,
          street = Nothing,
          city = Nothing,
          state = Nothing,
          country = Nothing,
          building = Nothing,
          areaCode = Nothing,
          area = Nothing
        }

castTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Common.Merchant -> TIT.CreateTicketReq -> m TIT.CreateTicketResp
castTicket merchantId = TT.createTicket (cast merchantId)

castMerchantById :: (CacheFlow m r, EsqDBFlow m r) => Id Common.Merchant -> m (Maybe Common.Merchant)
castMerchantById merchantId = do
  merchant <- CQM.findById (cast merchantId)
  return $ fmap castMerchant merchant
  where
    castMerchant merchant = Common.Merchant (ShortId $ show merchant.shortId)

buildIssueMediaUploadConfig :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id DM.Merchant -> m Common.IssueMediaUploadConfig
buildIssueMediaUploadConfig merchantId = do
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  return
    Common.IssueMediaUploadConfig
      { mediaFileSizeUpperLimit = transporterConfig.mediaFileSizeUpperLimit,
        mediaFileUrlPattern = transporterConfig.mediaFileUrlPattern
      }

issueReportDriverList :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportDriverList (driverId, merchantId) language = withFlowHandlerAPI $ Common.issueReportList (cast driverId, cast merchantId) language driverIssueHandle Common.DRIVER

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (driverId, merchantId) filePath = withFlowHandlerAPI $ Common.fetchMedia (cast driverId, cast merchantId) filePath

createIssueReport :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (driverId, merchantId) mbLanguage req = withFlowHandlerAPI $ Common.createIssueReport (cast driverId, cast merchantId) mbLanguage req driverIssueHandle Common.DRIVER

issueMediaUpload :: (Id SP.Person, Id DM.Merchant) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (driverId, merchantId) req = withFlowHandlerAPI $ Common.issueMediaUpload (cast driverId, cast merchantId) req (buildIssueMediaUploadConfig merchantId)

issueInfo :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo (driverId, merchantId) issueReportId language = withFlowHandlerAPI $ Common.issueInfo issueReportId (cast driverId, cast merchantId) language driverIssueHandle Common.DRIVER

updateIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption (driverId, merchantId) issueReportId req = withFlowHandlerAPI $ Common.updateIssueOption issueReportId (cast driverId, cast merchantId) req Common.DRIVER

deleteIssue :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> FlowHandler APISuccess
deleteIssue (driverId, merchantId) issueReportId = withFlowHandlerAPI $ Common.deleteIssue issueReportId (cast driverId, cast merchantId) Common.DRIVER

getIssueCategory :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (driverId, merchantId) language = withFlowHandlerAPI $ Common.getIssueCategory (cast driverId, cast merchantId) language driverIssueHandle Common.DRIVER

getIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueCategory -> Maybe (Id Domain.IssueOption) -> Maybe (Id Domain.IssueReport) -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (driverId, merchantId) issueCategoryId issueOptionId issueReportId language = withFlowHandlerAPI $ Common.getIssueOption (cast driverId, cast merchantId) issueCategoryId issueOptionId issueReportId language driverIssueHandle Common.DRIVER

updateIssueStatus :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> Common.IssueStatusUpdateReq -> FlowHandler Common.IssueStatusUpdateRes
updateIssueStatus (driverId, merchantId) issueReportId language req = withFlowHandlerAPI $ Common.updateIssueStatus (cast driverId, cast merchantId) issueReportId language req driverIssueHandle Common.DRIVER
