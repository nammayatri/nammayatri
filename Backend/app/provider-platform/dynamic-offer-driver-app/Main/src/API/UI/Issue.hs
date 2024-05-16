module API.UI.Issue where

import qualified Dashboard.ProviderPlatform.Ride as DRide
import Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.External.LocationTrackingService.Types
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import Tools.Auth
import Tools.Error
import qualified Tools.Ticket as TT
import Utils.Common.Cac.KeyNameConstants

type API =
  "issue" :> TokenAuth :> IA.IssueAPI

handler :: FlowServer API
handler = externalHandler
  where
    externalHandler (personId, merchantId, merchantOpCityId) =
      createIssueReport (personId, merchantId, merchantOpCityId)
        :<|> issueReportDriverList (personId, merchantId, merchantOpCityId)
        :<|> issueMediaUpload (personId, merchantId, merchantOpCityId)
        :<|> fetchMedia (personId, merchantId, merchantOpCityId)
        :<|> getIssueCategory (personId, merchantId, merchantOpCityId)
        :<|> getIssueOption (personId, merchantId, merchantOpCityId)
        :<|> issueInfo (personId, merchantId, merchantOpCityId)
        :<|> updateIssueOption (personId, merchantId, merchantOpCityId)
        :<|> deleteIssue (personId, merchantId, merchantOpCityId)
        :<|> updateIssueStatus (personId, merchantId, merchantOpCityId)

driverIssueHandle :: Common.ServiceHandle Flow
driverIssueHandle =
  Common.ServiceHandle
    { findPersonById = castPersonById,
      findRideById = castRideById,
      findMOCityById = castMOCityById,
      getRideInfo = castRideInfo,
      createTicket = castCreateTicket,
      updateTicket = castUpdateTicket,
      findMerchantConfig = buildMerchantConfig,
      mbReportACIssue = Nothing
    }

castPersonById :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id Common.Person -> m (Maybe Common.Person)
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
          mobileNumber = person.mobileNumber,
          merchantOperatingCityId = cast person.merchantOperatingCityId
        }

castRideById :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id Common.Ride -> Id Common.Merchant -> m (Maybe Common.Ride)
castRideById rideId _ = do
  ride <- runInReplica $ QR.findById (cast rideId)
  return $ fmap castRide ride
  where
    castRide ride =
      Common.Ride (cast ride.id) (ShortId ride.shortId.getShortId) (cast ride.merchantOperatingCityId) ride.createdAt Nothing

castMOCityById :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id Common.MerchantOperatingCity -> m (Maybe Common.MerchantOperatingCity)
castMOCityById moCityId = do
  moCity <- CQMOC.findById (cast moCityId)
  return $ fmap castMOCity moCity
  where
    castMOCity moCity =
      Common.MerchantOperatingCity
        { id = cast moCity.id,
          merchantId = cast moCity.merchantId,
          city = moCity.city
        }

castRideInfo ::
  ( EncFlow m r,
    KvDbFlow m r,
    HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]
  ) =>
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Id Common.Ride ->
  m Common.RideInfoRes
castRideInfo merchantId merchantOpCityId rideId = do
  rideInfoRes <- DRide.rideInfo (cast merchantId) (cast merchantOpCityId) (cast rideId)
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
          vehicleVariant = show <$> res.vehicleVariant,
          vehicleServiceTier = Just $ show res.vehicleServiceTierName,
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

castCreateTicket :: (EncFlow m r, KvDbFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.CreateTicketReq -> m TIT.CreateTicketResp
castCreateTicket merchantId merchantOpCityId = TT.createTicket (cast merchantId) (cast merchantOpCityId)

castUpdateTicket :: (EncFlow m r, KvDbFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.UpdateTicketReq -> m TIT.UpdateTicketResp
castUpdateTicket merchantId merchantOperatingCityId = TT.updateTicket (cast merchantId) (cast merchantOperatingCityId)

buildMerchantConfig :: (KvDbFlow m r, HasFlowEnv m r '["appBackendBapInternal" ::: CallBAPInternal.AppBackendBapInternal]) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> Id Common.Person -> m Common.MerchantConfig
buildMerchantConfig _merchantId merchantOpCityId personId = do
  transporterConfig <- CCT.findByMerchantOpCityId (cast merchantOpCityId) (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  appBackendBapInternal <- asks (.appBackendBapInternal)
  return
    Common.MerchantConfig
      { mediaFileSizeUpperLimit = transporterConfig.mediaFileSizeUpperLimit,
        mediaFileUrlPattern = transporterConfig.mediaFileUrlPattern,
        kaptureDisposition = transporterConfig.kaptureDisposition,
        kaptureQueue = transporterConfig.kaptureQueue,
        counterPartyUrl = appBackendBapInternal.url,
        counterPartyApiKey = appBackendBapInternal.apiKey
      }

issueReportDriverList :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportDriverList (driverId, merchantId, merchantOpCityId) language = withFlowHandlerAPI $ Common.issueReportList (cast driverId, cast merchantId, cast merchantOpCityId) language driverIssueHandle Common.DRIVER

fetchMedia :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> FlowHandler Text
fetchMedia (driverId, merchantId, _) filePath = withFlowHandlerAPI $ Common.fetchMedia (cast driverId, cast merchantId) filePath

createIssueReport :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (driverId, merchantId, _merchantOpCityId) mbLanguage req = withFlowHandlerAPI $ Common.createIssueReport (cast driverId, cast merchantId) mbLanguage req driverIssueHandle Common.DRIVER

issueMediaUpload :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (driverId, merchantId, _merchantOpCityId) req = withFlowHandlerAPI $ Common.issueMediaUpload (cast driverId, cast merchantId) driverIssueHandle req

issueInfo :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo (driverId, merchantId, _) issueReportId language = withFlowHandlerAPI $ Common.issueInfo issueReportId (cast driverId, cast merchantId) language driverIssueHandle Common.DRIVER

updateIssueOption :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption (driverId, merchantId, _) issueReportId req = withFlowHandlerAPI $ Common.updateIssueOption issueReportId (cast driverId, cast merchantId) req Common.DRIVER

deleteIssue :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> FlowHandler APISuccess
deleteIssue (driverId, merchantId, _) issueReportId = withFlowHandlerAPI $ Common.deleteIssue issueReportId (cast driverId, cast merchantId) Common.DRIVER

getIssueCategory :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (driverId, merchantId, _) language = withFlowHandlerAPI $ Common.getIssueCategory (cast driverId, cast merchantId) language driverIssueHandle Common.DRIVER

getIssueOption :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueCategory -> Maybe (Id Domain.IssueOption) -> Maybe (Id Domain.IssueReport) -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (driverId, merchantId, _) issueCategoryId issueOptionId issueReportId language = withFlowHandlerAPI $ Common.getIssueOption (cast driverId, cast merchantId) issueCategoryId issueOptionId issueReportId language driverIssueHandle Common.DRIVER

updateIssueStatus :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> Maybe Language -> Common.IssueStatusUpdateReq -> FlowHandler Common.IssueStatusUpdateRes
updateIssueStatus (driverId, merchantId, merchantOpCityId) issueReportId language req = withFlowHandlerAPI $ Common.updateIssueStatus (cast driverId, cast merchantId, cast merchantOpCityId) issueReportId language req driverIssueHandle Common.DRIVER
