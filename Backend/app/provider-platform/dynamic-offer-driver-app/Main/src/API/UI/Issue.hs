module API.UI.Issue where

import qualified API.Types.ProviderPlatform.Management.Ride as DRide
import qualified API.Types.ProviderPlatform.Management.Ride as PPMR
import qualified AWS.S3 as S3
import qualified Data.Text as T
import Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DR
import Environment
import EulerHS.Prelude hiding (elem, id)
import qualified IssueManagement.API.UI.Issue as IA
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueOption as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQIssueConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as TIT
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import Servant hiding (throwError)
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.IssueConfig (IssueConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import Tools.Auth
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Ticket as TT

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
        :<|> igmIssueStatus (personId, merchantId, merchantOpCityId)
        :<|> postChatMessage (personId, merchantId)
        :<|> getChatMessages (personId, merchantId)
        :<|> postChatRead (personId, merchantId)
        :<|> getChatState (personId, merchantId)

postChatMessage ::
  (Id SP.Person, Id DM.Merchant) ->
  Id Domain.IssueReport ->
  Common.CreateChatMessageReq ->
  FlowHandler Common.ChatMessageItem
postChatMessage (driverId, _) issueReportId req =
  withFlowHandlerAPI $ Common.createChatMessage (cast driverId) issueReportId Common.DRIVER driverIssueHandle req

getChatMessages ::
  (Id SP.Person, Id DM.Merchant) ->
  Id Domain.IssueReport ->
  Maybe UTCTime ->
  Maybe Int ->
  FlowHandler [Common.ChatMessageItem]
getChatMessages (driverId, _) issueReportId mbSince mbLimit =
  withFlowHandlerAPI $ Common.listChatMessages (cast driverId) issueReportId Common.DRIVER mbSince mbLimit

postChatRead ::
  (Id SP.Person, Id DM.Merchant) ->
  Id Domain.IssueReport ->
  Common.MarkChatReadReq ->
  FlowHandler APISuccess
postChatRead (driverId, _) issueReportId req =
  withFlowHandlerAPI $ Common.markChatRead (cast driverId) issueReportId Common.DRIVER req

getChatState ::
  (Id SP.Person, Id DM.Merchant) ->
  Id Domain.IssueReport ->
  FlowHandler Common.ChatStateRes
getChatState (driverId, _) issueReportId =
  withFlowHandlerAPI $ Common.getChatState (cast driverId) issueReportId

driverIssueHandle :: Common.ServiceHandle Flow
driverIssueHandle =
  Common.ServiceHandle
    { findPersonById = castPersonById,
      findRideById = castRideById,
      findMOCityById = castMOCityById,
      findMOCityByMerchantShortIdAndCity = castMOCityByMerchantShortIdAndCity,
      findByBookingId = castBookingById,
      findOneByBookingId = castRideByBookingId,
      findByMerchantId = castMerchantById,
      getRideInfo = castRideInfo,
      createTicket = castCreateTicket,
      updateTicket = castUpdateTicket,
      kaptureGetTicket = Nothing,
      getTicketStatus = Nothing,
      findMerchantConfig = buildMerchantConfig,
      mbCounterpartDashboardInfo = Nothing,
      mbReportACIssue = Nothing,
      mbReportIssue = Nothing,
      mbFindLatestBookingByPersonId = Nothing,
      mbFindRideByBookingId = Nothing,
      mbSyncRide = Nothing,
      mbSendUnattendedTicketAlert = Nothing,
      findRideByRideShortId = castRideByRideShortId,
      findByMobileNumberAndMerchantId = castPersonByMobileNumberAndMerchant,
      mbFindFRFSTicketBookingById = Nothing,
      mbFindStationByIdWithContext = Nothing,
      mbSendChatNotification = Just (\pid payload -> Notify.notifyOnIssueChatMessage (cast pid) payload),
      -- Nothing = never forward driver-side chat messages to the ticket
      -- service. Driver-app doesn't use XyneSpaces today; enable this by
      -- returning a check against MerchantServiceUsageConfig if that changes.
      mbShouldForwardChatToTicketService = Nothing,
      mbFetchMediaBase64 = Just fetchMediaBase64FromS3,
      findIssueConfig = \mocId issueIdentifier ->
        getConfig (IssueConfigDimensions {merchantOperatingCityId = mocId.getId, identifier = show issueIdentifier}) (Just (CQIssueConfig.findByMerchantOpCityId mocId Common.DRIVER)),
      mbUpdateTicketOnService = Nothing,
      mbUpdateTicketStatus = Just castUpdateTicketStatus,
      mbUpdateTicketCsat = Just castUpdateTicketCsat
    }

-- | Fetch a MediaFile's bytes directly from S3 (returning the base64 payload
-- 'AWS.S3.get' produces). Same mechanism the shared IssueManagement handler
-- uses to embed attachments as @data:@ URIs on outbound ticket calls.
fetchMediaBase64FromS3 :: DMF.MediaFile -> Flow (Maybe Text)
fetchMediaBase64FromS3 mf = case mf.s3FilePath of
  Just s3Key -> Just <$> S3.get (T.unpack s3Key)
  Nothing -> pure Nothing

castPersonById :: Id Common.Person -> Flow (Maybe Common.Person)
castPersonById driverId = do
  person <- runInReplica $ QP.findById (cast driverId)
  return $ mkPerson <$> person

castPersonByMobileNumberAndMerchant :: Text -> DbHash -> Id Common.Merchant -> Flow (Maybe Common.Person)
castPersonByMobileNumberAndMerchant mobileCountryCode numHash merchantId = do
  mbPerson <- runInReplica $ QP.findByMobileNumberAndMerchantAndRole mobileCountryCode numHash (cast merchantId) SP.DRIVER
  return $ mkPerson <$> mbPerson

mkPerson :: SP.Person -> Common.Person
mkPerson person =
  Common.Person
    { id = cast person.id,
      language = person.language,
      firstName = Just person.firstName,
      lastName = person.lastName,
      middleName = person.middleName,
      mobileNumber = person.mobileNumber,
      merchantOperatingCityId = cast person.merchantOperatingCityId,
      blocked = Nothing,
      merchantId = cast person.merchantId
    }

castRideById :: Id Common.Ride -> Id Common.Merchant -> Flow (Maybe Common.Ride)
castRideById rideId merchantId = do
  ride <- runInReplica $ QR.findById (cast rideId)
  return $ mkRide merchantId <$> ride

castRideByRideShortId :: Id Common.Merchant -> ShortId Common.Ride -> Flow (Maybe Common.Ride)
castRideByRideShortId merchantId (ShortId rideShortId) = do
  mbRide <- runInReplica $ QR.findRideByRideShortId (ShortId rideShortId)
  return $ mkRide merchantId <$> mbRide

mkRide :: Id Common.Merchant -> DR.Ride -> Common.Ride
mkRide merchantId ride = do
  Common.Ride
    { id = cast ride.id,
      shortId = ShortId ride.shortId.getShortId,
      merchantOperatingCityId = cast ride.merchantOperatingCityId,
      counterPartyRideId = Nothing,
      createdAt = ride.createdAt,
      merchantId = maybe merchantId cast ride.merchantId,
      driverId = cast <$> (Just ride.driverId)
    }

castMOCityById :: Id Common.MerchantOperatingCity -> Flow (Maybe Common.MerchantOperatingCity)
castMOCityById moCityId = do
  moCity <- CQMOC.findById (cast moCityId)
  return $ fmap castMOCity moCity

castMOCityByMerchantShortIdAndCity :: ShortId Common.Merchant -> Context.City -> Flow (Maybe Common.MerchantOperatingCity)
castMOCityByMerchantShortIdAndCity (ShortId merchantShortId) opCity = do
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity (ShortId merchantShortId) opCity
  return $ fmap castMOCity merchantOpCity

castMOCity :: DMOC.MerchantOperatingCity -> Common.MerchantOperatingCity
castMOCity moCity =
  Common.MerchantOperatingCity
    { id = cast moCity.id,
      city = moCity.city,
      merchantId = cast moCity.merchantId,
      merchantShortId = ShortId moCity.merchantShortId.getShortId
    }

castBookingById :: Id Common.Booking -> Flow (Maybe Common.Booking)
castBookingById bookingId =
  runInReplica (QB.findById $ cast bookingId) >>= \case
    Nothing -> pure Nothing
    Just booking -> Just . castBooking booking <$> parseBaseUrl booking.bapUri
  where
    castBooking booking bapUri =
      Common.Booking
        { id = cast booking.id,
          bapId = Just booking.bapId,
          bapUri = Just bapUri,
          bppId = Nothing,
          bppUri = Nothing,
          quoteId = Nothing,
          providerId = cast booking.providerId,
          merchantOperatingCityId = cast booking.merchantOperatingCityId
        }

castRideByBookingId :: Id Common.Booking -> Id Common.Merchant -> Flow (Maybe Common.Ride)
castRideByBookingId bookingId merchantId = do
  ride <- runInReplica $ QR.findOneByBookingId (cast bookingId)
  return $ fmap castRideMapping ride
  where
    castRideMapping ride =
      Common.Ride (cast ride.id) (ShortId ride.shortId.getShortId) (cast ride.merchantOperatingCityId) ride.createdAt Nothing (maybe merchantId cast ride.merchantId) (cast <$> (Just ride.driverId))

castMerchantById :: Id Common.Merchant -> Flow (Maybe Common.Merchant)
castMerchantById merchantId = do
  merchant <- runInReplica $ QM.findById (cast merchantId)
  return $ fmap castMerchant merchant
  where
    castMerchant merchant =
      Common.Merchant
        { id = cast merchant.id,
          shortId = ShortId merchant.shortId.getShortId,
          subscriberId = ShortId merchant.subscriberId.getShortId
        }

castRideInfo ::
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Id Common.Ride ->
  Flow Common.RideInfoRes
castRideInfo merchantId merchantOpCityId rideId = do
  rideInfoRes <-
    Redis.safeGet makeRideInfoCacheKey >>= \case
      Just res -> pure res
      Nothing -> cacheRideInfo /=<< DRide.rideInfo (cast merchantId) (cast merchantOpCityId) (cast rideId) Nothing
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
          vehicleVariant = res.vehicleVariant,
          vehicleServiceTierName = Just $ show res.vehicleServiceTierName,
          actualFare = res.actualFare,
          bookingStatus = Just $ castBookingStatus res.bookingStatus,
          merchantOperatingCityId = res.merchantOperatingCityId,
          estimatedDistance = metersToHighPrecMeters <$> res.rideDistanceEstimated,
          chargeableDistance = Just $ metersToHighPrecMeters res.rideDistanceActual,
          estimatedFare = toHighPrecMoney res.estimatedFare,
          computedPrice = toHighPrecMoney <$> res.actualFare,
          fareBreakup = [],
          rideCreatedAt = res.rideCreatedAt,
          rideStartTime = res.rideStartTime,
          rideStatus = castRideStatus res.rideStatus,
          mobileCountryCode = Nothing
        }

    castBookingStatus :: DRide.BookingStatus -> Common.BookingStatus
    castBookingStatus = \case
      DRide.UPCOMING -> Common.UPCOMING
      DRide.UPCOMING_6HRS -> Common.UPCOMING_6HRS
      DRide.ONGOING -> Common.ONGOING
      DRide.ONGOING_6HRS -> Common.ONGOING_6HRS
      DRide.COMPLETED -> Common.COMPLETED
      DRide.CANCELLED -> Common.CANCELLED

    castRideStatus :: DRide.RideStatus -> Common.RideStatus
    castRideStatus = \case
      DRide.RIDE_UPCOMING -> Common.R_UPCOMING
      DRide.RIDE_NEW -> Common.R_NEW
      DRide.RIDE_INPROGRESS -> Common.R_INPROGRESS
      DRide.RIDE_COMPLETED -> Common.R_COMPLETED
      DRide.RIDE_CANCELLED -> Common.R_CANCELLED

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

    makeRideInfoCacheKey :: Text
    makeRideInfoCacheKey = "CachedQueries:RideInfo:RideId-" <> show rideId.getId

    cacheRideInfo :: CacheFlow m r => DRide.RideInfoRes -> m ()
    cacheRideInfo rideInfoRes = do
      let shouldCacheRideInfo = elem (rideInfoRes.bookingStatus) [PPMR.COMPLETED, PPMR.CANCELLED]
      bool (return ()) (Redis.setExp makeRideInfoCacheKey rideInfoRes 259200) shouldCacheRideInfo

-- Driver-app does not fan out ticket writes to a secondary provider, so
-- create returns the primary response paired with 'Nothing' and update
-- ignores the trailing secondary-ticketId argument.
castCreateTicket :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.CreateTicketReq -> Flow (TIT.CreateTicketResp, Maybe Text)
castCreateTicket merchantId merchantOpCityId req = do
  resp <- TT.createTicket (cast merchantId) (cast merchantOpCityId) req
  pure (resp, Nothing)

castUpdateTicket :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> Maybe Text -> TIT.UpdateTicketReq -> Flow TIT.UpdateTicketResp
castUpdateTicket merchantId merchantOperatingCityId _additionalTicketId = TT.updateTicket (cast merchantId) (cast merchantOperatingCityId)

castUpdateTicketStatus :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.UpdateTicketStatusReq -> Flow ()
castUpdateTicketStatus merchantId merchantOperatingCityId = TT.updateTicketStatus (cast merchantId) (cast merchantOperatingCityId)

castUpdateTicketCsat :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.UpdateTicketCsatReq -> Flow ()
castUpdateTicketCsat merchantId merchantOperatingCityId = TT.updateTicketCsat (cast merchantId) (cast merchantOperatingCityId)

buildMerchantConfig :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> Maybe (Id Common.Person) -> Flow Common.MerchantConfig
buildMerchantConfig _merchantId merchantOpCityId _mbPersonId = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId (cast merchantOpCityId) Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  appBackendBapInternal <- asks (.appBackendBapInternal)
  return
    Common.MerchantConfig
      { mediaFileSizeUpperLimit = transporterConfig.mediaFileSizeUpperLimit,
        mediaFileUrlPattern = transporterConfig.mediaFileUrlPattern,
        dashboardMediaFileUrlPattern = Nothing,
        kaptureDisposition = transporterConfig.kaptureDisposition,
        kaptureQueue = transporterConfig.kaptureQueue,
        counterPartyUrl = appBackendBapInternal.url,
        counterPartyApiKey = appBackendBapInternal.apiKey,
        sensitiveWords = Nothing,
        sensitiveWordsForExactMatch = Nothing
      }

issueReportDriverList :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportDriverList (driverId, merchantId, merchantOpCityId) language = withFlowHandlerAPI $ Common.issueReportList (cast driverId, cast merchantId, cast merchantOpCityId) language driverIssueHandle Common.DRIVER

fetchMedia :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> FlowHandler Text
fetchMedia (driverId, merchantId, _) filePath = withFlowHandlerAPI $ Common.fetchMedia (cast driverId, cast merchantId) filePath

createIssueReport :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (driverId, merchantId, _merchantOpCityId) mbLanguage req = withFlowHandlerAPI $ Common.createIssueReport (cast driverId, cast merchantId) mbLanguage req driverIssueHandle Common.DRIVER Nothing

issueMediaUpload :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (driverId, merchantId, _merchantOpCityId) req = withFlowHandlerAPI $ Common.issueMediaUpload (cast driverId, cast merchantId) driverIssueHandle req

issueInfo :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo (driverId, merchantId, merchantOpCityId) issueReportId language = withFlowHandlerAPI $ Common.issueInfo issueReportId (cast driverId, cast merchantId, cast merchantOpCityId) language driverIssueHandle Common.DRIVER

updateIssueOption :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption (driverId, merchantId, _) issueReportId req = withFlowHandlerAPI $ Common.updateIssueOption issueReportId (cast driverId, cast merchantId) req Common.DRIVER

deleteIssue :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> FlowHandler APISuccess
deleteIssue (driverId, merchantId, _) issueReportId = withFlowHandlerAPI $ Common.deleteIssue issueReportId (cast driverId, cast merchantId) Common.DRIVER

getIssueCategory :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (driverId, merchantId, merchantOperatingCityId) language = withFlowHandlerAPI $ Common.getIssueCategory (cast driverId, cast merchantId, cast merchantOperatingCityId) language driverIssueHandle Common.DRIVER

getIssueOption :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueCategory -> Maybe (Id Domain.IssueOption) -> Maybe (Id Domain.IssueReport) -> Maybe (Id Common.Ride) -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (driverId, merchantId, merchantOpCityId) issueCategoryId issueOptionId issueReportId mbRideId language = withFlowHandlerAPI $ Common.getIssueOption (cast driverId, cast merchantId, cast merchantOpCityId) issueCategoryId issueOptionId issueReportId mbRideId language driverIssueHandle Common.DRIVER

igmIssueStatus :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
igmIssueStatus _ = withFlowHandlerAPI $ throwError $ InvalidRequest "IGM Issue Status should not be called by BPP"

updateIssueStatus :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.IssueReport -> Maybe Language -> Common.IssueStatusUpdateReq -> FlowHandler Common.IssueStatusUpdateRes
updateIssueStatus (driverId, merchantId, merchantOpCityId) issueReportId language req = withFlowHandlerAPI $ Common.updateIssueStatus (cast driverId, cast merchantId, cast merchantOpCityId) issueReportId language req driverIssueHandle Common.DRIVER
