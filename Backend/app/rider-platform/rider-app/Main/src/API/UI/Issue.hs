module API.UI.Issue where

import qualified Dashboard.RiderPlatform.Ride as DRR
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as DRPR
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (elem, id)
import IssueManagement.API.UI.Issue as IA
import IssueManagement.Common.UI.Issue
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueOption as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import Kernel.Beam.Functions
import qualified Kernel.External.Ticket.Interface.Types as TIT
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.CachedQueries.Person as CQPerson
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import Tools.Auth
import Tools.Error
import Tools.Ticket as TT

type API =
  "issue" :> TokenAuth :> IA.IssueAPI

handler :: FlowServer API
handler = externalHandler
  where
    externalHandler (personId, merchantId) =
      createIssueReport (personId, merchantId)
        :<|> issueReportCustomerList (personId, merchantId)
        :<|> issueMediaUpload (personId, merchantId)
        :<|> fetchMedia (personId, merchantId)
        :<|> getIssueCategory (personId, merchantId)
        :<|> getIssueOption (personId, merchantId)
        :<|> issueInfo (personId, merchantId)
        :<|> updateIssueOption (personId, merchantId)
        :<|> deleteIssue (personId, merchantId)
        :<|> updateIssueStatus (personId, merchantId)

customerIssueHandle :: Common.ServiceHandle Flow
customerIssueHandle =
  Common.ServiceHandle
    { findPersonById = castPersonById,
      findRideById = castRideById,
      findMOCityById = castMOCityById,
      findMOCityByMerchantShortIdAndCity = castMOCityByMerchantShortIdAndCity,
      getRideInfo = castRideInfo,
      createTicket = castCreateTicket,
      updateTicket = castUpdateTicket,
      findMerchantConfig = buildMerchantConfig,
      mbReportACIssue = Just reportACIssue,
      mbReportIssue = Just reportIssue
    }

castPersonById :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Common.Person -> m (Maybe Common.Person)
castPersonById personId = do
  person <- runInReplica $ QP.findById (cast personId)
  return $ fmap castPerson person
  where
    castPerson person =
      Common.Person
        { id = cast person.id,
          language = person.language,
          firstName = person.firstName,
          lastName = person.lastName,
          middleName = person.middleName,
          mobileNumber = person.mobileNumber,
          merchantOperatingCityId = cast person.merchantOperatingCityId,
          blocked = Just person.blocked
        }

castRideById :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Common.Ride -> Id Common.Merchant -> m (Maybe Common.Ride)
castRideById rideId merchantId = do
  ride <- runInReplica $ QR.findById (cast rideId)
  merchantOpCityId <- case (.merchantOperatingCityId) =<< ride of
    Just moCityId -> return moCityId
    Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (fromMaybe (cast merchantId) ((.merchantId) =<< ride))
  return $ fmap (castRide merchantOpCityId) ride
  where
    castRide moCityId ride = Common.Ride (cast ride.id) (ShortId ride.shortId.getShortId) (cast moCityId) ride.createdAt (Just ride.bppRideId.getId)

castMOCityById :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Common.MerchantOperatingCity -> m (Maybe Common.MerchantOperatingCity)
castMOCityById moCityId = do
  moCity <- CQMOC.findById (cast moCityId)
  return $ fmap castMOCity moCity

castMOCityByMerchantShortIdAndCity :: (CacheFlow m r, EsqDBFlow m r) => ShortId Common.Merchant -> Context.City -> m (Maybe Common.MerchantOperatingCity)
castMOCityByMerchantShortIdAndCity (ShortId merchantShortId) opCity = do
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity (ShortId merchantShortId) opCity
  return $ fmap castMOCity merchantOpCity

castMOCity :: DMOC.MerchantOperatingCity -> Common.MerchantOperatingCity
castMOCity moCity =
  Common.MerchantOperatingCity
    { id = cast moCity.id,
      city = moCity.city,
      merchantId = cast moCity.merchantId
    }

castRideInfo :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> Id Common.Ride -> Flow Common.RideInfoRes
castRideInfo merchantId _ rideId = do
  rideInfoRes <-
    Redis.safeGet makeRideInfoCacheKey >>= \case
      Just res -> pure res
      Nothing -> cacheRideInfo /=<< DRide.rideInfo (cast merchantId) (cast rideId)
  return $ castRideInfoRes rideInfoRes
  where
    castRideInfoRes res =
      Common.RideInfoRes
        { customerName = res.customerName,
          customerPhoneNo = fromMaybe "" res.customerPhoneNo,
          customerPickupLocation = castLocationAPIEntity res.customerPickupLocation,
          customerDropLocation = castLocationAPIEntity <$> res.customerDropLocation,
          driverName = res.driverName,
          driverPhoneNo = res.driverPhoneNo,
          vehicleNo = res.vehicleNo,
          vehicleVariant = Just (castVehicleVariant res.vehicleVariant),
          vehicleServiceTier = res.vehicleServiceTierName,
          actualFare = res.actualFare,
          bookingStatus = Nothing,
          merchantOperatingCityId = res.merchantOperatingCityId,
          estimatedDistance = res.estimatedDistance,
          chargeableDistance = res.chargeableDistance,
          estimatedFare = toHighPrecMoney res.estimatedFare,
          computedPrice = res.computedPrice,
          fareBreakup = transformFareBreakup <$> res.fareBreakup,
          rideCreatedAt = res.rideCreatedAt
        }

    castLocationAPIEntity ent =
      Common.LocationAPIEntity
        { lat = ent.lat,
          lon = ent.lon,
          street = ent.address.street,
          city = ent.address.city,
          state = ent.address.state,
          country = ent.address.country,
          building = ent.address.building,
          areaCode = ent.address.areaCode,
          area = ent.address.area
        }

    transformFareBreakup :: DRR.FareBreakup -> Common.FareBreakup
    transformFareBreakup DRR.FareBreakup {..} = do
      Common.FareBreakup
        { entityType = mkEntityType entityType,
          ..
        }

    mkEntityType :: DRR.FareBreakupEntityType -> Common.FareBreakupEntityType
    mkEntityType = \case
      DRR.BOOKING_UPDATE_REQUEST -> Common.BOOKING_UPDATE_REQUEST
      DRR.BOOKING -> Common.BOOKING
      DRR.RIDE -> Common.RIDE
      DRR.INITIAL_BOOKING -> Common.INITIAL_BOOKING

    castVehicleVariant :: DRR.Variant -> Common.Variant
    castVehicleVariant = \case
      DRR.SEDAN -> Common.SEDAN
      DRR.SUV -> Common.SUV
      DRR.SUV_PLUS -> Common.SUV_PLUS
      DRR.HATCHBACK -> Common.HATCHBACK
      DRR.AUTO_RICKSHAW -> Common.AUTO_RICKSHAW
      DRR.TAXI -> Common.TAXI
      DRR.TAXI_PLUS -> Common.TAXI_PLUS
      DRR.PREMIUM_SEDAN -> Common.PREMIUM_SEDAN
      DRR.BLACK -> Common.BLACK
      DRR.BLACK_XL -> Common.BLACK_XL
      DRR.BIKE -> Common.BIKE
      DRR.AMBULANCE_TAXI -> Common.AMBULANCE_TAXI
      DRR.AMBULANCE_TAXI_OXY -> Common.AMBULANCE_TAXI_OXY
      DRR.AMBULANCE_AC -> Common.AMBULANCE_AC
      DRR.AMBULANCE_AC_OXY -> Common.AMBULANCE_AC_OXY
      DRR.AMBULANCE_VENTILATOR -> Common.AMBULANCE_VENTILATOR

    makeRideInfoCacheKey :: Text
    makeRideInfoCacheKey = "CachedQueries:RideInfo:RideId-" <> show rideId.getId

    cacheRideInfo :: CacheFlow m r => DRR.RideInfoRes -> m ()
    cacheRideInfo rideInfoRes = do
      let shouldCacheRideInfo = elem (rideInfoRes.rideStatus) [DRPR.COMPLETED, DRPR.CANCELLED]
      bool (return ()) (Redis.setExp makeRideInfoCacheKey rideInfoRes 259200) shouldCacheRideInfo

castCreateTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.CreateTicketReq -> m TIT.CreateTicketResp
castCreateTicket merchantId merchantOperatingCityId = TT.createTicket (cast merchantId) (cast merchantOperatingCityId)

castUpdateTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.UpdateTicketReq -> m TIT.UpdateTicketResp
castUpdateTicket merchantId merchantOperatingCityId = TT.updateTicket (cast merchantId) (cast merchantOperatingCityId)

reportACIssue :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => BaseUrl -> Text -> Text -> m APISuccess
reportACIssue driverOfferBaseUrl driverOfferApiKey bppRideId = do
  void $ CallBPPInternal.reportACIssue driverOfferApiKey driverOfferBaseUrl bppRideId
  return Success

reportIssue :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => BaseUrl -> Text -> Text -> Common.IssueReportType -> m APISuccess
reportIssue driverOfferBaseUrl driverOfferApiKey bppRideId issueReportType = do
  void $ CallBPPInternal.reportIssue driverOfferApiKey driverOfferBaseUrl bppRideId issueReportType
  return Success

buildMerchantConfig :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> Maybe (Id Common.Person) -> m MerchantConfig
buildMerchantConfig merchantId merchantOpCityId _mbPersonId = do
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  riderConfig <- CQRC.findByMerchantOperatingCityId (cast merchantOpCityId) >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  return
    MerchantConfig
      { mediaFileSizeUpperLimit = merchant.mediaFileSizeUpperLimit,
        mediaFileUrlPattern = merchant.mediaFileUrlPattern,
        kaptureDisposition = merchant.kaptureDisposition,
        kaptureQueue = riderConfig.kaptureQueue,
        counterPartyUrl = merchant.driverOfferBaseUrl,
        counterPartyApiKey = merchant.driverOfferApiKey
      }

issueReportCustomerList :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportCustomerList (personId, merchantId) language = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Common.issueReportList (cast personId, cast merchantId, cast person.merchantOperatingCityId) language customerIssueHandle CUSTOMER

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (personId, merchantId) = withFlowHandlerAPI . Common.fetchMedia (cast personId, cast merchantId)

createIssueReport :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (personId, merchantId) mbLanguage req = withFlowHandlerAPI $ Common.createIssueReport (cast personId, cast merchantId) mbLanguage req customerIssueHandle CUSTOMER

issueMediaUpload :: (Id SP.Person, Id DM.Merchant) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (personId, merchantId) req = withFlowHandlerAPI $ Common.issueMediaUpload (cast personId, cast merchantId) customerIssueHandle req

issueInfo :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo (personId, merchantId) issueReportId language = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Common.issueInfo issueReportId (cast personId, cast merchantId, cast person.merchantOperatingCityId) language customerIssueHandle CUSTOMER

updateIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption (personId, merchantId) issueReportId req = withFlowHandlerAPI $ Common.updateIssueOption issueReportId (cast personId, cast merchantId) req CUSTOMER

deleteIssue :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> FlowHandler APISuccess
deleteIssue (personId, merchantId) issueReportId = withFlowHandlerAPI $ Common.deleteIssue issueReportId (cast personId, cast merchantId) CUSTOMER

getIssueCategory :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (personId, merchantId) language = withFlowHandlerAPI $ do
  personCityInfo <- CQPerson.findCityInfoById personId >>= fromMaybeM (PersonCityInformationNotFound personId.getId)
  Common.getIssueCategory (cast personId, cast merchantId, cast personCityInfo.merchantOperatingCityId) language customerIssueHandle CUSTOMER

getIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueCategory -> Maybe (Id Domain.IssueOption) -> Maybe (Id Domain.IssueReport) -> Maybe (Id Common.Ride) -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (personId, merchantId) issueCategoryId issueOptionId issueReportId mbRideId language = withFlowHandlerAPI $ do
  personCityInfo <- CQPerson.findCityInfoById personId >>= fromMaybeM (PersonCityInformationNotFound personId.getId)
  Common.getIssueOption (cast personId, cast merchantId, cast personCityInfo.merchantOperatingCityId) issueCategoryId issueOptionId issueReportId mbRideId language customerIssueHandle CUSTOMER

updateIssueStatus :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> Common.IssueStatusUpdateReq -> FlowHandler Common.IssueStatusUpdateRes
updateIssueStatus (personId, merchantId) issueReportId language req = withFlowHandlerAPI $ do
  personCityInfo <- CQPerson.findCityInfoById personId >>= fromMaybeM (PersonCityInformationNotFound personId.getId)
  Common.updateIssueStatus (cast personId, cast merchantId, cast personCityInfo.merchantOperatingCityId) issueReportId language req customerIssueHandle CUSTOMER
