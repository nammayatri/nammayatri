{-# LANGUAGE AllowAmbiguousTypes #-}

module API.UI.Issue where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Ride as DRR
import qualified Beckn.ACL.IGM.Issue as ACL
import qualified Beckn.ACL.IGM.IssueStatus as ACL
import Beckn.ACL.IGM.Utils
import qualified BecknV2.FRFS.Enums
import qualified BecknV2.OnDemand.Enums as OnDemandSpec
import qualified Data.Set as Set
import qualified Domain.Action.Dashboard.Ride as DRide
import Domain.Action.UI.IGM
import qualified Domain.Action.UI.Sos as Sos
import Domain.Types.Booking
import Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DR
import Environment
import EulerHS.Prelude hiding (elem, forM_, id)
import qualified IGM.Enums as Spec
import IssueManagement.API.UI.Issue as IA
import IssueManagement.Common.UI.Issue hiding (Booking, FRFSTicketBooking, Ride)
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueOption as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueCategory as QIC
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueOption as QIO
import qualified IssueManagement.Storage.Queries.Issue.IGMConfig as QIGMConfig
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
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
import Servant hiding (throwError)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.CallIGMBPP as CallBPP
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Person as CQPerson
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingExtra as QBE
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.RideExtra as QRE
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
        :<|> igmIssueStatus (personId, merchantId)

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
      kaptureGetTicket = Just castKaptureGetTicket,
      findMerchantConfig = buildMerchantConfig,
      mbReportACIssue = Just reportACIssue,
      mbReportIssue = Just reportIssue,
      mbFindLatestBookingByPersonId = Just findLatestBookingByRiderId,
      mbFindRideByBookingId = Just findRideByBookingId,
      mbSyncRide = Just syncRide,
      findByBookingId = castBookingById,
      findOneByBookingId = castRideByBookingId,
      findByMerchantId = castMerchantById,
      mbSendUnattendedTicketAlert = Just Sos.sendUnattendedSosTicketAlert,
      findRideByRideShortId = castRideByRideShortId,
      findByMobileNumberAndMerchantId = castPersonByMobileNumberAndMerchant,
      mbFindFRFSTicketBookingById = Just castFindFRFSTicketBookingById,
      mbFindStationByIdWithContext = Just castFindStationByIdWithContext
    }

castFindFRFSTicketBookingById :: Id Common.FRFSTicketBooking -> Flow (Maybe Common.FRFSTicketBooking)
castFindFRFSTicketBookingById ticketBookingId = do
  frfsTicketBooking <- runInReplica $ QFTB.findById (cast ticketBookingId)
  mapM
    ( \booking -> do
        quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
        let fareParameters = calculateFareParametersWithBookingFallback (mkCategoryPriceItemFromQuoteCategories quoteCategories) booking
        return $ castFRFSTicketBooking fareParameters booking
    )
    frfsTicketBooking
  where
    castFRFSTicketBooking fareParameters booking =
      Common.FRFSTicketBooking
        { id = cast booking.id,
          merchantOperatingCityId = cast booking.merchantOperatingCityId,
          merchantId = cast booking.merchantId,
          riderId = cast booking.riderId,
          fromStationCode = booking.fromStationCode,
          toStationCode = booking.toStationCode,
          vehicleType = castVehicleType booking.vehicleType,
          price = booking.totalPrice,
          quantity = fareParameters.totalQuantity,
          status = castFRFSStatus booking.status,
          createdAt = booking.createdAt,
          updatedAt = booking.updatedAt,
          stationsJson = booking.stationsJson
        }

    castVehicleType :: BecknV2.FRFS.Enums.VehicleCategory -> Common.VehicleCategory
    castVehicleType = \case
      BecknV2.FRFS.Enums.METRO -> Common.METRO
      BecknV2.FRFS.Enums.BUS -> Common.BUS
      _ -> Common.METRO

    castFRFSStatus :: DFRFSTicketBooking.FRFSTicketBookingStatus -> Common.FRFSTicketBookingStatus
    castFRFSStatus = \case
      DFRFSTicketBooking.NEW -> Common.FRFS_NEW
      DFRFSTicketBooking.CONFIRMED -> Common.FRFS_CONFIRMED
      DFRFSTicketBooking.CANCELLED -> Common.FRFS_CANCELLED
      _ -> Common.FRFS_NEW

castFindStationByIdWithContext :: Id Common.MerchantOperatingCity -> Common.VehicleCategory -> Text -> Flow (Maybe Common.Station)
castFindStationByIdWithContext merchantOpCityId vehicleType stationId = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing (cast merchantOpCityId) (castVehicleCategoryToOnDemand vehicleType) DIBC.APPLICATION
  mbStation <- OTPRest.getStationByGtfsIdAndStopCode stationId integratedBPPConfig

  case mbStation of
    Just station ->
      return $
        Just $
          Common.Station
            { code = station.code,
              name = station.name,
              lat = station.lat,
              lon = station.lon
            }
    Nothing -> return Nothing
  where
    castVehicleCategoryToOnDemand :: Common.VehicleCategory -> OnDemandSpec.VehicleCategory
    castVehicleCategoryToOnDemand = \case
      Common.METRO -> OnDemandSpec.METRO
      Common.BUS -> OnDemandSpec.BUS
      Common.SUBWAY -> OnDemandSpec.SUBWAY

castBookingById :: Id Common.Booking -> Flow (Maybe Common.Booking)
castBookingById bookingId = do
  booking <- runInReplica $ QB.findById (cast bookingId)
  return $ fmap castBooking booking
  where
    castBooking booking =
      Common.Booking
        { id = cast booking.id,
          bapId = Nothing,
          bapUri = Nothing,
          bppId = Just booking.providerId,
          bppUri = Just booking.providerUrl,
          quoteId = Nothing,
          providerId = cast $ Id booking.providerId,
          merchantOperatingCityId = cast booking.merchantOperatingCityId
        }

castRideByBookingId :: Id Common.Booking -> Id Common.Merchant -> Flow (Maybe Common.Ride)
castRideByBookingId bookingId merchantId = do
  ride <- runInReplica $ QR.findOneByBookingId (cast bookingId)
  merchantOpCityId <- case (.merchantOperatingCityId) =<< ride of
    Just moCityId -> return moCityId
    Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (fromMaybe (cast merchantId) ((.merchantId) =<< ride))
  return $ fmap (castRide merchantOpCityId) ride
  where
    castRide moCityId ride = Common.Ride (cast ride.id) (ShortId ride.shortId.getShortId) (cast moCityId) ride.createdAt (Just ride.bppRideId.getId) (maybe merchantId cast ride.merchantId) Nothing

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

castPersonById :: Id Common.Person -> Flow (Maybe Common.Person)
castPersonById personId = do
  person <- runInReplica $ QP.findById (cast personId)
  return $ mkPerson <$> person

castPersonByMobileNumberAndMerchant :: Text -> DbHash -> Id Common.Merchant -> Flow (Maybe Common.Person)
castPersonByMobileNumberAndMerchant mobileCountryCode numHash merchantId = do
  mbPerson <- runInReplica $ QP.findByMobileNumberAndMerchantId mobileCountryCode numHash (cast merchantId)
  return $ mkPerson <$> mbPerson

mkPerson :: SP.Person -> Common.Person
mkPerson person =
  Common.Person
    { id = cast person.id,
      language = person.language,
      firstName = person.firstName,
      lastName = person.lastName,
      middleName = person.middleName,
      mobileNumber = person.mobileNumber,
      merchantOperatingCityId = cast person.merchantOperatingCityId,
      blocked = Just person.blocked,
      merchantId = cast person.merchantId
    }

castRideById :: Id Common.Ride -> Id Common.Merchant -> Flow (Maybe Common.Ride)
castRideById rideId merchantId = do
  mbRide <- runInReplica $ QR.findById (cast rideId)
  traverse (mkRide merchantId) mbRide

castRideByRideShortId :: Id Common.Merchant -> ShortId Common.Ride -> Flow (Maybe Common.Ride)
castRideByRideShortId merchantId (ShortId rideShortId) = do
  mbRide <- runInReplica $ QR.findRideByRideShortId (ShortId rideShortId)
  traverse (mkRide merchantId) mbRide

mkRide :: Id Common.Merchant -> DR.Ride -> Flow Common.Ride
mkRide merchantId ride = do
  merchantOpCityId <- case ride.merchantOperatingCityId of
    Just moCityId -> return moCityId
    Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (fromMaybe (cast merchantId) ride.merchantId)
  pure $
    Common.Ride
      { id = cast ride.id,
        shortId = ShortId ride.shortId.getShortId,
        merchantOperatingCityId = cast merchantOpCityId,
        counterPartyRideId = Just ride.bppRideId.getId,
        createdAt = ride.createdAt,
        merchantId = maybe merchantId cast ride.merchantId,
        driverId = Nothing
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
          vehicleVariant = Just res.vehicleVariant,
          vehicleServiceTierName = res.vehicleServiceTierName,
          actualFare = res.actualFare,
          bookingStatus = Nothing,
          merchantOperatingCityId = res.merchantOperatingCityId,
          estimatedDistance = res.estimatedDistance,
          chargeableDistance = res.chargeableDistance,
          estimatedFare = toHighPrecMoney res.estimatedFare,
          computedPrice = res.computedPrice,
          fareBreakup = transformFareBreakup <$> res.fareBreakup,
          rideCreatedAt = res.rideCreatedAt,
          rideStartTime = res.rideStartTime,
          rideStatus = castRideStatus res.rideStatus,
          mobileCountryCode = res.mobileCountryCode
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

    castRideStatus :: DRR.RideStatus -> Common.RideStatus
    castRideStatus = \case
      DRR.UPCOMING_RIDE -> Common.R_UPCOMING
      DRR.NEW -> Common.R_NEW
      DRR.INPROGRESS -> Common.R_INPROGRESS
      DRR.COMPLETED -> Common.R_COMPLETED
      DRR.CANCELLED -> Common.R_CANCELLED

    makeRideInfoCacheKey :: Text
    makeRideInfoCacheKey = "CachedQueries:RideInfo:RideId-" <> show rideId.getId

    cacheRideInfo :: DRR.RideInfoRes -> Flow ()
    cacheRideInfo rideInfoRes = do
      let shouldCacheRideInfo = elem (rideInfoRes.rideStatus) [DRR.COMPLETED, DRR.CANCELLED]
      bool (return ()) (Redis.setExp makeRideInfoCacheKey rideInfoRes 259200) shouldCacheRideInfo

castCreateTicket :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.CreateTicketReq -> Flow TIT.CreateTicketResp
castCreateTicket merchantId merchantOperatingCityId = TT.createTicket (cast merchantId) (cast merchantOperatingCityId)

castUpdateTicket :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.UpdateTicketReq -> Flow TIT.UpdateTicketResp
castUpdateTicket merchantId merchantOperatingCityId = TT.updateTicket (cast merchantId) (cast merchantOperatingCityId)

castKaptureGetTicket :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.GetTicketReq -> Flow [TIT.GetTicketResp]
castKaptureGetTicket merchantId merchantOperatingCityId = TT.kaptureGetTicket (cast merchantId) (cast merchantOperatingCityId)

reportACIssue :: BaseUrl -> Text -> Text -> Flow APISuccess
reportACIssue driverOfferBaseUrl driverOfferApiKey bppRideId = do
  void $ CallBPPInternal.reportACIssue driverOfferApiKey driverOfferBaseUrl bppRideId
  return Success

reportIssue :: BaseUrl -> Text -> Text -> Common.IssueReportType -> Flow APISuccess
reportIssue driverOfferBaseUrl driverOfferApiKey bppRideId issueReportType = do
  void $ CallBPPInternal.reportIssue driverOfferApiKey driverOfferBaseUrl bppRideId issueReportType
  return Success

buildMerchantConfig :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> Maybe (Id Common.Person) -> Flow MerchantConfig
buildMerchantConfig merchantId merchantOpCityId _mbPersonId = do
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId (cast merchantOpCityId) Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  return
    MerchantConfig
      { mediaFileSizeUpperLimit = merchant.mediaFileSizeUpperLimit,
        mediaFileUrlPattern = merchant.mediaFileUrlPattern,
        dashboardMediaFileUrlPattern = riderConfig.dashboardMediaFileUrlPattern,
        kaptureDisposition = riderConfig.kaptureConfig.disposition,
        kaptureQueue = riderConfig.kaptureConfig.queue,
        counterPartyUrl = merchant.driverOfferBaseUrl,
        counterPartyApiKey = merchant.driverOfferApiKey,
        sensitiveWords = riderConfig.sensitiveWords,
        sensitiveWordsForExactMatch = riderConfig.sensitiveWordsForExactMatch
      }

findLatestBookingByRiderId :: Id Common.Person -> Flow (Maybe Common.Booking)
findLatestBookingByRiderId personId = do
  mbLatestBooking <- QBE.findLatestSelfAndPartyBookingByRiderId (cast personId)
  return $ castBooking <$> mbLatestBooking
  where
    castBooking booking =
      Common.Booking
        { id = cast booking.id,
          bapId = Nothing,
          bapUri = Nothing,
          bppId = Just booking.providerId,
          bppUri = Just booking.providerUrl,
          quoteId = Nothing,
          providerId = cast $ Id booking.providerId,
          merchantOperatingCityId = cast booking.merchantOperatingCityId
        }

findRideByBookingId :: Id Common.Booking -> Id Common.Merchant -> Flow (Maybe Common.Ride)
findRideByBookingId bookingId merchantId = do
  mbRide <- QRE.findActiveByRBId (cast bookingId)
  traverse (mkRide merchantId) mbRide

syncRide :: Id Common.Merchant -> Id Common.Ride -> Flow ()
syncRide merchantId rideId = do
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  void $ DRide.rideSync merchant (cast rideId)

issueReportCustomerList :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportCustomerList (personId, merchantId) language = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Common.issueReportList (cast personId, cast merchantId, cast person.merchantOperatingCityId) language customerIssueHandle CUSTOMER

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (personId, merchantId) = withFlowHandlerAPI . Common.fetchMedia (cast personId, cast merchantId)

createIssueReport :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (personId, merchantId) mbLanguage req = withFlowHandlerAPI $ do
  when (isJust req.ticketBookingId && isJust req.rideId) $
    throwError $ InvalidRequest "Only one issue can be raised at a time."

  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  mbIGMReq <- case (req.rideId, req.ticketBookingId) of
    (Just rideId, _) -> buildOnDemandIGMIssueReq rideId
    (_, Just ticketBookingId) -> buildFRFSIGMIssueReq ticketBookingId
    _ -> pure Nothing

  becknIssueId <-
    if riderConfig.enableIGMIssueFlow
      then case mbIGMReq of
        Just (OnDemandIGMIssueReq onDemandReq) ->
          if not (onDemandReq.isValueAddNP)
            then processIssueRequest (OnDemandIGMIssueReq onDemandReq) person req
            else return Nothing
        Just (FRFSIGMIssueReq frfsReq) -> processIssueRequest (FRFSIGMIssueReq frfsReq) person req
        Nothing -> return Nothing
      else return Nothing

  Common.createIssueReport (cast personId, cast merchantId) mbLanguage req customerIssueHandle CUSTOMER becknIssueId
  where
    processIssueRequest igmReq person reqBody = do
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      igmConfig <- QIGMConfig.findByMerchantId (cast merchantId) >>= fromMaybeM (InternalError $ "IGMConfig not found " <> show merchantId)
      category <- QIC.findById reqBody.categoryId CUSTOMER >>= fromMaybeM (InvalidRequest "Issue Category not found")
      option <- maybe (return Nothing) (`QIO.findById` CUSTOMER) req.optionId
      validateSubcategory option igmReq

      case igmReq of
        FRFSIGMIssueReq frfsReq -> processTicketBookingIssue frfsReq.ticketBooking category option merchant person igmConfig reqBody
        OnDemandIGMIssueReq onDemandReq -> processBookingIssue onDemandReq.booking category option merchant person igmConfig reqBody

    validateSubcategory mbOption igmReq =
      case mbOption of
        Just opt ->
          case opt.igmSubCategory of
            Just subcatCode ->
              if isValidSubcategory subcatCode igmReq
                then pure ()
                else do
                  case igmReq of
                    FRFSIGMIssueReq _ -> throwError $ InternalError "Invalid Metro SubCategory for the given issue option"
                    OnDemandIGMIssueReq _ -> throwError $ InternalError "Invalid OnDemand SubCategory for the given issue option"
            Nothing -> throwError $ InternalError "SubCategory is empty"
        Nothing -> pure ()
    isValidSubcategory subcatCode = \case
      FRFSIGMIssueReq _ -> Set.member subcatCode Spec.metroSubcategories
      OnDemandIGMIssueReq _ -> Set.member subcatCode Spec.onDemandSubcategories

    processBookingIssue booking category option merchant person igmConfig reqBody = do
      merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show booking.merchantOperatingCityId)
      (becknIssueReq, issueId, igmIssue) <- ACL.buildIssueReq (fromBooking booking) category option reqBody.description merchant person igmConfig merchantOperatingCity Nothing Nothing Nothing
      QIGM.create igmIssue
      fork "sending beckn issue" . withShortRetry $ do
        void $ CallBPP.issue booking.providerUrl becknIssueReq
      pure $ Just issueId

    processTicketBookingIssue ticketBooking category option merchant person igmConfig reqBody = do
      quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId ticketBooking.quoteId
      let fareParameters = calculateFareParametersWithBookingFallback (mkCategoryPriceItemFromQuoteCategories quoteCategories) ticketBooking
      frfsTicketBookingDetails <- fromFRFSTicketBooking ticketBooking fareParameters
      merchantOperatingCity <- CQMOC.findById frfsTicketBookingDetails.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show frfsTicketBookingDetails.merchantOperatingCityId)
      (becknIssueReq, issueId, igmIssue) <- ACL.buildIssueReq frfsTicketBookingDetails category option reqBody.description merchant person igmConfig merchantOperatingCity Nothing Nothing Nothing
      QIGM.create igmIssue
      providerUrl <- parseBaseUrl ticketBooking.bppSubscriberUrl
      logDebug $ "Sending beckn issue for ticket booking" <> show becknIssueReq
      fork "sending beckn issue" . withShortRetry $ do
        void $ CallBPP.issue providerUrl becknIssueReq
      pure $ Just issueId

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
  case req.status of
    CLOSED -> resolveIGMIssue (personId, merchantId) issueReportId req.customerResponse req.customerRating
    RESOLVED -> resolveIGMIssue (personId, merchantId) issueReportId req.customerResponse req.customerRating
    _ -> pure ()
  Common.updateIssueStatus (cast personId, cast merchantId, cast personCityInfo.merchantOperatingCityId) issueReportId language req customerIssueHandle CUSTOMER

igmIssueStatus :: (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
igmIssueStatus (personId, merchantId) = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  issues <- QIGM.findAllByStatus DIGM.OPEN

  forM_ issues $ \issue -> do
    let mopCid = fromMaybe person.merchantOperatingCityId (cast <$> issue.merchantOperatingCityId)
    merchantOperatingCity <- CQMOC.findById mopCid >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show issue.merchantOperatingCityId)

    (bookingDetails, providerUrl) <-
      if issue.domain == Spec.ON_DEMAND
        then QB.findById (Id issue.bookingId) >>= fromMaybeM (BookingNotFound issue.bookingId) >>= \b -> pure (fromBooking b, b.providerUrl)
        else
          QFTB.findById (Id issue.bookingId) >>= fromMaybeM (TicketBookingNotFound issue.bookingId) >>= \tb -> do
            quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId tb.quoteId
            let fareParameters = calculateFareParametersWithBookingFallback (mkCategoryPriceItemFromQuoteCategories quoteCategories) tb
            liftA2 (,) (fromFRFSTicketBooking tb fareParameters) (parseBaseUrl tb.bppSubscriberUrl)

    becknIssueStatusReq <- ACL.buildIssueStatusReq merchant merchantOperatingCity bookingDetails issue.id.getId issue.transactionId
    fork "sending beckn issue_status" . withShortRetry $ void $ CallBPP.issueStatus providerUrl becknIssueStatusReq

  pure Success

resolveIGMIssue :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe CustomerResponse -> Maybe Common.CustomerRating -> Flow ()
resolveIGMIssue (personId, merchantId) issueReportId response rating = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (InternalError $ "Issue Report not found " <> show issueReportId.getId)
  becknIssueId <- maybe (throwError $ InvalidRequest "IGM Issue Id not found") return issueReport.becknIssueId
  mbIGMIssue <- QIGM.findByPrimaryKey (Id becknIssueId)
  maybe (throwError $ InvalidRequest "IGM Issue not found") (\igmIssue -> processIGMIssue igmIssue issueReport person) mbIGMIssue
  pure ()
  where
    processIGMIssue igmIssue issueReport person = do
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      igmConfig <- QIGMConfig.findByMerchantId (cast merchantId) >>= fromMaybeM (InternalError $ "IGMConfig not found " <> show merchantId)
      let mopCid = fromMaybe person.merchantOperatingCityId (cast <$> igmIssue.merchantOperatingCityId)
      merchantOperatingCity <- CQMOC.findById mopCid >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show igmIssue.merchantOperatingCityId)
      rideBooking <- case igmIssue.domain of
        Spec.ON_DEMAND -> do
          booking <- QB.findById (Id igmIssue.bookingId) >>= fromMaybeM (BookingNotFound igmIssue.bookingId)
          return $ fromBooking booking
        Spec.PUBLIC_TRANSPORT -> do
          frfsBooking <- QFTB.findById (Id igmIssue.bookingId) >>= fromMaybeM (FRFSTicketBookingNotFound igmIssue.bookingId)
          quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId frfsBooking.quoteId
          let fareParameters = calculateFareParametersWithBookingFallback (mkCategoryPriceItemFromQuoteCategories quoteCategories) frfsBooking
          fromFRFSTicketBooking frfsBooking fareParameters
      option <- maybe (return Nothing) (`QIO.findById` CUSTOMER) issueReport.optionId
      category <- case issueReport.categoryId of
        Nothing -> throwError $ InvalidRequest "Issue Category not found"
        Just catId -> QIC.findById catId CUSTOMER >>= fromMaybeM (InvalidRequest "Issue Category not found")
      (becknIssueReq, _, updatedIgmIssue) <- ACL.buildIssueReq rideBooking category option issueReport.description merchant person igmConfig merchantOperatingCity response rating (Just igmIssue)
      QIGM.updateByPrimaryKey updatedIgmIssue
      fork "sending beckn issue" . withShortRetry $ do
        void $ CallBPP.issue rideBooking.providerUrl becknIssueReq

fromBooking :: Booking -> RideBooking
fromBooking b = do
  RideBooking
    { bookingId = b.id.getId,
      providerId = b.providerId,
      providerUrl = b.providerUrl,
      merchantOperatingCityId = b.merchantOperatingCityId,
      merchantId = b.merchantId,
      bppBookingId = b.bppBookingId <&> getId,
      status = Nothing,
      bppItemId = b.bppEstimateId,
      contactPhone = Just $ b.primaryExophone,
      domain = Spec.ON_DEMAND,
      quantity = Nothing,
      bppOrderId = Nothing
    }

fromFRFSTicketBooking :: (MonadFlow m) => FRFSTicketBooking -> FRFSFareParameters -> m RideBooking
fromFRFSTicketBooking b fareParameters = do
  providerUrl <- parseBaseUrl b.bppSubscriberUrl
  pure $
    RideBooking
      { bookingId = b.id.getId,
        providerId = b.providerId,
        providerUrl = providerUrl,
        merchantOperatingCityId = b.merchantOperatingCityId,
        merchantId = b.merchantId,
        bppBookingId = Just $ getId b.searchId,
        status = Just $ show b.status,
        bppItemId = b.bppItemId,
        contactPhone = Nothing,
        domain = Spec.PUBLIC_TRANSPORT,
        quantity = Just fareParameters.totalQuantity,
        bppOrderId = b.bppOrderId
      }
