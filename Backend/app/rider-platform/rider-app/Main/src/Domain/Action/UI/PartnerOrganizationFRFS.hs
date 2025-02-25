{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.PartnerOrganizationFRFS
  ( upsertPersonAndGetToken,
    getFareV2,
    getConfigByStationIds,
    shareTicketInfo,
    mkLatLong,
    upsertPersonAndQuoteConfirm,
    GetFareReq (..),
    GetFareResp (..),
    GetConfigResp (..),
    ShareTicketInfoResp (..),
    UpsertPersonResp (..),
    GetFareReqV2 (..),
    UpsertPersonAndQuoteConfirmReq (..),
    UpsertPersonAndQuoteConfirmRes (..),
    GetFareRespV2 (..),
    UpsertPersonAndQuoteConfirmResBody (..),
    QuoteConfirmStatus (..),
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Data.OpenApi hiding (email, info, name)
import qualified Data.Text as T
import qualified Domain.Action.UI.FRFSTicketService as DFRFSTicketService
import qualified Domain.Action.UI.Registration as DReg
import Domain.Types.Extra.FRFSCachedQuote as CachedQuote
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Domain.Types.PartnerOrganization
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Domain.Types.Station
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, map, null, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude hiding (sequenceA_)
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common as Kernel
import Kernel.Utils.JSON (removeNullFields)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.FRFSUtils as Utils
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.CachedQueries.Station as CQS
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicket as QFT
import qualified Storage.Queries.FRFSTicketBokingPayment as QFTBP
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.PersonStats as QPStats
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Storage.Queries.Route as QRoute
import Tools.Error

data GetFareReq = GetFareReq
  { fromStationCode :: Text,
    toStationCode :: Text,
    routeCode :: Maybe Text,
    numberOfPassengers :: Int,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    identifierType :: SP.IdentifierType,
    partnerOrgTransactionId :: Maybe (Id PartnerOrgTransaction),
    cityId :: Id DMOC.MerchantOperatingCity,
    vehicleType :: Maybe Spec.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data GetFareReqV2 = GetFareReqV2
  { fromStationCode :: Text,
    toStationCode :: Text,
    partnerOrgTransactionId :: Maybe (Id PartnerOrgTransaction),
    routeCode :: Maybe Text,
    cityId :: Id DMOC.MerchantOperatingCity,
    vehicleType :: Maybe Spec.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

validateGetFareReq :: Validate GetFareReq
validateGetFareReq GetFareReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

data GetFareResp = GetFareResp
  { searchId :: Id DFRFSSearch.FRFSSearch,
    personId :: Id SP.Person,
    token :: RegToken
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data GetFareRespV2 = GetFareRespV2
  { searchId :: Id DFRFSSearch.FRFSSearch,
    quotes :: Maybe [FRFSTypes.FRFSQuoteAPIRes]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON GetFareRespV2 where
  toJSON = genericToJSON removeNullFields

data UpsertPersonResp = UpsertPersonResp
  { personId :: Id SP.Person,
    token :: RegToken
  }

data UpsertPersonAndQuoteConfirmReq = UpsertPersonAndQuoteConfirmReq
  { searchId :: Id DFRFSSearch.FRFSSearch,
    quoteId :: Id DFRFSQuote.FRFSQuote,
    numberOfPassengers :: Int,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    identifierType :: SP.IdentifierType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteConfirmStatus = ON_SEARCH_NOT_RECEIVED_YET | INIT_TRIGGERED deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpsertPersonAndQuoteConfirmRes = UpsertPersonAndQuoteConfirmRes
  { quoteConfirmStatus :: QuoteConfirmStatus,
    body :: Maybe UpsertPersonAndQuoteConfirmResBody
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpsertPersonAndQuoteConfirmResBody = UpsertPersonAndQuoteConfirmResBody
  { token :: RegToken,
    bookingInfo :: FRFSTypes.FRFSTicketBookingStatusAPIRes
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data GetConfigResp = GetConfigResp
  { frfsConfig :: FRFSTypes.FRFSConfigAPIRes,
    fromStation :: FRFSTypes.FRFSStationAPI,
    toStation :: FRFSTypes.FRFSStationAPI,
    city :: Context.City,
    cityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ShareTicketInfoResp = ShareTicketInfoResp
  { tickets :: [FRFSTypes.FRFSTicketAPI],
    city :: Context.City,
    returnType :: DFRFSQuote.FRFSQuoteType,
    fromStation :: FRFSTypes.FRFSStationAPI,
    toStation :: FRFSTypes.FRFSStationAPI,
    bookingPrice :: HighPrecMoney,
    paymentStatus :: FRFSTypes.FRFSBookingPaymentStatusAPI,
    partnerOrgTransactionId :: Maybe (Id PartnerOrgTransaction),
    googleWalletJWTUrl :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

upsertPersonAndGetToken ::
  Id PartnerOrganization ->
  DPOC.RegistrationConfig ->
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  Maybe Maps.LatLong ->
  GetFareReq ->
  Flow (Id SP.Person, RegToken)
upsertPersonAndGetToken pOrgId regPOCfg fromStationMOCId mId mbRegCoordinates req@GetFareReq {..} = do
  runRequestValidation validateGetFareReq req
  merchant <- B.runInReplica $ CQM.findById mId >>= fromMaybeM (MerchantNotFound mId.getId)

  (person, isCreatedNow) <-
    case identifierType of
      SP.MOBILENUMBER -> do
        mobileNumberHash <- getDbHash mobileNumber
        Person.findByRoleAndMobileNumberAndMerchantId SP.USER mobileCountryCode mobileNumberHash mId
          >>= maybe (createPersonViaPartner req merchant mbRegCoordinates pOrgId) (return . (,False))
      _ -> throwError . InvalidRequest $ "Unsupported identifier type" +|| identifierType ||+ ""

  when (isCreatedNow && person.merchantOperatingCityId /= fromStationMOCId) $ do
    moc <- B.runInReplica $ CQMOC.findById fromStationMOCId >>= fromMaybeM (MerchantOperatingCityNotFound fromStationMOCId.getId)
    CQP.updateCityInfoById person.id moc.city moc.id

  regToken <- getRegToken person.id pOrgId regPOCfg mId

  return (person.id, regToken.token)

getRegToken :: Id SP.Person -> Id PartnerOrganization -> DPOC.RegistrationConfig -> Id DM.Merchant -> Flow SR.RegistrationToken
getRegToken personId pOrgId regPOCfg mId = do
  let entityId = personId
  RegistrationToken.findAllByPersonId entityId
    <&> listToMaybe . sortOn (.updatedAt) . filter (isJust . (.createdViaPartnerOrgId))
    >>= validateToken pOrgId
    >>= maybe (makeSessionViaPartner regPOCfg.sessionConfig entityId.getId mId.getId regPOCfg.fakeOtp pOrgId) return

validateToken :: Id PartnerOrganization -> Maybe SR.RegistrationToken -> Flow (Maybe SR.RegistrationToken)
validateToken pOrgId = \case
  Nothing -> pure Nothing
  Just regToken -> do
    let nominal = realToFrac $ regToken.tokenExpiry * 24 * 60 * 60
    expired <- Kernel.isExpired nominal regToken.updatedAt
    let res = bool Nothing (Just regToken) $ regToken.verified && not expired && regToken.createdViaPartnerOrgId == Just pOrgId
    when (isNothing res && regToken.createdViaPartnerOrgId == Just pOrgId) $ do
      RegistrationToken.deleteByTokenCreatedViaPartnerOrgId regToken.token pOrgId
    pure res

createPersonViaPartner ::
  GetFareReq ->
  DM.Merchant ->
  Maybe Maps.LatLong ->
  Id PartnerOrganization ->
  Flow (SP.Person, Bool)
createPersonViaPartner req merchant mbRegCoordinates partnerOrgId = do
  let identifierType = req.identifierType
      notificationToken = Nothing
      mbBundleVersion = Nothing
      mbClientVersion = Nothing
      mbRnVersion = Nothing
      mbClientConfigVersion = Nothing
      mbDevice = Nothing
      authReq = buildPartnerAuthReq identifierType notificationToken
  person <- DReg.createPerson authReq identifierType notificationToken mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice merchant (Just partnerOrgId)
  return (person, True)
  where
    buildPartnerAuthReq identifierType notificationToken =
      DReg.AuthReq
        { mobileNumber = Just req.mobileNumber,
          mobileCountryCode = Just req.mobileCountryCode,
          identifierType = Just identifierType,
          merchantId = merchant.shortId,
          deviceToken = Nothing,
          notificationToken,
          whatsappNotificationEnroll = Nothing,
          firstName = Nothing,
          middleName = Nothing,
          lastName = Nothing,
          email = Nothing,
          language = Nothing,
          gender = Nothing,
          otpChannel = Nothing,
          registrationLat = mbRegCoordinates <&> (.lat),
          registrationLon = mbRegCoordinates <&> (.lon),
          enableOtpLessRide = Nothing,
          allowBlockedUserLogin = Nothing
        }

makeSessionViaPartner ::
  SmsSessionConfig ->
  Text ->
  Text ->
  Text ->
  Id PartnerOrganization ->
  Flow SR.RegistrationToken
makeSessionViaPartner sessionConfig entityId mId fakeOtp partnerOrgId = do
  let authMedium = SR.PARTNER_ORG
  regToken <- makeSession authMedium sessionConfig entityId mId (Just fakeOtp) partnerOrgId
  void $ RegistrationToken.create regToken
  void $ RegistrationToken.setDirectAuth regToken.id authMedium
  return regToken

makeSession ::
  SR.Medium ->
  SmsSessionConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Id PartnerOrganization ->
  Flow SR.RegistrationToken
makeSession authMedium SmsSessionConfig {..} entityId merchantId fakeOtp partnerOrgId = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = attempts,
        authMedium,
        authType = SR.DIRECT,
        authValueHash = otp,
        verified = False,
        authExpiry = authExpiry,
        tokenExpiry = tokenExpiry,
        entityId = entityId,
        merchantId = merchantId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        createdViaPartnerOrgId = Just partnerOrgId
      }

getConfigByStationIds :: PartnerOrganization -> Text -> Text -> Flow GetConfigResp
getConfigByStationIds partnerOrg fromGMMStationId toGMMStationId = do
  let isGMMStationId = T.isPrefixOf "Ch" fromGMMStationId && T.isPrefixOf "Ch" toGMMStationId
  (fromStation', toStation') <-
    if isGMMStationId
      then do
        let fromPOrgStationId = Id fromGMMStationId
        let toPOrgStationId = Id toGMMStationId
        fromStation' <- B.runInReplica $ CQPOS.findStationWithPOrgName partnerOrg.orgId fromPOrgStationId
        toStation' <- B.runInReplica $ CQPOS.findStationWithPOrgName partnerOrg.orgId toPOrgStationId
        return (fromStation', toStation')
      else do
        let fromStationId' = Id fromGMMStationId
        let toStationId' = Id toGMMStationId
        fromStation' <- B.runInReplica $ CQPOS.findStationWithPOrgIdAndStationId fromStationId' partnerOrg.orgId
        toStation' <- B.runInReplica $ CQPOS.findStationWithPOrgIdAndStationId toStationId' partnerOrg.orgId
        return (fromStation', toStation')
  frfsConfig' <- B.runInReplica $ CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow fromStation'.merchantOperatingCityId [] >>= fromMaybeM (FRFSConfigNotFound fromStation'.merchantOperatingCityId.getId)

  unless (frfsConfig'.merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId.getId ||+ " not valid for merchantId:" +|| frfsConfig'.merchantId.getId ||+ ""
  unless (fromStation'.merchantOperatingCityId == toStation'.merchantOperatingCityId) $
    throwError . InvalidRequest $ "origin:" +|| fromStation'.name ||+ "and destination:" +|| toStation'.name ||+ " locations are not of same city"

  fromStation <- Utils.mkPOrgStationAPIRes fromStation' (Just partnerOrg.orgId)
  toStation <- Utils.mkPOrgStationAPIRes toStation' (Just partnerOrg.orgId)
  let frfsConfig = Utils.mkFRFSConfigAPI frfsConfig'
  moc <- CQMOC.findById fromStation'.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound fromStation'.merchantOperatingCityId.getId)
  let city = moc.city
  let cityId = moc.id
  pure $ GetConfigResp {..}

shareTicketInfo :: Id DFTB.FRFSTicketBooking -> Flow ShareTicketInfoResp
shareTicketInfo ticketBookingId = do
  -- TODO: Make it findAllWithKVAndConditionalDB
  tickets' <- B.runInReplica $ QFT.findAllByTicketBookingId ticketBookingId

  when (null tickets') $
    throwError $ FRFSTicketsForBookingDoesNotExist ticketBookingId.getId

  let tickets = map Utils.mkTicketAPI tickets'

  ticketBooking <- B.runInReplica $ QFTB.findById ticketBookingId >>= fromMaybeM (FRFSTicketBookingNotFound ticketBookingId.getId)
  paymentBooking <- B.runInReplica $ QFTBP.findNewTBPByBookingId ticketBookingId >>= fromMaybeM (FRFSTicketBookingPaymentNotFound ticketBookingId.getId)
  fromStation' <- B.runInReplica $ CQS.findById ticketBooking.fromStationId >>= fromMaybeM (StationNotFound $ "StationId:" +|| ticketBooking.fromStationId.getId ||+ "")
  toStation' <- B.runInReplica $ CQS.findById ticketBooking.toStationId >>= fromMaybeM (StationNotFound $ "StationId:" +|| ticketBooking.toStationId.getId ||+ "")
  city <- CQMOC.findById fromStation'.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound fromStation'.merchantOperatingCityId.getId)
  pOrgId <- ticketBooking.partnerOrgId & fromMaybeM (InternalError $ "PartnerOrgId is missing for ticketBookingId:" +|| ticketBookingId.getId ||+ "")

  fromStation <- Utils.mkPOrgStationAPIRes fromStation' (Just pOrgId)
  toStation <- Utils.mkPOrgStationAPIRes toStation' (Just pOrgId)

  void $ bppStatusSync fromStation'.merchantId pOrgId city ticketBooking

  pure $
    ShareTicketInfoResp
      { returnType = ticketBooking._type,
        bookingPrice = ticketBooking.price.amount,
        paymentStatus = Utils.mkTBPStatusAPI paymentBooking.status,
        partnerOrgTransactionId = ticketBooking.partnerOrgTransactionId,
        city = city.city,
        googleWalletJWTUrl = ticketBooking.googleWalletJWTUrl,
        ..
      }
  where
    whenWithLockRedisWithoutUnlock :: (CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Text -> Int -> m () -> m ()
    whenWithLockRedisWithoutUnlock key expiry action = do
      lockAcquired <- Redis.tryLockRedis key expiry
      when lockAcquired action

    statusTriggerLockKey = "FRFSTicketBooking:BPP:Status:Trigger:BookingId-" <> ticketBookingId.getId

    bppStatusSync merchantId pOrgId city ticketBooking = do
      pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType pOrgId DPOC.BPP_STATUS_CALL >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.BPP_STATUS_CALL)
      bppStatusCallCfg <- DPOC.getBPPStatusCallConfig pOrgCfg.config

      whenWithLockRedisWithoutUnlock statusTriggerLockKey bppStatusCallCfg.intervalInSec $ do
        bapConfig <-
          B.runInReplica $
            CQBC.findByMerchantIdDomainAndVehicle merchantId (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType)
              >>= fromMaybeM (BecknConfigNotFound $ "MerchantId:" +|| merchantId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType ||+ "")
        void $ CallExternalBPP.status merchantId city bapConfig ticketBooking DIBC.PARTNERORG

getFareV2 :: PartnerOrganization -> Station -> Station -> Maybe (Id PartnerOrgTransaction) -> Maybe Text -> Flow GetFareRespV2
getFareV2 partnerOrg fromStation toStation partnerOrgTransactionId routeCode = do
  let merchantId = fromStation.merchantId
      frfsVehicleType = fromStation.vehicleType
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow fromStation.merchantOperatingCityId [] >>= fromMaybeM (FRFSConfigNotFound fromStation.merchantOperatingCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  bapConfig <-
    CQBC.findByMerchantIdDomainAndVehicle merchantId (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType)
      >>= fromMaybeM (BecknConfigNotFound $ "MerchantId:" +|| merchantId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType ||+ "")
  merchantOperatingCity <- CQMOC.findById fromStation.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> fromStation.merchantOperatingCityId.getId)
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType) DIBC.PARTNERORG >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType ||+ "Platform Type:" +|| DIBC.PARTNERORG ||+ "")
  route <-
    maybe
      (pure Nothing)
      ( \routeCode' -> do
          route' <- B.runInReplica $ QRoute.findByRouteCode routeCode' integratedBPPConfig.id >>= fromMaybeM (RouteNotFound routeCode')
          return $ Just route'
      )
      routeCode
  let frfsRouteDetails =
        [ FRFSRouteDetails
            { routeCode = routeCode,
              startStationCode = fromStation.code,
              endStationCode = toStation.code
            }
        ]
  searchReq <- mkSearchReq frfsVehicleType partnerOrgTransactionId partnerOrg fromStation toStation route
  fork ("FRFS Search: " <> searchReq.id.getId) $ do
    QSearch.create searchReq
    CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq frfsRouteDetails integratedBPPConfig
  quotes <- mkQuoteFromCache fromStation toStation frfsConfig partnerOrg partnerOrgTransactionId searchReq.id
  whenJust quotes $ \quotes' -> QQuote.createMany quotes'
  case quotes of
    Just quotes' -> do
      quoteRes <- mapM mkQuoteRes quotes'
      return $
        GetFareRespV2
          { searchId = searchReq.id,
            quotes = Just quoteRes
          }
    Nothing ->
      return $
        GetFareRespV2
          { searchId = searchReq.id,
            quotes = Nothing
          }
  where
    mkSearchReq frfsVehicleType partnerOrgTransactionId' partnerOrg' fromStation' toStation' route = do
      now <- getCurrentTime
      uid <- generateGUID
      return
        DFRFSSearch.FRFSSearch
          { id = uid,
            vehicleType = frfsVehicleType,
            merchantId = fromStation'.merchantId,
            merchantOperatingCityId = fromStation'.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            quantity = 1,
            fromStationId = fromStation'.id,
            toStationId = toStation'.id,
            routeId = route <&> (.id),
            riderId = Utils.partnerOrgRiderId,
            partnerOrgTransactionId = partnerOrgTransactionId',
            partnerOrgId = Just partnerOrg'.orgId,
            journeyLegInfo = Nothing,
            isOnSearchReceived = Nothing,
            journeyLegStatus = Nothing,
            journeyRouteDetails = [],
            ..
          }

mkLatLong :: Maybe Double -> Maybe Double -> Maybe Maps.LatLong
mkLatLong mbLat mbLon = case (mbLat, mbLon) of
  (Just lat, Just lon) -> Just $ Maps.LatLong {..}
  _ -> Nothing

mkQuoteRes :: (MonadFlow m) => DFRFSQuote.FRFSQuote -> m FRFSTypes.FRFSQuoteAPIRes
mkQuoteRes quote = do
  (stations :: [FRFSTypes.FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InvalidStationJson $ show quote.stationsJson)
  let routeStations :: Maybe [FRFSTypes.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
  let discounts :: Maybe [FRFSTypes.FRFSDiscountRes] = decodeFromText =<< quote.discountsJson
  return $
    FRFSTypes.FRFSQuoteAPIRes
      { quoteId = quote.id,
        _type = quote._type,
        price = quote.price.amount,
        priceWithCurrency = mkPriceAPIEntity quote.price,
        quantity = quote.quantity,
        validTill = quote.validTill,
        vehicleType = quote.vehicleType,
        discountedTickets = quote.discountedTickets,
        eventDiscountAmount = quote.eventDiscountAmount,
        ..
      }

upsertPersonAndQuoteConfirm :: PartnerOrganization -> UpsertPersonAndQuoteConfirmReq -> Flow UpsertPersonAndQuoteConfirmRes
upsertPersonAndQuoteConfirm partnerOrg req = do
  search <- QSearch.findById req.searchId >>= fromMaybeM (FRFSSearchNotFound req.searchId.getId)
  if isNothing search.isOnSearchReceived
    then do
      return
        UpsertPersonAndQuoteConfirmRes
          { quoteConfirmStatus = ON_SEARCH_NOT_RECEIVED_YET,
            body = Nothing
          }
    else do
      pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.REGISTRATION >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrg.orgId.getId $ show DPOC.REGISTRATION)
      regPOCfg <- DPOC.getRegistrationConfig pOrgCfg.config
      mbBooking <- QBooking.findByQuoteId req.quoteId
      case mbBooking of
        Just booking -> cretateBookingResIfBookingAlreadyCreated partnerOrg booking regPOCfg
        Nothing -> createNewBookingAndTriggerInit partnerOrg req regPOCfg

mkQuoteFromCache :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Station -> Station -> FRFSConfig -> PartnerOrganization -> Maybe (Id PartnerOrgTransaction) -> Id DFRFSSearch.FRFSSearch -> m (Maybe [DFRFSQuote.FRFSQuote])
mkQuoteFromCache fromStation toStation frfsConfig partnerOrg partnerOrgTransactionId searchId = do
  let isroundTripAllowed = frfsConfig.roundTripTicketLimit > 0
  now <- getCurrentTime
  let validTill = addUTCTime (secondsToNominalDiffTime frfsConfig.validTillSeconds) now
  case frfsConfig.providerId of
    Nothing -> pure Nothing
    Just providerId -> do
      let cachedQuoteDataKey =
            FRFSCachedQuoteKey
              { CachedQuote.fromStationId = fromStation.id,
                CachedQuote.toStationId = toStation.id,
                CachedQuote.providerId = providerId,
                CachedQuote.quoteType = DFRFSQuote.SingleJourney
              }
      mbSingleJourneyQuotes <-
        CachedQuote.findByFRFSCachedQuoteKey cachedQuoteDataKey >>= \case
          Nothing -> pure Nothing
          Just a -> mkQuotes fromStation toStation frfsConfig a DFRFSQuote.SingleJourney validTill searchId
      mbReturnJourneyQuotes <-
        if isroundTripAllowed
          then
            CachedQuote.findByFRFSCachedQuoteKey cachedQuoteDataKey{CachedQuote.quoteType = DFRFSQuote.ReturnJourney} >>= \case
              Nothing -> pure Nothing
              Just a -> mkQuotes toStation fromStation frfsConfig a DFRFSQuote.ReturnJourney validTill searchId
          else pure Nothing
      let quotes = catMaybes [mbSingleJourneyQuotes, mbReturnJourneyQuotes]
      return $ if null quotes then Nothing else Just quotes
  where
    mkQuotes fromStation' toStation' frfsConfig' frfsCachedData quoteType validTill' searchId' = do
      quoteId <- generateGUID
      now <- getCurrentTime
      let quote =
            DFRFSQuote.FRFSQuote
              { DFRFSQuote._type = quoteType,
                DFRFSQuote.bppItemId = Utils.partnerOrgBppItemId,
                DFRFSQuote.bppSubscriberId = Utils.partnerOrgBppSubscriberId,
                DFRFSQuote.bppSubscriberUrl = Utils.partnerOrgBppSubscriberUrl,
                DFRFSQuote.fromStationId = fromStation'.id,
                DFRFSQuote.id = quoteId,
                DFRFSQuote.price = frfsCachedData.price,
                DFRFSQuote.providerDescription = Nothing,
                DFRFSQuote.providerId = fromMaybe "metro_provider_id" frfsConfig'.providerId,
                DFRFSQuote.providerName = fromMaybe "metro_provider_name" frfsConfig'.providerName,
                DFRFSQuote.quantity = 1,
                DFRFSQuote.riderId = Utils.partnerOrgRiderId,
                DFRFSQuote.searchId = searchId',
                DFRFSQuote.stationsJson = frfsCachedData.stationsJson,
                DFRFSQuote.routeStationsJson = Nothing,
                DFRFSQuote.discountsJson = Nothing,
                DFRFSQuote.toStationId = toStation'.id,
                DFRFSQuote.validTill = validTill',
                DFRFSQuote.vehicleType = fromStation'.vehicleType,
                DFRFSQuote.merchantId = fromStation'.merchantId,
                DFRFSQuote.merchantOperatingCityId = fromStation.merchantOperatingCityId,
                DFRFSQuote.partnerOrgId = Just partnerOrg.orgId,
                DFRFSQuote.partnerOrgTransactionId = partnerOrgTransactionId,
                DFRFSQuote.createdAt = now,
                DFRFSQuote.updatedAt = now,
                DFRFSQuote.bppDelayedInterest = Nothing,
                DFRFSQuote.discountedTickets = Nothing,
                DFRFSQuote.eventDiscountAmount = Nothing,
                DFRFSQuote.oldCacheDump = Nothing
              }
      return $ Just quote

cretateBookingResIfBookingAlreadyCreated :: PartnerOrganization -> DFTB.FRFSTicketBooking -> DPOC.RegistrationConfig -> Flow UpsertPersonAndQuoteConfirmRes
cretateBookingResIfBookingAlreadyCreated partnerOrg booking regPOCfg = do
  merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show booking.merchantOperatingCityId)
  stations <- decodeFromText booking.stationsJson & fromMaybeM (InvalidStationJson (show booking.stationsJson))
  let routeStations = decodeFromText =<< booking.routeStationsJson
      discounts = decodeFromText =<< booking.discountsJson
  let bookingRes =
        FRFSTypes.FRFSTicketBookingStatusAPIRes
          { FRFSTypes._type = booking._type,
            bookingId = booking.id,
            city = merchantOperatingCity.city,
            createdAt = booking.createdAt,
            discountedTickets = booking.discountedTickets,
            discounts,
            eventDiscountAmount = booking.eventDiscountAmount,
            googleWalletJWTUrl = booking.googleWalletJWTUrl,
            isFareChanged = booking.isFareChanged,
            payment = Nothing,
            price = booking.price.amount,
            priceWithCurrency = mkPriceAPIEntity booking.price,
            quantity = booking.quantity,
            routeStations,
            stations,
            status = booking.status,
            tickets = [],
            updatedAt = booking.updatedAt,
            validTill = booking.validTill,
            vehicleType = booking.vehicleType
          }
  regToken <- getRegToken booking.riderId partnerOrg.orgId regPOCfg booking.merchantId
  let body = UpsertPersonAndQuoteConfirmResBody {bookingInfo = bookingRes, token = regToken.token}
  return
    UpsertPersonAndQuoteConfirmRes
      { body = Just body,
        quoteConfirmStatus = INIT_TRIGGERED
      }

createNewBookingAndTriggerInit :: PartnerOrganization -> UpsertPersonAndQuoteConfirmReq -> DPOC.RegistrationConfig -> Flow UpsertPersonAndQuoteConfirmRes
createNewBookingAndTriggerInit partnerOrg req regPOCfg = do
  quote <- QQuote.findById req.quoteId >>= fromMaybeM (FRFSQuoteNotFound req.quoteId.getId)
  fromStation <- CQS.findById quote.fromStationId >>= fromMaybeM (StationDoesNotExist $ "StationId: " <> quote.fromStationId.getId)
  toStation <- CQS.findById quote.toStationId >>= fromMaybeM (StationDoesNotExist $ "StationId: " <> quote.toStationId.getId)
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow fromStation.merchantOperatingCityId [] >>= fromMaybeM (FRFSConfigNotFound fromStation.merchantOperatingCityId.getId)
  redisLockSearchId <- Redis.tryLockRedis lockKey 10
  if not redisLockSearchId
    then throwError $ RedisLockStillProcessing lockKey
    else do
      let getFareReq =
            GetFareReq
              { fromStationCode = fromStation.code,
                toStationCode = toStation.code,
                routeCode = Nothing,
                numberOfPassengers = req.numberOfPassengers,
                mobileCountryCode = req.mobileCountryCode,
                mobileNumber = req.mobileNumber,
                identifierType = req.identifierType,
                partnerOrgTransactionId = Nothing,
                cityId = fromStation.merchantOperatingCityId,
                vehicleType = Just fromStation.vehicleType
              }
      let mbRegCoordinates = mkLatLong fromStation.lat fromStation.lon
      (personId, token) <- upsertPersonAndGetToken partnerOrg.orgId regPOCfg fromStation.merchantOperatingCityId fromStation.merchantId mbRegCoordinates getFareReq
      QSearch.updateRiderIdById personId req.searchId
      let isEventOngoing = fromMaybe False frfsConfig.isEventOngoing
      stats <- QPStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
      let ticketsBookedInEvent = fromMaybe 0 stats.ticketsBookedInEvent
          (discountedTickets, eventDiscountAmount) = Utils.getDiscountInfo isEventOngoing frfsConfig.freeTicketInterval frfsConfig.maxFreeTicketCashback quote.price req.numberOfPassengers ticketsBookedInEvent
      QQuote.backfillQuotesForCachedQuoteFlow personId req.numberOfPassengers discountedTickets eventDiscountAmount frfsConfig.isEventOngoing req.searchId
      bookingRes <- DFRFSTicketService.postFrfsQuoteConfirm (Just personId, fromStation.merchantId) quote.id
      let body = UpsertPersonAndQuoteConfirmResBody {bookingInfo = bookingRes, token}
      Redis.unlockRedis lockKey
      return
        UpsertPersonAndQuoteConfirmRes
          { body = Just body,
            quoteConfirmStatus = INIT_TRIGGERED
          }
  where
    lockKey = "FRFS:PartnerOrgId:" <> partnerOrg.orgId.getId <> ":UpsertPersonAndQuoteConfirm:SearchId:" <> req.searchId.getId
