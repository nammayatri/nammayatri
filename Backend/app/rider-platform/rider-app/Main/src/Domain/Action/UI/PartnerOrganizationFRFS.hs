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
    partnerQuoteConfirm,
    GetFareReq (..),
    GetFareResp (..),
    GetConfigResp (..),
    ShareTicketInfoResp (..),
    UpsertPersonResp (..),
    GetFareReqV2 (..),
    PartnerQuoteConfirmReq (..),
    PartnerQuoteConfirmRes (..),
    GetFareRespV2 (..),
    PartnerQuoteConfirmResBody (..),
    QuoteConfirmStatus (..),
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified BecknV2.FRFS.Utils as Utils
import Data.OpenApi hiding (email, info, name)
import qualified Data.Text as T
import qualified Domain.Action.UI.FRFSTicketService as DFRFSTicketService
import qualified Domain.Action.UI.Registration as DReg
import Domain.Types.Extra.FRFSCachedQuote as CachedQuote
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Domain.Types.PartnerOrganization
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Domain.Types.Station
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, map, null)
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
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.FRFSUtils as Utils
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
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
    cityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data GetFareReqV2 = GetFareReqV2
  { fromStationCode :: Text,
    toStationCode :: Text,
    partnerOrgTransactionId :: Maybe (Id PartnerOrgTransaction),
    routeCode :: Maybe Text,
    cityId :: Id DMOC.MerchantOperatingCity
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
    quotes :: [FRFSTypes.FRFSQuoteAPIRes]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpsertPersonResp = UpsertPersonResp
  { personId :: Id SP.Person,
    token :: RegToken
  }

data PartnerQuoteConfirmReq = PartnerQuoteConfirmReq
  { searchId :: Id DFRFSSearch.FRFSSearch,
    quoteId :: Id DFRFSQuote.FRFSQuote,
    numberOfPassengers :: Int,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    identifierType :: SP.IdentifierType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteConfirmStatus = ON_SEARCH_NOT_RECEIVED_YET | INIT_TRIGGERED deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PartnerQuoteConfirmRes = PartnerQuoteConfirmRes
  { quoteConfirmStatus :: QuoteConfirmStatus,
    body :: Maybe PartnerQuoteConfirmResBody
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PartnerQuoteConfirmResBody = PartnerQuoteConfirmResBody
  { token :: RegToken,
    bookingStatusRes :: FRFSTypes.FRFSTicketBookingStatusAPIRes
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

  regToken <- do
    let entityId = person.id
    RegistrationToken.findAllByPersonId entityId
      <&> listToMaybe . sortOn (.updatedAt) . filter (isJust . (.createdViaPartnerOrgId))
      >>= validateToken pOrgId
      >>= maybe (makeSessionViaPartner regPOCfg.sessionConfig entityId.getId mId.getId regPOCfg.fakeOtp pOrgId) return

  return (person.id, regToken.token)

validateToken :: (CacheFlow m r, EsqDBFlow m r) => Id PartnerOrganization -> Maybe SR.RegistrationToken -> m (Maybe SR.RegistrationToken)
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
  let isGMMStationId = T.isPrefixOf "Ch" fromGMMStationId
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
  frfsConfig' <- B.runInReplica $ CQFRFSConfig.findByMerchantOperatingCityId fromStation'.merchantOperatingCityId >>= fromMaybeM (FRFSConfigNotFound fromStation'.merchantOperatingCityId.getId)

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
        void $ CallExternalBPP.status merchantId city bapConfig ticketBooking

getFareV2 :: PartnerOrganization -> Station -> Station -> Maybe (Id PartnerOrgTransaction) -> Maybe Text -> Flow GetFareRespV2
getFareV2 partnerOrg fromStation toStation partnerOrgTransactionId routeCode = do
  let merchantId = fromStation.merchantId
      frfsVehicleType = fromStation.vehicleType
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityId fromStation.merchantOperatingCityId >>= fromMaybeM (FRFSConfigNotFound fromStation.merchantOperatingCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  bapConfig <- CQBC.findByMerchantIdDomainAndVehicle merchantId (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType) >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId- " <> merchantId.getId)
  route <-
    maybe
      (pure Nothing)
      ( \routeCode' -> do
          route' <- B.runInReplica $ QRoute.findByRouteCode routeCode' >>= fromMaybeM (RouteNotFound routeCode')
          return $ Just route'
      )
      routeCode
  merchantOperatingCity <- CQMOC.findById fromStation.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show fromStation.merchantOperatingCityId)
  searchId <- generateGUID
  now <- getCurrentTime
  let searchReq =
        DFRFSSearch.FRFSSearch
          { id = searchId,
            vehicleType = frfsVehicleType,
            merchantId,
            merchantOperatingCityId = fromStation.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            quantity = 1,
            fromStationId = fromStation.id,
            toStationId = toStation.id,
            routeId = route <&> (.id),
            riderId = "partnerOrg_rider_id",
            partnerOrgTransactionId = partnerOrgTransactionId,
            partnerOrgId = Just partnerOrg.orgId,
            journeyLegInfo = Nothing,
            frequency = Nothing,
            isOnSearchReceived = Nothing,
            lineColor = Nothing,
            lineColorCode = Nothing,
            ..
          }
  fork ("FRFS Search: " <> searchId.getId) $ do
    QSearch.create searchReq
    CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq
  let validTill = addUTCTime (intToNominalDiffTime 900) now
  let isroundTripAllowed = frfsConfig.roundTripTicketLimit > 0
  (singleJourneyQuotes, returnJourneyQuotes) <- do
    case frfsConfig.providerId of
      Nothing -> pure ([], [])
      Just providerId -> do
        let cachedQuoteDataKey = FRFSCachedQuoteKey {CachedQuote.fromStationId = fromStation.id, CachedQuote.toStationId = toStation.id, CachedQuote.providerId, CachedQuote.quoteType = DFRFSQuote.SingleJourney}
        singleJourneyQuotes <-
          CachedQuote.findCachedQuoteByFromToProviderIdAndQuoteType cachedQuoteDataKey >>= \case
            Nothing -> pure []
            Just a -> mkQuotes fromStation toStation frfsConfig a DFRFSQuote.SingleJourney validTill searchId
        returnJourneyQuotes <-
          if isroundTripAllowed
            then do
              CachedQuote.findCachedQuoteByFromToProviderIdAndQuoteType cachedQuoteDataKey{CachedQuote.quoteType = DFRFSQuote.ReturnJourney} >>= \case
                Nothing -> pure []
                Just a -> mkQuotes fromStation toStation frfsConfig a DFRFSQuote.ReturnJourney validTill searchId
            else return []
        return (singleJourneyQuotes, returnJourneyQuotes)
  let quotes = singleJourneyQuotes ++ returnJourneyQuotes
  QQuote.createMany quotes
  quoteRes <- mapM mkQuoteRes quotes
  return $
    GetFareRespV2
      { searchId,
        quotes = quoteRes
      }
  where
    mkQuotes fromStation' toStation' frfsConfig frfsCachedData quoteType validTill searchId = do
      quoteId <- generateGUID
      now <- getCurrentTime
      let quote =
            DFRFSQuote.FRFSQuote
              { DFRFSQuote._type = quoteType,
                DFRFSQuote.bppItemId = "partnerOrg_bpp_item_id",
                DFRFSQuote.bppSubscriberId = "partnerOrg_bpp_subscriber_id",
                DFRFSQuote.bppSubscriberUrl = "partnerOrg_bpp_subscriber_url",
                DFRFSQuote.fromStationId = fromStation'.id,
                DFRFSQuote.id = quoteId,
                DFRFSQuote.price = frfsCachedData.price,
                DFRFSQuote.providerDescription = Nothing,
                DFRFSQuote.providerId = fromMaybe "" frfsConfig.providerId,
                DFRFSQuote.providerName = fromMaybe "" frfsConfig.providerName,
                DFRFSQuote.quantity = 1,
                DFRFSQuote.riderId = "partnerOrg_rider_id",
                DFRFSQuote.searchId = searchId,
                DFRFSQuote.stationsJson = frfsCachedData.stationsJson,
                DFRFSQuote.routeStationsJson = Nothing,
                DFRFSQuote.discountsJson = Nothing,
                DFRFSQuote.toStationId = toStation'.id,
                DFRFSQuote.validTill,
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
      return [quote]

mkLatLong :: Maybe Double -> Maybe Double -> Maybe Maps.LatLong
mkLatLong mbLat mbLon = case (mbLat, mbLon) of
  (Just lat, Just lon) -> Just $ Maps.LatLong {..}
  _ -> Nothing

mkQuoteRes :: (MonadFlow m) => DFRFSQuote.FRFSQuote -> m FRFSTypes.FRFSQuoteAPIRes
mkQuoteRes quote = do
  (stations :: [FRFSTypes.FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InternalError $ "Invalid stations jsons from db" <> show quote.stationsJson)
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

partnerQuoteConfirm :: PartnerOrganization -> PartnerQuoteConfirmReq -> Flow PartnerQuoteConfirmRes
partnerQuoteConfirm partnerOrg req = do
  redisLockSearchId <- Redis.tryLockRedis lockKey 10
  if not redisLockSearchId
    then throwError $ InternalError $ "Thread is still processing. Redis Lock Key" <> lockKey
    else do
      search <- QSearch.findById req.searchId >>= fromMaybeM (SearchRequestDoesNotExist req.searchId.getId)
      if isNothing search.isOnSearchReceived
        then do
          return
            PartnerQuoteConfirmRes
              { quoteConfirmStatus = ON_SEARCH_NOT_RECEIVED_YET,
                body = Nothing
              }
        else do
          quote <- QQuote.findById req.quoteId >>= fromMaybeM (QuoteNotFound req.quoteId.getId)
          fromStation <- CQS.findById quote.fromStationId >>= fromMaybeM (StationDoesNotExist $ "StationId: " <> quote.fromStationId.getId)
          toStation <- CQS.findById quote.toStationId >>= fromMaybeM (StationDoesNotExist $ "StationId: " <> quote.toStationId.getId)
          pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.REGISTRATION >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrg.orgId.getId $ show DPOC.REGISTRATION)
          regPOCfg <- DPOC.getRegistrationConfig pOrgCfg.config
          mbBooking <- QBooking.findByQuoteId req.quoteId
          case mbBooking of
            Just a -> do
              merchantOperatingCity <- CQMOC.findById a.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show a.merchantOperatingCityId)
              stations <- decodeFromText a.stationsJson & fromMaybeM (InternalError $ "Invalid stations jsons from booking. Booking Id :- " <> a.id.getId)
              let routeStations = decodeFromText =<< a.routeStationsJson
                  discounts = decodeFromText =<< a.discountsJson
              let bookingRes =
                    FRFSTypes.FRFSTicketBookingStatusAPIRes
                      { FRFSTypes._type = a._type,
                        bookingId = a.id,
                        city = merchantOperatingCity.city,
                        createdAt = a.createdAt,
                        discountedTickets = a.discountedTickets,
                        discounts,
                        eventDiscountAmount = a.eventDiscountAmount,
                        googleWalletJWTUrl = a.googleWalletJWTUrl,
                        isFareChanged = a.isFareChanged,
                        payment = Nothing,
                        price = a.price.amount,
                        priceWithCurrency = mkPriceAPIEntity a.price,
                        quantity = a.quantity,
                        routeStations,
                        stations,
                        status = a.status,
                        tickets = [],
                        updatedAt = a.updatedAt,
                        validTill = a.validTill,
                        vehicleType = a.vehicleType
                      }
              regToken <- do
                let entityId = a.riderId
                RegistrationToken.findAllByPersonId entityId
                  <&> listToMaybe . sortOn (.updatedAt) . filter (isJust . (.createdViaPartnerOrgId))
                  >>= validateToken partnerOrg.orgId
                  >>= maybe (makeSessionViaPartner regPOCfg.sessionConfig entityId.getId a.merchantId.getId regPOCfg.fakeOtp partnerOrg.orgId) return
              let body = PartnerQuoteConfirmResBody {bookingStatusRes = bookingRes, token = regToken.token}
              return
                PartnerQuoteConfirmRes
                  { body = Just body,
                    quoteConfirmStatus = INIT_TRIGGERED
                  }
            Nothing -> do
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
                        cityId = fromStation.merchantOperatingCityId
                      }
              let mbRegCoordinates = mkLatLong fromStation.lat fromStation.lon
              (personId, token) <- upsertPersonAndGetToken partnerOrg.orgId regPOCfg fromStation.merchantOperatingCityId fromStation.merchantId mbRegCoordinates getFareReq
              QSearch.updateRiderIdById personId req.searchId
              QQuote.updateManyRiderIdAndQuantityBySearchId personId req.numberOfPassengers req.searchId
              bookingRes <- DFRFSTicketService.postFrfsQuoteConfirm (Just personId, fromStation.merchantId) quote.id
              let body = PartnerQuoteConfirmResBody {bookingStatusRes = bookingRes, token}
              return
                PartnerQuoteConfirmRes
                  { body = Just body,
                    quoteConfirmStatus = INIT_TRIGGERED
                  }
  where
    lockKey = "PartnerQuoteConfirm:SearchId-" <> req.searchId.getId
