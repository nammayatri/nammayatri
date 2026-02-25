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
    partnerOrgAuthVerify,
    partnerOrgAuth,
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
    PartnerOrgAuthRes (..),
    PartnerOrgAuthVerifyRes (..),
    PartnerOrgAuthVerifyReq (..),
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Data.OpenApi hiding (description, email, info, name, title)
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as DReg
import Domain.Types.Extra.FRFSCachedQuote as CachedQuote
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Domain.Types.PartnerOrganization
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Domain.Types.Station
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.Prelude as KP
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common as Kernel
import Kernel.Utils.JSON (removeNullFields)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import SharedLogic.FRFSConfirm
import qualified SharedLogic.FRFSUtils as Utils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicket as QFT
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.FRFSTicketBookingPayment as QFTBP
import qualified Storage.Queries.FRFSTicketCategoryMetadataConfig as QFRFSTicketCategoryMetadataConfig
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.PersonStats as QPStats
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Storage.Queries.Transformers.FRFSQuoteCategory as TFQC
import Tools.Error
import qualified Tools.SMS as Sms

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
    googleWalletJWTUrl :: Maybe Text,
    providerId :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PartnerOrgAuthVerifyReq = PartnerOrgAuthVerifyReq
  { otp :: Text,
    tokenId :: Id SR.RegistrationToken
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

validateAuthVerifyReq' :: Validate PartnerOrgAuthVerifyReq
validateAuthVerifyReq' PartnerOrgAuthVerifyReq {..} =
  sequenceA_
    [ validateField "otp" otp $ ExactLength 4 `And` star P.digit
    ]

newtype PartnerOrgAuthVerifyRes = PartnerOrgAuthVerifyRes
  {token :: RegToken}
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data PartnerOrgAuthRes = PartnerOrgAuthRes
  { authId :: Id SR.RegistrationToken,
    maskedMobileNumber :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

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

  (regToken, _) <- getRegToken person.id pOrgId regPOCfg mId True

  return (person.id, regToken.token)

getRegToken :: Id SP.Person -> Id PartnerOrganization -> DPOC.RegistrationConfig -> Id DM.Merchant -> Bool -> Flow (SR.RegistrationToken, Bool)
getRegToken personId pOrgId regPOCfg mId isApiKeyAuth = do
  let entityId = personId
  RegistrationToken.findAllByPersonId entityId
    <&> KP.listToMaybe . sortOn (.updatedAt) . filter (isJust . (.createdViaPartnerOrgId))
    >>= validateToken pOrgId
    >>= maybe (makeSessionViaPartner regPOCfg.sessionConfig entityId.getId mId.getId regPOCfg.fakeOtp pOrgId isApiKeyAuth) return

validateToken :: Id PartnerOrganization -> Maybe SR.RegistrationToken -> Flow (Maybe (SR.RegistrationToken, Bool))
validateToken pOrgId = \case
  Nothing -> pure Nothing
  Just regToken -> do
    let nominal = realToFrac $ regToken.tokenExpiry * 24 * 60 * 60
    expired <- Kernel.isExpired nominal regToken.updatedAt
    let res = bool Nothing (Just regToken) $ regToken.verified && not expired && regToken.createdViaPartnerOrgId == Just pOrgId
    when (isNothing res && regToken.createdViaPartnerOrgId == Just pOrgId) $ do
      RegistrationToken.deleteByTokenCreatedViaPartnerOrgId regToken.token pOrgId
    return $ (,False) <$> res

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
  person <- DReg.createPerson authReq identifierType notificationToken mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice Nothing merchant (Just partnerOrgId)
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
          businessEmail = Nothing,
          language = Nothing,
          gender = Nothing,
          otpChannel = Nothing,
          registrationLat = mbRegCoordinates <&> (.lat),
          registrationLon = mbRegCoordinates <&> (.lon),
          enableOtpLessRide = Nothing,
          allowBlockedUserLogin = Nothing,
          isOperatorReq = Nothing,
          reuseToken = Nothing
        }

makeSessionViaPartner ::
  SmsSessionConfig ->
  Text ->
  Text ->
  Text ->
  Id PartnerOrganization ->
  Bool ->
  Flow (SR.RegistrationToken, Bool)
makeSessionViaPartner sessionConfig entityId mId fakeOtp partnerOrgId isApiKeyAuth = do
  logDebug $ "We are in makeSessionViaPartner Creating session for entityId:" +|| entityId ||+ " merchantId:" +|| mId ||+ " partnerOrgId:" +|| partnerOrgId ||+ ""
  let authMedium = SR.PARTNER_ORG
  regToken <- makeSession authMedium sessionConfig entityId mId (Just fakeOtp) partnerOrgId
  void $ RegistrationToken.create regToken
  when isApiKeyAuth $ void $ RegistrationToken.setDirectAuth regToken.id authMedium
  return (regToken, True)

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

getConfigByStationIds :: PartnerOrganization -> Text -> Text -> DIBC.IntegratedBPPConfig -> Flow GetConfigResp
getConfigByStationIds partnerOrg fromGMMStationId toGMMStationId integratedBPPConfig = do
  let isGMMStationId = T.isPrefixOf "Ch" fromGMMStationId && T.isPrefixOf "Ch" toGMMStationId
  (fromStation', toStation') <-
    if isGMMStationId
      then do
        let fromPOrgStationId = Id fromGMMStationId
        let toPOrgStationId = Id toGMMStationId
        fromStation' <- CQPOS.findStationWithPOrgName partnerOrg.orgId fromPOrgStationId integratedBPPConfig
        toStation' <- CQPOS.findStationWithPOrgName partnerOrg.orgId toPOrgStationId integratedBPPConfig
        return (fromStation', toStation')
      else do
        let fromStationId' = Id fromGMMStationId
        let toStationId' = Id toGMMStationId
        fromStation' <- CQPOS.findStationWithPOrgIdAndStationId fromStationId'.getId partnerOrg.orgId integratedBPPConfig
        toStation' <- CQPOS.findStationWithPOrgIdAndStationId toStationId'.getId partnerOrg.orgId integratedBPPConfig
        return (fromStation', toStation')
  frfsConfig' <- B.runInReplica $ CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow fromStation'.merchantOperatingCityId [] >>= fromMaybeM (FRFSConfigNotFound fromStation'.merchantOperatingCityId.getId)

  unless (frfsConfig'.merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId.getId ||+ " not valid for merchantId:" +|| frfsConfig'.merchantId.getId ||+ ""
  unless (fromStation'.merchantOperatingCityId == toStation'.merchantOperatingCityId) $
    throwError . InvalidRequest $ "origin:" +|| fromStation'.name ||+ "and destination:" +|| toStation'.name ||+ " locations are not of same city"

  fromStation <- Utils.mkPOrgStationAPIRes fromStation' (Just partnerOrg.orgId)
  toStation <- Utils.mkPOrgStationAPIRes toStation' (Just partnerOrg.orgId)
  let modifiedFrfsConfig = case integratedBPPConfig.providerConfig of
        DIBC.ONDC DIBC.ONDCBecknConfig {providerInfo} ->
          case providerInfo of
            Just providerInfo' ->
              mkProviderSpecificConfig providerInfo' frfsConfig'
            Nothing -> frfsConfig'
        _ -> frfsConfig'
  let frfsConfig = Utils.mkFRFSConfigAPI modifiedFrfsConfig
  moc <- CQMOC.findById fromStation'.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound fromStation'.merchantOperatingCityId.getId)
  let city = moc.city
  let cityId = moc.id
  pure $ GetConfigResp {..}
  where
    mkProviderSpecificConfig :: DIBC.ProviderLevelInfo -> FRFSConfig -> FRFSConfig
    mkProviderSpecificConfig providerInfo baseFrfsConfig =
      baseFrfsConfig
        { bookingEndTime = providerInfo.bookingEndTime,
          bookingStartTime = providerInfo.bookingStartTime,
          isCancellationAllowed = providerInfo.isCancellationAllowed,
          oneWayTicketLimit = providerInfo.oneWayTicketLimit,
          roundTripTicketLimit = providerInfo.roundTripTicketLimit,
          providerId = Just providerInfo.providerId
        }

shareTicketInfo :: Id DFTB.FRFSTicketBooking -> Flow ShareTicketInfoResp
shareTicketInfo ticketBookingId = do
  -- TODO: Make it findAllWithKVAndConditionalDB
  tickets' <- B.runInReplica $ QFT.findAllByTicketBookingId ticketBookingId

  when (null tickets') $
    throwError $ FRFSTicketsForBookingDoesNotExist ticketBookingId.getId

  let tickets = map Utils.mkTicketAPI tickets'

  ticketBooking <- B.runInReplica $ QFTB.findById ticketBookingId >>= fromMaybeM (FRFSTicketBookingNotFound ticketBookingId.getId)
  paymentBooking <- B.runInReplica $ QFTBP.findTicketBookingPayment ticketBooking >>= fromMaybeM (FRFSTicketBookingPaymentNotFound ticketBookingId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity ticketBooking
  fromStation' <- OTPRest.getStationByGtfsIdAndStopCode ticketBooking.fromStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound $ "Station not found for fromStationCode:" +|| ticketBooking.fromStationCode ||+ "")
  toStation' <- OTPRest.getStationByGtfsIdAndStopCode ticketBooking.toStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound $ "Station not found for toStationCode:" +|| ticketBooking.toStationCode ||+ "")
  city <- CQMOC.findById fromStation'.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound fromStation'.merchantOperatingCityId.getId)
  pOrgId <- ticketBooking.partnerOrgId & fromMaybeM (InternalError $ "PartnerOrgId is missing for ticketBookingId:" +|| ticketBookingId.getId ||+ "")

  fromStation <- Utils.mkPOrgStationAPIRes fromStation' (Just pOrgId)
  toStation <- Utils.mkPOrgStationAPIRes toStation' (Just pOrgId)

  void $ bppStatusSync fromStation'.merchantId pOrgId city ticketBooking

  let providerId = case integratedBPPConfig.providerConfig of
        DIBC.ONDC DIBC.ONDCBecknConfig {providerInfo} -> providerInfo <&> (.providerId)
        _ -> Nothing

  pure $
    ShareTicketInfoResp
      { returnType = ticketBooking._type,
        bookingPrice = ticketBooking.totalPrice.amount,
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
            CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback city.id merchantId (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType)
              >>= fromMaybeM (BecknConfigNotFound $ "MerchantId:" +|| merchantId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType ||+ "")
        void $ CallExternalBPP.status merchantId city bapConfig ticketBooking

getFareV2 :: MerchantOperatingCity -> PartnerOrganization -> Station -> Station -> Maybe (Id PartnerOrgTransaction) -> Maybe Text -> DIBC.IntegratedBPPConfig -> Flow GetFareRespV2
getFareV2 merchantOperatingCity partnerOrg fromStation toStation partnerOrgTransactionId routeCode integratedBPPConfig = do
  let merchantId = fromStation.merchantId
      frfsVehicleType = fromStation.vehicleType
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow fromStation.merchantOperatingCityId [] >>= fromMaybeM (FRFSConfigNotFound fromStation.merchantOperatingCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  bapConfig <-
    CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchantId (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType)
      >>= fromMaybeM (BecknConfigNotFound $ "MerchantId:" +|| merchantId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType ||+ "")
  route <-
    maybe
      (pure Nothing)
      (\routeCode' -> OTPRest.getRouteByRouteId integratedBPPConfig routeCode')
      routeCode
  let frfsRouteDetails =
        [ FRFSRouteDetails
            { routeCode = routeCode,
              startStationCode = fromStation.code,
              endStationCode = toStation.code,
              serviceTier = Nothing -- TODO: pass this for optimization
            }
        ]
  searchReq <- mkSearchReq bapConfig frfsVehicleType partnerOrgTransactionId partnerOrg fromStation toStation route
  fork ("FRFS Search: " <> searchReq.id.getId) $ do
    QSearch.create searchReq
    CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq Nothing frfsRouteDetails integratedBPPConfig [] [] False
  mbQuotes <- mkQuoteFromCache fromStation toStation frfsConfig partnerOrg partnerOrgTransactionId searchReq.id
  whenJust mbQuotes $ \quotes -> do
    QQuote.createMany (fst <$> quotes)
    QFRFSQuoteCategory.createMany (concatMap snd quotes)
  case mbQuotes of
    Just quotes -> do
      quoteRes <- mapM mkQuoteRes quotes
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
    mkSearchReq bapConfig frfsVehicleType partnerOrgTransactionId' partnerOrg' fromStation' toStation' route = do
      now <- getCurrentTime
      let validTill = addUTCTime (maybe 30 KP.intToNominalDiffTime bapConfig.searchTTLSec) now
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
            fromStationCode = fromStation'.code,
            toStationCode = toStation'.code,
            routeCode = route <&> (.code),
            riderId = Utils.partnerOrgRiderId,
            partnerOrgTransactionId = partnerOrgTransactionId',
            partnerOrgId = Just partnerOrg'.orgId,
            isOnSearchReceived = Nothing,
            onSearchFailed = Nothing,
            integratedBppConfigId = integratedBPPConfig.id,
            recentLocationId = Nothing,
            validTill = Just validTill,
            multimodalSearchRequestId = Nothing,
            searchAsParentStops = Nothing,
            busLocationData = [],
            fromStationPoint = Maps.LatLong <$> fromStation'.lat <*> fromStation'.lon,
            toStationPoint = Maps.LatLong <$> toStation'.lat <*> toStation'.lon,
            fromStationName = Just fromStation'.name,
            toStationName = Just toStation'.name,
            fromStationAddress = fromStation'.address,
            toStationAddress = toStation'.address,
            vehicleNumber = Nothing,
            ..
          }

mkLatLong :: Maybe Double -> Maybe Double -> Maybe Maps.LatLong
mkLatLong mbLat mbLon = case (mbLat, mbLon) of
  (Just lat, Just lon) -> Just $ Maps.LatLong {..}
  _ -> Nothing

mkQuoteRes :: (MonadFlow m) => (DFRFSQuote.FRFSQuote, [FRFSQuoteCategory.FRFSQuoteCategory]) -> m FRFSTypes.FRFSQuoteAPIRes
mkQuoteRes (quote, quoteCategories) = do
  (stations :: [FRFSTypes.FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InvalidStationJson $ show quote.stationsJson)
  let routeStations :: Maybe [FRFSTypes.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
      fareParameters = Utils.mkFareParameters (Utils.mkCategoryPriceItemFromQuoteCategories quoteCategories)
      categories = map Utils.mkCategoryInfoResponse quoteCategories
  singleAdultTicketPrice <- find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice) & fromMaybeM (InternalError "Single Adult Ticket Price not found.")
  return $
    FRFSTypes.FRFSQuoteAPIRes
      { quoteId = quote.id,
        _type = quote._type,
        price = singleAdultTicketPrice.amount,
        priceWithCurrency = mkPriceAPIEntity singleAdultTicketPrice,
        quantity = fareParameters.totalQuantity,
        validTill = quote.validTill,
        vehicleType = quote.vehicleType,
        discountedTickets = quote.discountedTickets,
        eventDiscountAmount = quote.eventDiscountAmount,
        integratedBppConfigId = quote.integratedBppConfigId,
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
      mbBooking <- QFTB.findByQuoteId req.quoteId
      case mbBooking of
        Just booking -> cretateBookingResIfBookingAlreadyCreated partnerOrg booking regPOCfg
        Nothing -> createNewBookingAndTriggerInit partnerOrg req regPOCfg

mkQuoteFromCache :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Station -> Station -> FRFSConfig -> PartnerOrganization -> Maybe (Id PartnerOrgTransaction) -> Id DFRFSSearch.FRFSSearch -> m (Maybe [(DFRFSQuote.FRFSQuote, [FRFSQuoteCategory.FRFSQuoteCategory])])
mkQuoteFromCache fromStation toStation frfsConfig partnerOrg partnerOrgTransactionId searchId = do
  let isroundTripAllowed = frfsConfig.roundTripTicketLimit > 0
  now <- getCurrentTime
  let validTill = addUTCTime (secondsToNominalDiffTime frfsConfig.validTillSeconds) now
  case frfsConfig.providerId of
    Nothing -> pure Nothing
    Just providerId -> do
      let cachedQuoteDataKey =
            FRFSCachedQuoteKey
              { CachedQuote.fromStationId = fromStation.code,
                CachedQuote.toStationId = toStation.code,
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
                DFRFSQuote.fromStationCode = fromStation'.code,
                DFRFSQuote.toStationCode = toStation'.code,
                DFRFSQuote.id = quoteId,
                DFRFSQuote.providerDescription = Nothing,
                DFRFSQuote.providerId = fromMaybe "metro_provider_id" frfsConfig'.providerId,
                DFRFSQuote.providerName = fromMaybe "metro_provider_name" frfsConfig'.providerName,
                DFRFSQuote.riderId = Utils.partnerOrgRiderId,
                DFRFSQuote.searchId = searchId',
                DFRFSQuote.stationsJson = frfsCachedData.stationsJson,
                DFRFSQuote.routeStationsJson = Nothing,
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
                DFRFSQuote.integratedBppConfigId = fromStation'.integratedBppConfigId,
                DFRFSQuote.fareDetails = Nothing,
                DFRFSQuote.oldCacheDump = Nothing,
                DFRFSQuote.multimodalSearchRequestId = Nothing,
                DFRFSQuote.busLocationData = [],
                DFRFSQuote.fromStationAddress = fromStation'.address,
                DFRFSQuote.fromStationName = Just fromStation'.name,
                DFRFSQuote.fromStationPoint = Maps.LatLong <$> fromStation'.lat <*> fromStation'.lon,
                DFRFSQuote.toStationAddress = toStation'.address,
                DFRFSQuote.toStationName = Just toStation'.name,
                DFRFSQuote.toStationPoint = Maps.LatLong <$> toStation'.lat <*> toStation'.lon,
                DFRFSQuote.vehicleNumber = Nothing
              }
      quoteCategoryId <- generateGUID
      ticketCategoryMetadataConfig' <- QFRFSTicketCategoryMetadataConfig.findByCategoryVehicleAndCity ADULT fromStation'.vehicleType fromStation.merchantOperatingCityId
      let quoteCategories =
            [ FRFSQuoteCategory.FRFSQuoteCategory
                { id = quoteCategoryId,
                  category = ADULT,
                  quoteId = quoteId,
                  bppItemId = Utils.partnerOrgBppItemId,
                  price = frfsCachedData.price, -- Single Ticket Price
                  offeredPrice = frfsCachedData.price, -- Single Ticket Offered Price (Should be less than or equal to price)
                  finalPrice = Nothing,
                  categoryMeta = TFQC.mkQuoteCategoryMetadata (ticketCategoryMetadataConfig' <&> (.code)) (ticketCategoryMetadataConfig' <&> (.title)) (ticketCategoryMetadataConfig' <&> (.description)) (ticketCategoryMetadataConfig' <&> (.tnc)),
                  merchantId = fromStation'.merchantId,
                  merchantOperatingCityId = fromStation.merchantOperatingCityId,
                  selectedQuantity = 1,
                  createdAt = now,
                  updatedAt = now,
                  seatIds = Nothing,
                  seatLabels = Nothing
                }
            ]
      return $ Just (quote, quoteCategories)

cretateBookingResIfBookingAlreadyCreated :: PartnerOrganization -> DFTB.FRFSTicketBooking -> DPOC.RegistrationConfig -> Flow UpsertPersonAndQuoteConfirmRes
cretateBookingResIfBookingAlreadyCreated partnerOrg booking regPOCfg = do
  merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show booking.merchantOperatingCityId)
  stations <- decodeFromText booking.stationsJson & fromMaybeM (InvalidStationJson (show booking.stationsJson))
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
  let fareParameters = Utils.mkFareParameters (Utils.mkCategoryPriceItemFromQuoteCategories quoteCategories)
  let routeStations = decodeFromText =<< booking.routeStationsJson
  let bookingRes =
        FRFSTypes.FRFSTicketBookingStatusAPIRes
          { FRFSTypes._type = booking._type,
            bookingId = booking.id,
            city = merchantOperatingCity.city,
            createdAt = booking.createdAt,
            discountedTickets = booking.discountedTickets,
            eventDiscountAmount = booking.eventDiscountAmount,
            quoteCategories = map mkFRFSQuoteCategoryAPIEntity quoteCategories,
            googleWalletJWTUrl = booking.googleWalletJWTUrl,
            isFareChanged = booking.isFareChanged,
            payment = Nothing,
            price = Just booking.totalPrice.amount,
            priceWithCurrency = Just $ mkPriceAPIEntity booking.totalPrice,
            quantity = Just fareParameters.totalQuantity,
            routeStations,
            stations,
            status = booking.status,
            tickets = [],
            updatedAt = booking.updatedAt,
            validTill = booking.validTill,
            vehicleType = booking.vehicleType,
            integratedBppConfigId = booking.integratedBppConfigId
          }
  (regToken, _) <- getRegToken booking.riderId partnerOrg.orgId regPOCfg booking.merchantId True
  let body = UpsertPersonAndQuoteConfirmResBody {bookingInfo = bookingRes, token = regToken.token}
  return
    UpsertPersonAndQuoteConfirmRes
      { body = Just body,
        quoteConfirmStatus = INIT_TRIGGERED
      }
  where
    mkFRFSQuoteCategoryAPIEntity :: FRFSQuoteCategory.FRFSQuoteCategory -> FRFSTypes.FRFSQuoteCategoryAPIEntity
    mkFRFSQuoteCategoryAPIEntity FRFSQuoteCategory.FRFSQuoteCategory {..} =
      FRFSTypes.FRFSQuoteCategoryAPIEntity {categoryMetadata = mkCategoryMetadataAPIEntity <$> categoryMeta, price = mkPriceAPIEntity price, offeredPrice = mkPriceAPIEntity offeredPrice, finalPrice = mkPriceAPIEntity <$> finalPrice, ..}
      where
        mkCategoryMetadataAPIEntity FRFSQuoteCategory.QuoteCategoryMetadata {..} = FRFSTypes.FRFSTicketCategoryMetadataAPIEntity {..}

createNewBookingAndTriggerInit :: PartnerOrganization -> UpsertPersonAndQuoteConfirmReq -> DPOC.RegistrationConfig -> Flow UpsertPersonAndQuoteConfirmRes
createNewBookingAndTriggerInit partnerOrg req regPOCfg = do
  quote <- QQuote.findById req.quoteId >>= fromMaybeM (FRFSQuoteNotFound req.quoteId.getId)
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId req.quoteId
  let selections =
        map
          ( \category ->
              Utils.QuoteCategorySelection
                { qcQuoteCategoryId = category.id
                , qcQuantity =
                    if category.category == ADULT
                      then req.numberOfPassengers
                      else category.selectedQuantity
                , qcSeatIds = Nothing
                , qcSeatLabels = Nothing
                }
          )
          quoteCategories
  updatedQuoteCategories <-
    Utils.updateQuoteCategoriesWithSelections
      selections
      quoteCategories
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  fromStation <- OTPRest.getStationByGtfsIdAndStopCode quote.fromStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound $ "Station not found for fromStationCode:" +|| quote.fromStationCode ||+ "")
  toStation <- OTPRest.getStationByGtfsIdAndStopCode quote.toStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound $ "Station not found for toStationCode:" +|| quote.toStationCode ||+ "")
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
      stats <- B.runInMasterDbAndRedis $ QPStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
      let ticketsBookedInEvent = fromMaybe 0 stats.ticketsBookedInEvent
          fareParameters = Utils.mkFareParameters (Utils.mkCategoryPriceItemFromQuoteCategories updatedQuoteCategories)
          (discountedTickets, eventDiscountAmount) =
            case find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice) of
              Just adultPrice -> Utils.getDiscountInfo isEventOngoing frfsConfig.freeTicketInterval frfsConfig.maxFreeTicketCashback adultPrice req.numberOfPassengers ticketsBookedInEvent
              Nothing -> (Nothing, Nothing)
      QQuote.backfillQuotesForCachedQuoteFlow personId discountedTickets eventDiscountAmount frfsConfig.isEventOngoing req.searchId
      let selectedQuoteCategories =
            map
              ( \quoteCategory -> FRFSTypes.FRFSCategorySelectionReq {quoteCategoryId = quoteCategory.id, quantity = quoteCategory.selectedQuantity, seatIds = Nothing}
              )
              updatedQuoteCategories
      bookingRes <- postFrfsQuoteV2ConfirmUtil (Just personId, fromStation.merchantId) quote selectedQuoteCategories Nothing Nothing Nothing (Just False) integratedBPPConfig Nothing
      let body = UpsertPersonAndQuoteConfirmResBody {bookingInfo = bookingRes, token}
      Redis.unlockRedis lockKey
      return
        UpsertPersonAndQuoteConfirmRes
          { body = Just body,
            quoteConfirmStatus = INIT_TRIGGERED
          }
  where
    lockKey = "FRFS:PartnerOrgId:" <> partnerOrg.orgId.getId <> ":UpsertPersonAndQuoteConfirm:SearchId:" <> req.searchId.getId

partnerOrgAuth :: Id DFTB.FRFSTicketBooking -> Flow PartnerOrgAuthRes
partnerOrgAuth ticketBookingId = do
  ticketBooking <- B.runInReplica $ QFTB.findById ticketBookingId >>= fromMaybeM (FRFSTicketBookingNotFound ticketBookingId.getId)
  case ticketBooking.partnerOrgId of
    Nothing -> throwError $ FRFSBookingNotMadeThroughPartnerOrg ticketBookingId.getId
    Just partnerOrgId -> do
      let entityId = ticketBooking.riderId
      person <- B.runInReplica $ Person.findById entityId >>= fromMaybeM (PersonNotFound ticketBooking.riderId.getId)
      pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrgId DPOC.REGISTRATION >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrgId.getId $ show DPOC.REGISTRATION)
      regPOCfg <- DPOC.getRegistrationConfig pOrgCfg.config
      smsCfg <- asks (.smsCfg)
      let isApiKeyAuth = False
          scfg = smsCfg.sessionConfig
      newOtp <- generateOTPCode
      let updatedSessionConfig = regPOCfg.sessionConfig{authExpiry = scfg.authExpiry}
      (regToken, isTokenNew) <- getRegToken entityId partnerOrgId regPOCfg{fakeOtp = newOtp, sessionConfig = updatedSessionConfig} ticketBooking.merchantId isApiKeyAuth
      unless isTokenNew $ RegistrationToken.updateOtpByIdForPartnerOrgId regToken.id partnerOrgId newOtp scfg.authExpiry
      let mRiderMobileCountryCode = person.mobileCountryCode
      mobileNumber <- mapM decrypt person.mobileNumber
      sendTicketCancelOTPSMS mobileNumber mRiderMobileCountryCode ticketBooking newOtp
      maskedMobileNumber <- (maskMobileNumber <$> mobileNumber) & fromMaybeM (PersonFieldNotPresent "mobileNumber")
      return $ PartnerOrgAuthRes regToken.id maskedMobileNumber
      where
        sendTicketCancelOTPSMS :: Maybe Text -> Maybe Text -> DFTB.FRFSTicketBooking -> Text -> Flow ()
        sendTicketCancelOTPSMS mRiderNumber mRiderMobileCountryCode ticketBooking' otpCode' =
          withLogTag ("SMS:PersonId:" <> ticketBooking'.riderId.getId) $ do
            mobileNumber <- mRiderNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
            let mocId = ticketBooking'.merchantOperatingCityId
                countryCode = fromMaybe "+91" mRiderMobileCountryCode
                phoneNumber = countryCode <> mobileNumber
            buildSmsReq <-
              MessageBuilder.buildFRFSTicketCancelOTPMessage mocId $
                MessageBuilder.BuildFRFSTicketCancelOTPMessageReq
                  { otp = otpCode'
                  }

            Sms.sendSMS ticketBooking'.merchantId mocId (buildSmsReq phoneNumber) >>= Sms.checkSmsResult
        maskMobileNumber :: Text -> Text
        maskMobileNumber text = if (T.length text) > 4 then T.take 2 text <> "XXXXXX" <> T.takeEnd 2 text else "XXXXXX"

partnerOrgAuthVerify ::
  PartnerOrgAuthVerifyReq ->
  Flow PartnerOrgAuthVerifyRes
partnerOrgAuthVerify req = do
  runRequestValidation validateAuthVerifyReq' req
  regToken@SR.RegistrationToken {..} <- DReg.getRegistrationTokenE req.tokenId
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (regToken.authValueHash == req.otp && isJust regToken.createdViaPartnerOrgId) $ throwError InvalidAuthData
  void $ RegistrationToken.setDirectAuth regToken.id SR.SMS
  return $ PartnerOrgAuthVerifyRes regToken.token
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired
