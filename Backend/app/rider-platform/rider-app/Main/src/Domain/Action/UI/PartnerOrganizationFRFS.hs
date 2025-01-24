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
    getConfigByStationIds,
    shareTicketInfo,
    GetFareReq (..),
    GetFareResp (..),
    GetConfigResp (..),
    ShareTicketInfoResp (..),
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Data.Maybe (listToMaybe)
import Data.OpenApi hiding (email, info, name)
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Domain.Types.PartnerOrganization
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.External.Maps as Maps
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
import qualified Storage.Queries.FRFSTicket as QFT
import qualified Storage.Queries.FRFSTicketBokingPayment as QFTBP
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
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
      >>= validateToken
      >>= maybe (makeSessionViaPartner regPOCfg.sessionConfig entityId.getId mId.getId regPOCfg.fakeOtp pOrgId) return

  return (person.id, regToken.token)
  where
    validateToken :: (CacheFlow m r, EsqDBFlow m r) => Maybe SR.RegistrationToken -> m (Maybe SR.RegistrationToken)
    validateToken = \case
      Nothing -> pure Nothing
      Just regToken -> do
        let nominal = realToFrac $ regToken.tokenExpiry * 24 * 60 * 60
        expired <- Kernel.isExpired nominal regToken.updatedAt
        let res = bool Nothing (Just regToken) $ regToken.verified && not expired && regToken.createdViaPartnerOrgId == Just pOrgId
        when (isNothing res && regToken.createdViaPartnerOrgId == Just pOrgId) $ do
          RegistrationToken.deleteByTokenCreatedViaPartnerOrgId regToken.token pOrgId
        return res

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
