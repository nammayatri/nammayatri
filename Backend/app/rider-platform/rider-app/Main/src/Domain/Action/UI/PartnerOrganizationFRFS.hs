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
import Data.Maybe (listToMaybe)
import Data.OpenApi hiding (email, info, name)
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FRFSConfig as DFRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFT
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import qualified Domain.Types.PartnerOrgStation as DPOS
import Domain.Types.PartnerOrganization
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.Station as Station
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.External.Maps as Maps
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.CachedQueries.Station as CQS
import qualified Storage.Queries.FRFSTicket as QFT
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Error

data GetFareReq = GetFareReq
  { fromStationCode :: Text,
    toStationCode :: Text,
    numberOfPassengers :: Int,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    identifierType :: SP.IdentifierType,
    partnerOrgTransactionId :: Id PartnerOrgTransaction
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
    city :: Context.City
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ShareTicketInfoResp = ShareTicketInfoResp
  { tickets :: [FRFSTypes.FRFSTicketAPI],
    city :: Context.City,
    returnType :: DFRFSQuote.FRFSQuoteType,
    fromStation :: FRFSTypes.FRFSStationAPI,
    toStation :: FRFSTypes.FRFSStationAPI,
    partnerOrgTransactionId :: Maybe (Id PartnerOrgTransaction)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

upsertPersonAndGetToken ::
  ( HasFlowEnv m r '["version" ::: DeploymentVersion],
    EncFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    CacheFlow m r
  ) =>
  Id PartnerOrganization ->
  DPOC.RegistrationConfig ->
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  Maybe Maps.LatLong ->
  GetFareReq ->
  m (Id SP.Person, RegToken)
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
      <&> listToMaybe . sortBy (comparing (.updatedAt))
      >>= maybe (makeSessionViaPartner regPOCfg.sessionConfig entityId.getId mId.getId regPOCfg.fakeOtp pOrgId) return

  return (person.id, regToken.token)

createPersonViaPartner ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["version" ::: DeploymentVersion]
  ) =>
  GetFareReq ->
  DM.Merchant ->
  Maybe Maps.LatLong ->
  Id PartnerOrganization ->
  m (SP.Person, Bool)
createPersonViaPartner req merchant mbRegCoordinates partnerOrgId = do
  let identifierType = req.identifierType
      notificationToken = Nothing
      mbBundleVersion = Nothing
      mbClientVersion = Nothing
      mbClientConfigVersion = Nothing
      mbDevice = Nothing
      authReq = buildPartnerAuthReq identifierType notificationToken
  person <- DReg.createPerson authReq identifierType notificationToken mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice merchant (Just partnerOrgId)
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
          registrationLon = mbRegCoordinates <&> (.lon)
        }

makeSessionViaPartner ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  SmsSessionConfig ->
  Text ->
  Text ->
  Text ->
  Id PartnerOrganization ->
  m SR.RegistrationToken
makeSessionViaPartner sessionConfig entityId mId fakeOtp partnerOrgId = do
  let authMedium = SR.PARTNER_ORG
  regToken <- makeSession authMedium sessionConfig entityId mId (Just fakeOtp) partnerOrgId
  void $ RegistrationToken.create regToken
  void $ RegistrationToken.setDirectAuth regToken.id authMedium
  return regToken

makeSession ::
  MonadFlow m =>
  SR.Medium ->
  SmsSessionConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Id PartnerOrganization ->
  m SR.RegistrationToken
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

getConfigByStationIds :: (CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => PartnerOrganization -> Id DPOS.PartnerOrgStation -> Id DPOS.PartnerOrgStation -> m GetConfigResp
getConfigByStationIds partnerOrg fromPOrgStationId toPOrgStationId = do
  fromStation' <- B.runInReplica $ CQPOS.findStationWithPOrgName partnerOrg.orgId fromPOrgStationId
  toStation' <- B.runInReplica $ CQPOS.findStationWithPOrgName partnerOrg.orgId toPOrgStationId
  frfsConfig' <- B.runInReplica $ CQFRFSConfig.findByMerchantOperatingCityId fromStation'.merchantOperatingCityId >>= fromMaybeM (FRFSConfigNotFound fromStation'.merchantOperatingCityId.getId)

  unless (frfsConfig'.merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId.getId ||+ " not valid for merchantId:" +|| frfsConfig'.merchantId.getId ||+ ""
  unless (fromStation'.merchantOperatingCityId == toStation'.merchantOperatingCityId) $
    throwError . InvalidRequest $ "origin:" +|| fromStation'.name ||+ "and destination:" +|| toStation'.name ||+ " locations are not of same city"

  fromStation <- mkPOrgStationAPIRes fromStation' partnerOrg.orgId
  toStation <- mkPOrgStationAPIRes toStation' partnerOrg.orgId
  let frfsConfig = mkFRFSConfigAPI frfsConfig'
  city <- CQMOC.findById fromStation'.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound fromStation'.merchantOperatingCityId.getId)

  pure $ GetConfigResp {..}
  where
    mkFRFSConfigAPI :: DFRFSConfig.FRFSConfig -> FRFSTypes.FRFSConfigAPIRes
    mkFRFSConfigAPI DFRFSConfig.FRFSConfig {..} = FRFSTypes.FRFSConfigAPIRes {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Id PartnerOrganization -> m FRFSTypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} pOrgId = do
  pOrgStation <- B.runInReplica $ CQPOS.findByStationIdAndPOrgId id pOrgId >>= fromMaybeM (PartnerOrgStationNotFoundForStationId pOrgId.getId id.getId)
  pure $ FRFSTypes.FRFSStationAPI {name = pOrgStation.name, stationType = Nothing, color = Nothing, sequenceNum = Nothing, ..}

shareTicketInfo :: (CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, CallFRFSBPP.BecknAPICallFlow m r) => Id DFTB.FRFSTicketBooking -> m ShareTicketInfoResp
shareTicketInfo ticketBookingId = do
  -- TODO: Make it findAllWithKVAndConditionalDB
  tickets' <- B.runInReplica $ QFT.findAllByTicketBookingId ticketBookingId

  when (null tickets') $
    throwError $ FRFSTicketsForBookingDoesNotExist ticketBookingId.getId

  let tickets = map mkTicketAPI tickets'

  ticketBooking <- B.runInReplica $ QFTB.findById ticketBookingId >>= fromMaybeM (FRFSTicketBookingNotFound ticketBookingId.getId)
  fromStation' <- B.runInReplica $ CQS.findById ticketBooking.fromStationId >>= fromMaybeM (StationNotFound $ "StationId:" +|| ticketBooking.fromStationId.getId ||+ "")
  toStation' <- B.runInReplica $ CQS.findById ticketBooking.toStationId >>= fromMaybeM (StationNotFound $ "StationId:" +|| ticketBooking.toStationId.getId ||+ "")
  city <- CQMOC.findById fromStation'.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound fromStation'.merchantOperatingCityId.getId)
  pOrgId <- ticketBooking.partnerOrgId & fromMaybeM (InternalError $ "PartnerOrgId is missing for ticketBookingId:" +|| ticketBookingId.getId ||+ "")

  fromStation <- mkPOrgStationAPIRes fromStation' pOrgId
  toStation <- mkPOrgStationAPIRes toStation' pOrgId

  void $ bppStatusSync fromStation'.merchantId pOrgId city ticketBooking

  pure $ ShareTicketInfoResp {returnType = ticketBooking._type, partnerOrgTransactionId = ticketBooking.partnerOrgTransactionId, ..}
  where
    mkTicketAPI DFT.FRFSTicket {..} = FRFSTypes.FRFSTicketAPI {..}

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
            CQBC.findByMerchantIdDomainAndVehicle merchantId (show Spec.FRFS) DBC.METRO
              >>= fromMaybeM (BecknConfigNotFound $ "MerchantId:" +|| merchantId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| DBC.METRO ||+ "")
        void $ CallFRFSBPP.callBPPStatus ticketBooking bapConfig city merchantId
