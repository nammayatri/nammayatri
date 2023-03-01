{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Driver
  ( DriverInformationRes (..),
    ListDriverRes (..),
    DriverEntityRes (..),
    OnboardDriverReq (..),
    OnboardDriverRes (..),
    CreatePerson (..),
    CreateVehicle (..),
    UpdateDriverReq (..),
    UpdateDriverRes,
    GetNearbySearchRequestsRes (..),
    DriverOfferReq (..),
    DriverRespondReq (..),
    DriverStatsRes (..),
    getInformation,
    setActivity,
    listDriver,
    changeDriverEnableState,
    createDriver,
    deleteDriver,
    updateDriver,
    getNearbySearchRequests,
    offerQuote,
    respondQuote,
    offerQuoteLockKey,
    getStats,
  )
where

import Data.OpenApi (ToSchema)
import Data.Time (Day)
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.DriverQuote as DDrQuote
import qualified Domain.Types.FareParameters as Fare
import Domain.Types.FarePolicy.FarePolicy (ExtraFee)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person (Person, PersonAPIEntity)
import qualified Domain.Types.Person as SP
import qualified Domain.Types.SearchRequest as DSReq
import Domain.Types.SearchRequestForDriver
import Domain.Types.Vehicle (VehicleAPIEntity)
import qualified Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle as Veh
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Encryption
import Kernel.External.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.SMS.MyValueFirst.Flow as SF
import qualified Kernel.External.SMS.MyValueFirst.Types as SMS
import Kernel.Prelude (NominalDiffTime)
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import SharedLogic.CallBAP (sendDriverOffer)
import SharedLogic.DriverPool as DP
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as QDriverInformation
import Storage.CachedQueries.FarePolicy.FarePolicy (findByMerchantIdAndVariant)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverLocation as QDriverLocation
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Auth as Auth
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

data DriverInformationRes = DriverInformationRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    verified :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    organization :: DM.MerchantAPIEntity,
    language :: Maybe Maps.Language
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ListDriverRes = ListDriverRes
  {list :: [DriverEntityRes]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverEntityRes = DriverEntityRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    verified :: Bool,
    registeredAt :: UTCTime,
    language :: Maybe Maps.Language
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Create Person request and response
data OnboardDriverReq = OnboardDriverReq
  { person :: CreatePerson,
    vehicle :: CreateVehicle
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateOnboardDriverReq :: Validate OnboardDriverReq
validateOnboardDriverReq OnboardDriverReq {..} =
  sequenceA_
    [ validateObject "person" person validateCreatePerson,
      validateObject "vehicle" vehicle validateCreateVehicle
    ]

data CreatePerson = CreatePerson
  { firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

validateCreatePerson :: Validate CreatePerson
validateCreatePerson CreatePerson {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

data CreateVehicle = CreateVehicle
  { category :: Veh.Category,
    model :: Text,
    variant :: Variant.Variant,
    color :: Text,
    registrationNo :: Text,
    capacity :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

validateCreateVehicle :: Validate CreateVehicle
validateCreateVehicle CreateVehicle {..} =
  sequenceA_
    [ validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit),
      validateField "model" model $
        NotEmpty `And` star P.latinOrSpace,
      validateField "color" color $ NotEmpty `And` P.name
    ]

newtype OnboardDriverRes = OnboardDriverRes
  {driver :: PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateDriverReq = UpdateDriverReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken,
    language :: Maybe Maps.Language,
    canDowngradeToSedan :: Maybe Bool,
    canDowngradeToHatchback :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateUpdateDriverReq :: Validate UpdateDriverReq
validateUpdateDriverReq UpdateDriverReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ InMaybe $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name
    ]

type UpdateDriverRes = DriverInformationRes

newtype GetNearbySearchRequestsRes = GetNearbySearchRequestsRes
  { searchRequestsForDriver :: [SearchRequestForDriverAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, PrettyShow)

data DriverOfferReq = DriverOfferReq
  { offeredFare :: Maybe Money,
    searchRequestId :: Id DSReq.SearchRequest
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data DriverRespondReq = DriverRespondReq
  { offeredFare :: Maybe Money,
    searchRequestId :: Id DSReq.SearchRequest,
    response :: SearchRequestForDriverResponse
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data DriverStatsRes = DriverStatsRes
  { totalRidesOfDay :: Int,
    totalEarningsOfDay :: Money
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

createDriver ::
  ( HasCacheConfig r,
    HasFlowEnv m r ["inviteSmsTemplate" ::: Text, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  OnboardDriverReq ->
  m OnboardDriverRes
createDriver admin req = do
  let merchantId = admin.merchantId
  runRequestValidation validateOnboardDriverReq req
  let personEntity = req.person
  mobileNumberHash <- getDbHash personEntity.mobileNumber
  duplicateCheck
    (QVehicle.findByRegistrationNo req.vehicle.registrationNo)
    "Vehicle with this registration number already exists."
  duplicateCheck
    (QPerson.findByMobileNumber personEntity.mobileCountryCode mobileNumberHash)
    "Person with this mobile number already exists"
  person <- buildDriver req.person merchantId
  vehicle <- buildVehicle req.vehicle person.id merchantId
  Esq.runTransaction $ do
    QPerson.create person
    QDFS.create $ makeIdleDriverFlowStatus person
    createDriverDetails person.id admin.id
    QVehicle.create vehicle
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> createDriver : ") (show person.id)
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  decPerson <- decrypt person
  let mobNum = personEntity.mobileNumber
      mobCounCode = personEntity.mobileCountryCode
  smsCfg <- asks (.smsCfg)
  inviteSmsTemplate <- asks (.inviteSmsTemplate)
  sendInviteSms smsCfg inviteSmsTemplate (mobCounCode <> mobNum) (org.name)
    >>= SF.checkSmsResult
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ OnboardDriverRes personAPIEntity
  where
    duplicateCheck cond err = whenM (isJust <$> cond) $ throwError $ InvalidRequest err

    makeIdleDriverFlowStatus person =
      DDFS.DriverFlowStatus
        { personId = person.id,
          flowStatus = DDFS.IDLE,
          updatedAt = person.updatedAt
        }

createDriverDetails :: Id SP.Person -> Id SP.Person -> Esq.SqlDB ()
createDriverDetails personId adminId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            adminId = Just adminId,
            active = False,
            onRide = False,
            enabled = True,
            blocked = False,
            verified = False,
            referralCode = Nothing,
            lastEnabledOn = Just now,
            createdAt = now,
            updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

getInformation ::
  ( HasCacheConfig r,
    Redis.HedisFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  m DriverInformationRes
getInformation personId = do
  let driverId = cast personId
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  let merchantId = person.merchantId
  organization <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  pure $ makeDriverInformationRes driverEntity organization

setActivity :: (CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> Bool -> m APISuccess.APISuccess
setActivity personId isActive = do
  _ <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  when isActive $ do
    driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
    unless driverInfo.enabled $ throwError DriverAccountDisabled
    unless (not driverInfo.blocked) $ throwError DriverAccountBlocked
  QDriverInformation.updateActivity driverId isActive
  driverStatus <- QDFS.getStatus personId >>= fromMaybeM (PersonNotFound personId.getId)
  logInfo $ "driverStatus " <> show driverStatus
  unless (driverStatus `notElem` [DDFS.IDLE, DDFS.ACTIVE]) $ do
    if isActive
      then Esq.runTransaction $ QDFS.updateStatus personId DDFS.ACTIVE
      else Esq.runTransaction $ QDFS.updateStatus personId DDFS.IDLE
  pure APISuccess.Success

listDriver :: (EsqDBReplicaFlow m r, EncFlow m r) => SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> m ListDriverRes
listDriver admin mbSearchString mbLimit mbOffset = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  personList <- Esq.runInReplica $ QDriverInformation.findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset admin.merchantId
  respPersonList <- traverse buildDriverEntityRes personList
  return $ ListDriverRes respPersonList

buildDriverEntityRes :: (EsqDBReplicaFlow m r, EncFlow m r) => (SP.Person, DriverInformation) -> m DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  vehicleMB <- Esq.runInReplica $ QVehicle.findById person.id
  decMobNum <- mapM decrypt person.mobileNumber
  return $
    DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        rating = round <$> person.rating,
        linkedVehicle = SV.makeVehicleAPIEntity <$> vehicleMB,
        active = driverInfo.active,
        onRide = driverInfo.onRide,
        enabled = driverInfo.enabled,
        blocked = driverInfo.blocked,
        verified = driverInfo.verified,
        registeredAt = person.createdAt,
        language = person.language
      }

changeDriverEnableState ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r
  ) =>
  SP.Person ->
  Id SP.Person ->
  Bool ->
  m APISuccess
changeDriverEnableState admin personId isEnabled = do
  person <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (person.merchantId == admin.merchantId) $ throwError Unauthorized
  QDriverInformation.updateEnabledState driverId isEnabled
  unless isEnabled $ QDriverInformation.updateActivity driverId False
  unless isEnabled $ do
    Notify.notifyDriver person.merchantId FCM.ACCOUNT_DISABLED notificationTitle notificationMessage person.id person.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> changeDriverEnableState : ") (show (driverId, isEnabled))
  return Success
  where
    driverId = cast personId
    notificationTitle = "Account is disabled."
    notificationMessage = "Your account has been disabled. Contact support for more info."

deleteDriver :: (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r) => SP.Person -> Id SP.Person -> m APISuccess
deleteDriver admin driverId = do
  driver <-
    QPerson.findById driverId
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.merchantId == admin.merchantId || driver.role == SP.DRIVER) $ throwError Unauthorized
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession driverId
  QDriverInformation.deleteById (cast driverId)
  Esq.runTransaction $ do
    QDriverStats.deleteById (cast driverId)
    QR.deleteByPersonId driverId
    QVehicle.deleteById driverId
    QDriverLocation.deleteById driverId
    QPerson.deleteById driverId

  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteDriver : ") (show driverId)
  return Success

updateDriver ::
  ( HasCacheConfig r,
    Redis.HedisFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  UpdateDriverReq ->
  m UpdateDriverRes
updateDriver personId req = do
  runRequestValidation validateUpdateDriverReq req
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let updPerson =
        person{firstName = fromMaybe person.firstName req.firstName,
               middleName = req.middleName <|> person.middleName,
               lastName = req.lastName <|> person.lastName,
               deviceToken = req.deviceToken <|> person.deviceToken,
               language = req.language <|> person.language
              }

  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  Esq.runTransaction $
    QPerson.updatePersonRec personId updPerson
  driverEntity <- buildDriverEntityRes (updPerson, driverInfo)
  let merchantId = person.merchantId
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return $ makeDriverInformationRes driverEntity org

sendInviteSms ::
  ( MonadFlow m,
    CoreMetrics m,
    MonadReader r m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  Text ->
  m SMS.SubmitSmsRes
sendInviteSms smsCfg inviteTemplate phoneNumber orgName = do
  let url = smsCfg.url
  let smsCred = smsCfg.credConfig
  let sender = smsCfg.sender
  SF.submitSms
    url
    SMS.SubmitSms
      { SMS.username = smsCred.username,
        SMS.password = smsCred.password,
        SMS.from = sender,
        SMS.to = phoneNumber,
        SMS.text = SF.constructInviteSms orgName inviteTemplate
      }

buildDriver :: (EncFlow m r) => CreatePerson -> Id DM.Merchant -> m SP.Person
buildDriver req merchantId = do
  pid <- generateGUID
  now <- getCurrentTime
  mobileNumber <- Just <$> encrypt req.mobileNumber
  return
    SP.Person
      { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
        SP.id = pid,
        SP.firstName = req.firstName,
        SP.middleName = req.middleName,
        SP.lastName = req.lastName,
        SP.role = SP.DRIVER,
        SP.gender = SP.UNKNOWN,
        SP.email = Nothing,
        SP.passwordHash = Nothing,
        SP.identifier = Nothing,
        SP.identifierType = SP.MOBILENUMBER,
        SP.unencryptedMobileNumber = Just req.mobileNumber,
        SP.mobileNumber = mobileNumber,
        SP.mobileCountryCode = Just req.mobileCountryCode,
        SP.isNew = True,
        SP.rating = Nothing,
        SP.deviceToken = Nothing,
        SP.language = Nothing,
        SP.merchantId = merchantId,
        SP.description = Nothing,
        SP.createdAt = now,
        SP.updatedAt = now,
        SP.clientVersion = Nothing,
        SP.whatsappNotificationEnrollStatus = Nothing,
        SP.bundleVersion = Nothing
      }

buildVehicle :: MonadFlow m => CreateVehicle -> Id SP.Person -> Id DM.Merchant -> m SV.Vehicle
buildVehicle req personId merchantId = do
  now <- getCurrentTime
  return $
    SV.Vehicle
      { SV.driverId = personId,
        SV.capacity = Just req.capacity,
        SV.category = Just req.category,
        SV.make = Nothing,
        SV.model = req.model,
        SV.size = Nothing,
        SV.merchantId = merchantId,
        SV.variant = req.variant,
        SV.color = req.color,
        SV.energyType = Nothing,
        SV.registrationNo = req.registrationNo,
        SV.registrationCategory = Nothing,
        SV.vehicleClass = "3WT",
        SV.createdAt = now,
        SV.updatedAt = now
      }

makeDriverInformationRes :: DriverEntityRes -> DM.Merchant -> DriverInformationRes
makeDriverInformationRes DriverEntityRes {..} org =
  DriverInformationRes
    { organization = DM.makeMerchantAPIEntity org,
      ..
    }

getNearbySearchRequests ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    DP.HasDriverPoolConfig r,
    Redis.HedisFlow m r,
    HasCacheConfig r
  ) =>
  Id SP.Person ->
  m GetNearbySearchRequestsRes
getNearbySearchRequests driverId = do
  person <- runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  nearbyReqs <- runInReplica $ QSRD.findByDriver driverId
  cancellationRatio <- DP.getLatestCancellationRatio person.merchantId (cast driverId)
  searchRequestForDriverAPIEntity <- mapM (buildSearchRequestForDriverAPIEntity cancellationRatio) nearbyReqs
  return $ GetNearbySearchRequestsRes searchRequestForDriverAPIEntity
  where
    buildSearchRequestForDriverAPIEntity cancellationRatio nearbyReq = do
      let sId = nearbyReq.searchRequestId
      searchRequest <- runInReplica $ QSReq.findById sId >>= fromMaybeM (SearchRequestNotFound sId.getId)
      cancellationScoreRelatedConfig <- asks (.cancellationScoreRelatedConfig)
      popupDelaySeconds <- DP.getPopupDelay searchRequest.providerId (cast driverId) cancellationRatio cancellationScoreRelatedConfig
      return $ makeSearchRequestForDriverAPIEntity nearbyReq searchRequest popupDelaySeconds

isAllowedExtraFee :: ExtraFee -> Money -> Bool
isAllowedExtraFee extraFee val = extraFee.minFee <= val && val <= extraFee.maxFee

offerQuoteLockKey :: Id Person -> Text
offerQuoteLockKey driverId = "Driver:OfferQuote:DriverId-" <> driverId.getId

-- DEPRECATED
offerQuote ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasPrettyLogger m r,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "coreVersion" r Text,
    HasField "nwAddress" r BaseUrl,
    HasField "driverUnlockDelay" r Seconds,
    DP.HasDriverPoolConfig r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  Id SP.Person ->
  DriverOfferReq ->
  m APISuccess
offerQuote driverId DriverOfferReq {..} = do
  let response = Accept
  respondQuote driverId DriverRespondReq {..}

respondQuote ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasPrettyLogger m r,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "coreVersion" r Text,
    HasField "nwAddress" r BaseUrl,
    HasField "driverUnlockDelay" r Seconds,
    DP.HasDriverPoolConfig r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  Id SP.Person ->
  DriverRespondReq ->
  m APISuccess
respondQuote driverId req = do
  Redis.whenWithLockRedis (offerQuoteLockKey driverId) 60 $ do
    sReq <- QSReq.findById req.searchRequestId >>= fromMaybeM (SearchRequestNotFound req.searchRequestId.getId)
    now <- getCurrentTime
    when (sReq.validTill < now) $ throwError SearchRequestExpired
    let mbOfferedFare = req.offeredFare
    organization <- CQM.findById sReq.providerId >>= fromMaybeM (MerchantDoesNotExist sReq.providerId.getId)
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    driverInfo <- QDriverInformation.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
    when driverInfo.onRide $ throwError DriverOnRide
    DP.removeSearchReqIdFromMap sReq.providerId driverId sReq.id
    sReqFD <-
      QSRD.findByDriverAndSearchReq driverId sReq.id
        >>= fromMaybeM NoSearchRequestForDriver
    case req.response of
      Pulled -> throwError UnexpectedResponseValue
      Accept -> do
        logDebug $ "offered fare: " <> show req.offeredFare
        whenM thereAreActiveQuotes (throwError FoundActiveQuotes)
        when (sReqFD.response == Just Reject) (throwError QuoteAlreadyRejected)
        quoteLimit <- getQuoteLimit sReq.estimatedDistance
        quoteCount <- runInReplica $ QDrQt.countAllByRequestId sReq.id
        when (quoteCount >= quoteLimit) (throwError QuoteAlreadyRejected)
        farePolicy <- findByMerchantIdAndVariant organization.id sReqFD.vehicleVariant (Just sReq.estimatedDistance) >>= fromMaybeM NoFarePolicy
        whenJust mbOfferedFare $ \off ->
          unless (isAllowedExtraFee farePolicy.driverExtraFee off) $
            throwError $ NotAllowedExtraFee $ show off
        fareParams <- calculateFare organization.id farePolicy sReq.estimatedDistance sReqFD.startTime mbOfferedFare sReqFD.customerExtraFee
        driverQuote <- buildDriverQuote driver sReq sReqFD fareParams
        Esq.runTransaction $ do
          QDrQt.create driverQuote
          QSRD.updateDriverResponse sReqFD.id req.response
          QDFS.updateStatus sReqFD.driverId DDFS.OFFERED_QUOTE {quoteId = driverQuote.id, validTill = driverQuote.validTill}
        DP.incrementQuoteAcceptedCount sReq.providerId driverId
        -- Adding +1 in quoteCount because one more quote added above (QDrQt.create driverQuote)
        when ((quoteCount + 1) >= quoteLimit || sReq.autoAssignEnabled) $ sendRemoveRideRequestNotification organization.id driverQuote
        sendDriverOffer organization sReq driverQuote
      Reject -> do
        Esq.runTransaction $ do
          QSRD.updateDriverResponse sReqFD.id req.response
  pure Success
  where
    buildDriverQuote ::
      (MonadFlow m, MonadReader r m, HasField "driverQuoteExpirationSeconds" r NominalDiffTime) =>
      SP.Person ->
      DSReq.SearchRequest ->
      SearchRequestForDriver ->
      Fare.FareParameters ->
      m DDrQuote.DriverQuote
    buildDriverQuote driver s sd fareParams = do
      guid <- generateGUID
      now <- getCurrentTime
      driverQuoteExpirationSeconds <- asks (.driverQuoteExpirationSeconds)
      let estimatedFare = fareSum fareParams
      pure
        DDrQuote.DriverQuote
          { id = guid,
            status = DDrQuote.Active,
            searchRequestId = s.id,
            driverId,
            driverName = driver.firstName,
            driverRating = driver.rating,
            vehicleVariant = sd.vehicleVariant,
            searchRequestForDriverId = Just sd.id,
            distance = s.estimatedDistance,
            distanceToPickup = sd.actualDistanceToPickup,
            durationToPickup = sd.durationToPickup,
            createdAt = now,
            updatedAt = now,
            validTill = addUTCTime driverQuoteExpirationSeconds now,
            estimatedFare,
            fareParams
          }
    thereAreActiveQuotes = do
      driverUnlockDelay <- asks (.driverUnlockDelay)
      activeQuotes <- QDrQt.findActiveQuotesByDriverId driverId driverUnlockDelay
      logPretty DEBUG ("active quotes for driverId = " <> driverId.getId) activeQuotes
      pure $ not $ null activeQuotes
    getQuoteLimit dist = do
      driverPoolCfg <- DP.getDriverPoolConfig dist
      pure $ fromIntegral driverPoolCfg.driverQuoteLimit
    sendRemoveRideRequestNotification orgId driverQuote = do
      driverSearchReqs <- QSRD.findAllActiveWithoutRespByRequestId req.searchRequestId
      for_ driverSearchReqs $ \driverReq -> do
        DP.decrementTotalQuotesCount orgId (cast driverReq.driverId) driverReq.searchRequestId
        DP.removeSearchReqIdFromMap orgId driverReq.driverId driverReq.searchRequestId
        Esq.runTransaction $ do
          QSRD.updateDriverResponse driverReq.id Pulled
        driver_ <- runInReplica $ QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
        Notify.notifyDriverClearedFare orgId driverReq.driverId driverReq.searchRequestId driverQuote.estimatedFare driver_.deviceToken

getStats ::
  (EsqDBReplicaFlow m r, EncFlow m r) =>
  Id SP.Person ->
  Day ->
  m DriverStatsRes
getStats driverId date = do
  rides <- runInReplica $ QRide.getRidesForDate driverId date
  return $
    DriverStatsRes
      { totalRidesOfDay = length rides,
        totalEarningsOfDay = sum . catMaybes $ rides <&> (.fare)
      }
