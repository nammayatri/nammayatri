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
    offerQuoteLockKey,
    getStats,
  )
where

import Beckn.External.Encryption
import Beckn.External.FCM.Types (FCMRecipientToken)
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.Maps as Maps
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Prelude (NominalDiffTime)
import Beckn.Sms.Config (SmsConfig)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty (PrettyShow)
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Time (Day)
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.DriverQuote as DDrQuote
import qualified Domain.Types.FareParams as Fare
import Domain.Types.FarePolicy (ExtraFee)
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
import SharedLogic.CallBAP (sendDriverOffer)
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import Storage.CachedQueries.FarePolicy (findByMerchantIdAndVariant)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverInformation as QDrInfo
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Auth (authTokenCacheKey)
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

data DriverStatsRes = DriverStatsRes
  { totalRidesOfDay :: Int,
    totalEarningsOfDay :: Money
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

createDriver ::
  ( HasCacheConfig r,
    HasFlowEnv m r ["inviteSmsTemplate" ::: Text, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  OnboardDriverReq ->
  m OnboardDriverRes
createDriver admin req = do
  let Just merchantId = admin.merchantId
  runRequestValidation validateOnboardDriverReq req
  let personEntity = req.person
  duplicateCheck
    (QVehicle.findByRegistrationNo req.vehicle.registrationNo)
    "Vehicle with this registration number already exists."
  duplicateCheck
    (QPerson.findByMobileNumber personEntity.mobileCountryCode personEntity.mobileNumber)
    "Person with this mobile number already exists"
  person <- buildDriver req.person merchantId
  vehicle <- buildVehicle req.vehicle person.id merchantId
  Esq.runTransaction $ do
    QPerson.create person
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
            verified = False,
            referralCode = Nothing,
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
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  m DriverInformationRes
getInformation personId = do
  let driverId = cast personId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  merchantId <- person.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
  organization <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  pure $ makeDriverInformationRes driverEntity organization

setActivity :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SP.Person -> Bool -> m APISuccess.APISuccess
setActivity personId isActive = do
  _ <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  when isActive $ do
    driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
    unless driverInfo.enabled $ throwError DriverAccountDisabled
  Esq.runTransaction $
    QDriverInformation.updateActivity driverId isActive
  pure APISuccess.Success

listDriver :: (EsqDBReplicaFlow m r, EncFlow m r) => SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> m ListDriverRes
listDriver admin mbSearchString mbLimit mbOffset = do
  let Just merchantId = admin.merchantId
  personList <- QDriverInformation.findAllWithLimitOffsetByMerchantId mbSearchString mbLimit mbOffset merchantId
  respPersonList <- traverse buildDriverEntityRes personList
  return $ ListDriverRes respPersonList

buildDriverEntityRes :: (Esq.Transactionable m, EncFlow m r) => (SP.Person, DriverInformation) -> m DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  vehicleMB <- QVehicle.findById person.id
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
        verified = driverInfo.verified,
        registeredAt = person.createdAt,
        language = person.language
      }

changeDriverEnableState ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  Id SP.Person ->
  Bool ->
  m APISuccess
changeDriverEnableState admin personId isEnabled = do
  let Just merchantId = admin.merchantId
  person <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (person.merchantId == Just merchantId) $ throwError Unauthorized
  Esq.runTransaction $ do
    QDriverInformation.updateEnabledState driverId isEnabled
    unless isEnabled $ QDriverInformation.updateActivity driverId False
  unless isEnabled $
    Notify.notifyDriver FCM.ACCOUNT_DISABLED notificationTitle notificationMessage person.id person.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> changeDriverEnableState : ") (show (driverId, isEnabled))
  return Success
  where
    driverId = cast personId
    notificationTitle = "Account is disabled."
    notificationMessage = "Your account has been disabled. Contact support for more info."

deleteDriver :: (EsqDBFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r) => SP.Person -> Id SP.Person -> m APISuccess
deleteDriver admin driverId = do
  let Just merchantId = admin.merchantId
  driver <-
    QPerson.findById driverId
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.merchantId == Just merchantId || driver.role == SP.DRIVER) $ throwError Unauthorized
  clearDriverSession driverId
  Esq.runTransaction $ do
    QDriverInformation.deleteById (cast driverId)
    QDriverStats.deleteById (cast driverId)
    QR.deleteByPersonId driverId
    QVehicle.deleteById driverId
    QPerson.deleteById driverId

  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteDriver : ") (show driverId)
  return Success
  where
    clearDriverSession personId = do
      regTokens <- QR.findAllByPersonId personId
      for_ regTokens $ \regToken ->
        void $ Redis.del $ authTokenCacheKey regToken.token

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
  merchantId <- person.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
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
        SP.mobileNumber = mobileNumber,
        SP.mobileCountryCode = Just req.mobileCountryCode,
        SP.isNew = True,
        SP.rating = Nothing,
        SP.deviceToken = Nothing,
        SP.language = Nothing,
        SP.merchantId = Just merchantId,
        SP.description = Nothing,
        SP.createdAt = now,
        SP.updatedAt = now
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
        SV.createdAt = now,
        SV.updatedAt = now
      }

makeDriverInformationRes :: DriverEntityRes -> DM.Merchant -> DriverInformationRes
makeDriverInformationRes DriverEntityRes {..} org =
  DriverInformationRes
    { organization = DM.makeMerchantAPIEntity org,
      ..
    }

getNearbySearchRequests :: (EsqDBReplicaFlow m r) => Id SP.Person -> m GetNearbySearchRequestsRes
getNearbySearchRequests driverId = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  _ <- person.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
  nearbyReqs <- QSRD.findByDriver driverId
  searchRequestForDriverAPIEntity <- mapM buildSearchRequestForDriverAPIEntity nearbyReqs
  return $ GetNearbySearchRequestsRes searchRequestForDriverAPIEntity
  where
    buildSearchRequestForDriverAPIEntity nearbyReq = do
      let sId = nearbyReq.searchRequestId
      searchRequest <- QSReq.findById sId >>= fromMaybeM (SearchRequestNotFound sId.getId)
      return $ makeSearchRequestForDriverAPIEntity nearbyReq searchRequest

isAllowedExtraFee :: ExtraFee -> Money -> Bool
isAllowedExtraFee extraFee val = extraFee.minFee <= val && val <= extraFee.maxFee

offerQuoteLockKey :: Id Person -> Text
offerQuoteLockKey driverId = "Driver:OfferQuote:DriverId-" <> driverId.getId

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
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  Id SP.Person ->
  DriverOfferReq ->
  m APISuccess
offerQuote driverId req = do
  whenM (Redis.tryLockRedis (offerQuoteLockKey driverId) 60) $ do
    logDebug $ "offered fare: " <> show req.offeredFare
    sReq <- QSReq.findById req.searchRequestId >>= fromMaybeM (SearchRequestNotFound req.searchRequestId.getId)
    now <- getCurrentTime
    when (sReq.validTill < now) $ throwError SearchRequestExpired
    let mbOfferedFare = req.offeredFare
    organization <- CQM.findById sReq.providerId >>= fromMaybeM (MerchantDoesNotExist sReq.providerId.getId)
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    whenM thereAreActiveQuotes (throwError FoundActiveQuotes)
    driverInfo <- QDrInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
    when driverInfo.onRide $ throwError DriverOnRide
    sReqFD <-
      QSRD.findByDriverAndSearchReq driverId sReq.id
        >>= fromMaybeM NoSearchRequestForDriver
    farePolicy <- findByMerchantIdAndVariant organization.id sReqFD.vehicleVariant >>= fromMaybeM NoFarePolicy
    whenJust mbOfferedFare $ \off ->
      unless (isAllowedExtraFee farePolicy.driverExtraFee off) $
        throwError $ NotAllowedExtraFee $ show off
    fareParams <- calculateFare organization.id farePolicy sReq.estimatedDistance sReqFD.startTime mbOfferedFare
    driverQuote <- buildDriverQuote driver sReq sReqFD fareParams
    Esq.runTransaction $ QDrQt.create driverQuote
    sendDriverOffer organization sReq driverQuote
    Redis.unlockRedis $ offerQuoteLockKey driverId
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
            distance = s.estimatedDistance,
            distanceToPickup = sd.distanceToPickup,
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

getStats ::
  (EsqDBReplicaFlow m r, EncFlow m r) =>
  Id SP.Person ->
  Day ->
  m DriverStatsRes
getStats driverId date = do
  rides <- QRide.getRidesForDate driverId date
  return $
    DriverStatsRes
      { totalRidesOfDay = length rides,
        totalEarningsOfDay = sum . catMaybes $ rides <&> (.fare)
      }
