{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
    getInformation,
    setActivity,
    setRental,
    listDriver,
    changeDriverEnableState,
    createDriver,
    deleteDriver,
    updateDriver,
  )
where

import Control.Applicative ((<|>))
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as SV
import GHC.Records.Extra
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.SMS.MyValueFirst.Flow as SF
import qualified Kernel.External.SMS.MyValueFirst.Types as SMS
import Kernel.Prelude
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common (fromMaybeM, logTagInfo, throwError, (:::))
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import SharedLogic.TransporterConfig
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverLocation as QDriverLocation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Auth as Auth
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

data DriverInformationRes = DriverInformationRes
  { id :: Id SP.Person,
    adminId :: Maybe (Id SP.Person),
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: SV.VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    optForRental :: Bool,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    organization :: DM.MerchantAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ListDriverRes = ListDriverRes
  {list :: [DriverEntityRes]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverEntityRes = DriverEntityRes
  { id :: Id SP.Person,
    adminId :: Maybe (Id SP.Person),
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: SV.VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    optForRental :: Bool,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    registeredAt :: UTCTime
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
  { category :: SV.Category,
    model :: Text,
    variant :: SV.Variant,
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
  {driver :: SP.PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateDriverReq = UpdateDriverReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken,
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

createDriver ::
  forall m r.
  ( HasCacheConfig r,
    HasFlowEnv m r ["inviteSmsTemplate" ::: Text, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
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
    (QVehicle.findByRegistrationNo req.vehicle.registrationNo (Proxy @m))
    "Vehicle with this registration number already exists."
  duplicateCheck
    (QPerson.findByMobileNumber personEntity.mobileCountryCode mobileNumberHash (Proxy @m))
    "Person with this mobile number already exists"
  person <- buildDriver req.person merchantId
  vehicle <- buildVehicle req.vehicle person.id merchantId
  Esq.runTransaction $ do
    QPerson.create @m person
    createDriverDetails person.id admin.id
    QVehicle.create vehicle
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> createDriver : ") (show person.id)
  org <-
    QMerchant.findById merchantId
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

createDriverDetails :: Id SP.Person -> Id SP.Person -> Esq.SqlDB m ()
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
            optForRental = False,
            createdAt = now,
            updatedAt = now,
            canDowngradeToSedan = False,
            canDowngradeToHatchback = False
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

getInformation ::
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  m DriverInformationRes
getInformation personId = do
  _ <- runInReplica $ QPerson.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  person <- runInReplica $ QPerson.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- runInReplica $ QDriverInformation.findById driverId (Proxy @m) >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  let merchantId = person.merchantId
  organization <-
    QMerchant.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  pure $ makeDriverInformationRes driverEntity organization

setActivity ::
  forall m r.
  ( EsqDBFlow m r
  ) =>
  Id SP.Person ->
  Bool ->
  m APISuccess.APISuccess
setActivity personId isActive = do
  _ <- QPerson.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  when isActive $ do
    driverInfo <- QDriverInformation.findById driverId (Proxy @m) >>= fromMaybeM DriverInfoNotFound
    unless driverInfo.enabled $ throwError DriverAccountDisabled
    unless (not driverInfo.blocked) $ throwError DriverAccountBlocked
  Esq.runTransaction $
    QDriverInformation.updateActivity @m driverId isActive
  pure APISuccess.Success

setRental ::
  forall m r.
  ( EsqDBFlow m r
  ) =>
  Id SP.Person ->
  Bool ->
  m APISuccess.APISuccess
setRental personId isRental = do
  _ <- QPerson.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  Esq.runTransaction $
    QDriverInformation.updateRental @m driverId isRental
  pure APISuccess.Success

listDriver ::
  forall m r.
  ( EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  SP.Person ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListDriverRes
listDriver admin mbSearchString mbLimit mbOffset = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  personList <- runInReplica $ QDriverInformation.findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset admin.merchantId (Proxy @m)
  respPersonList <- traverse buildDriverEntityRes personList
  return $ ListDriverRes respPersonList

buildDriverEntityRes :: forall m r. (EsqDBReplicaFlow m r, EncFlow m r) => (SP.Person, DriverInformation) -> m DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  vehicle <- runInReplica $ QVehicle.findById person.id (Proxy @m) >>= fromMaybeM (VehicleNotFound person.id.getId)
  decMobNum <- mapM decrypt person.mobileNumber
  return $
    DriverEntityRes
      { id = person.id,
        adminId = driverInfo.adminId,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        rating = round <$> person.rating,
        linkedVehicle = SV.makeVehicleAPIEntity vehicle,
        active = driverInfo.active,
        onRide = driverInfo.onRide,
        enabled = driverInfo.enabled,
        blocked = driverInfo.blocked,
        optForRental = driverInfo.optForRental,
        registeredAt = person.createdAt,
        canDowngradeToSedan = driverInfo.canDowngradeToSedan,
        canDowngradeToHatchback = driverInfo.canDowngradeToHatchback
      }

changeDriverEnableState ::
  forall m r.
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
    QPerson.findById personId (Proxy @m)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (person.merchantId == admin.merchantId) $ throwError Unauthorized
  Esq.runTransaction $ do
    QDriverInformation.updateEnabledState @m driverId isEnabled
    unless isEnabled $ QDriverInformation.updateActivity driverId False
  unless isEnabled $ do
    fcmConfig <- findFCMConfigByMerchantId person.merchantId
    Notify.notifyDriver fcmConfig FCM.ACCOUNT_DISABLED notificationTitle notificationMessage person.id person.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> changeDriverEnableState : ") (show (driverId, isEnabled))
  return Success
  where
    driverId = cast personId
    notificationTitle = "Account is disabled."
    notificationMessage = "Your account has been disabled. Contact support for more info."

deleteDriver ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  SP.Person ->
  Id SP.Person ->
  m APISuccess
deleteDriver admin driverId = do
  driver <-
    QPerson.findById driverId (Proxy @m)
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.merchantId == admin.merchantId || driver.role == SP.DRIVER) $ throwError Unauthorized
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession driverId
  Esq.runTransaction $ do
    QDriverInformation.deleteById @m (cast driverId)
    QDriverStats.deleteById (cast driverId)
    QR.deleteByPersonId driverId
    QVehicle.deleteById driverId
    QDriverLocation.deleteById driverId
    QPerson.deleteById driverId
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteDriver : ") (show driverId)
  return Success

updateDriver ::
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  UpdateDriverReq ->
  m UpdateDriverRes
updateDriver personId req = do
  runRequestValidation validateUpdateDriverReq req
  person <- QPerson.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  let updPerson =
        person{firstName = fromMaybe person.firstName req.firstName,
               middleName = req.middleName <|> person.middleName,
               lastName = req.lastName <|> person.lastName,
               deviceToken = req.deviceToken <|> person.deviceToken
              }
  vehicle <- QVehicle.findById personId (Proxy @m) >>= fromMaybeM (VehicleNotFound personId.getId)
  driverInfo <- QDriverInformation.findById (cast personId) (Proxy @m) >>= fromMaybeM DriverInfoNotFound
  let wantToDowngrade = req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True
  when (vehicle.variant == SV.SEDAN && req.canDowngradeToSedan == Just True) $
    throwError $ InvalidRequest "Driver with sedan can't downgrade to sedan"
  when (vehicle.variant == SV.HATCHBACK && wantToDowngrade) $
    throwError $ InvalidRequest "Driver with hatchback can't downgrade"
  let updDriverInfo =
        driverInfo{canDowngradeToSedan = fromMaybe driverInfo.canDowngradeToSedan req.canDowngradeToSedan,
                   canDowngradeToHatchback = fromMaybe driverInfo.canDowngradeToHatchback req.canDowngradeToHatchback
                  }
  Esq.runTransaction $ do
    QPerson.updatePersonRec @m personId updPerson
    QDriverInformation.updateDowngradingOptions (cast person.id) updDriverInfo.canDowngradeToSedan updDriverInfo.canDowngradeToHatchback
  driverEntity <- buildDriverEntityRes (updPerson, updDriverInfo)
  let merchantId = person.merchantId
  org <-
    QMerchant.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return $ makeDriverInformationRes driverEntity org

sendInviteSms ::
  ( MonadFlow m,
    CoreMetrics m
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
        SP.merchantId = merchantId,
        SP.description = Nothing,
        SP.createdAt = now,
        SP.updatedAt = now,
        SP.bundleVersion = Nothing,
        SP.clientVersion = Nothing
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
