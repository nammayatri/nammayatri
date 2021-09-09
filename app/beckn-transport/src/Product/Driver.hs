module Product.Driver
  ( getInformation,
    setActivity,
    listDriver,
    linkVehicle,
    changeDriverEnableState,
    createDriver,
    deleteDriver,
    updateDriver,
  )
where

import App.Types
import qualified App.Types as App
import Beckn.External.Encryption (decrypt, encrypt)
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config (SmsConfig)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Validation
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Product.Registration as Registration
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Types.API.Driver as DriverAPI
import Types.Error
import Types.Metrics
import Types.Storage.DriverInformation (DriverInformation, DriverInformationT (createdAt))
import qualified Types.Storage.DriverInformation as DriverInfo
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Vehicle as SV
import Utils.Common (fromMaybeM, maskText, throwError, withFlowHandlerAPI)
import qualified Utils.Notifications as Notify

createDriver :: SP.Person -> DriverAPI.OnboardDriverReq -> FlowHandler DriverAPI.OnboardDriverRes
createDriver admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation DriverAPI.validateOnboardDriverReq req
  let personEntity = req.person
  validateVehicle req.vehicle
  person <- buildDriver req.person orgId
  vehicle <- buildVehicle req.vehicle orgId
  DB.runSqlDBTransaction $ do
    QPerson.create person
    createDriverDetails (person.id)
    QVehicle.create vehicle
    QPerson.updateVehicle person.id $ Just vehicle.id
  org <-
    QOrganization.findOrganizationById orgId
      >>= fromMaybeM OrgNotFound
  decPerson <- decrypt person
  let mobNum = personEntity.mobileNumber
      mobCounCode = personEntity.mobileCountryCode
  smsCfg <- smsCfg <$> ask
  inviteSmsTemplate <- inviteSmsTemplate <$> ask
  sendInviteSms smsCfg inviteSmsTemplate (mobCounCode <> mobNum) (org.name)
    >>= SF.checkRegistrationSmsResult
  return . DriverAPI.OnboardDriverRes $ SP.makePersonAPIEntity decPerson
  where
    validateVehicle preq =
      whenM (isJust <$> QVehicle.findByRegistrationNo preq.registrationNo) $
        throwError $ InvalidRequest "Vehicle with this registration number already exists."

createDriverDetails :: Id SP.Person -> DB.SqlDB ()
createDriverDetails personId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            active = False,
            onRide = False,
            enabled = True,
            createdAt = now,
            updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

getInformation :: Id SP.Person -> App.FlowHandler DriverAPI.DriverInformationRes
getInformation personId = withFlowHandlerAPI $ do
  _ <- Registration.checkPersonExists $ getId personId
  let driverId = cast personId
  person <- QPerson.findPersonById personId >>= fromMaybeM PersonNotFound
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  organization <-
    QOrganization.findOrganizationById orgId
      >>= fromMaybeM OrgNotFound
  pure $ makeDriverInformationRes driverEntity organization

setActivity :: Id SP.Person -> Bool -> App.FlowHandler APISuccess.APISuccess
setActivity personId isActive = withFlowHandlerAPI $ do
  _ <- QPerson.findPersonById personId >>= fromMaybeM PersonNotFound
  let driverId = cast personId
  when isActive $ do
    driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
    unless driverInfo.enabled $ throwError DriverAccountDisabled
  DB.runSqlDBTransaction $
    QDriverInformation.updateActivity driverId isActive
  pure APISuccess.Success

listDriver :: SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DriverAPI.ListDriverRes
listDriver admin mbSearchString mbLimit mbOffset = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  personList <- QDriverInformation.findAllWithLimitOffsetByOrgId mbSearchString mbLimit mbOffset orgId
  respPersonList <- traverse buildDriverEntityRes personList
  return $ DriverAPI.ListDriverRes respPersonList

buildDriverEntityRes :: (DBFlow m r, EncFlow m r) => (SP.Person, DriverInformation) -> m DriverAPI.DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  vehicle <- traverse QVehicle.findVehicleById (Id <$> person.udf1) >>= fromMaybeM VehicleNotFound . join
  decMobNum <- decrypt person.mobileNumber
  vehAPIEntity <- SV.buildVehicleAPIEntity vehicle
  return $
    DriverAPI.DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        maskedMobileNumber = maskText <$> decMobNum,
        rating = round <$> person.rating,
        linkedVehicle = vehAPIEntity,
        active = driverInfo.active,
        onRide = driverInfo.onRide,
        enabled = driverInfo.enabled,
        registeredAt = person.createdAt
      }

linkVehicle :: SP.Person -> Id SP.Person -> Id SV.Vehicle -> FlowHandler DriverAPI.LinkVehicleRes
linkVehicle admin personId vehicleId = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  person <-
    QPerson.findPersonById personId
      >>= fromMaybeM PersonDoesNotExist
  vehicle <-
    QVehicle.findVehicleById vehicleId
      >>= fromMaybeM VehicleDoesNotExist
  unless
    ( person.organizationId == Just orgId
        && vehicle.organizationId == orgId
    )
    (throwError Unauthorized)
  prevPerson <- QPerson.findByVehicleId vehicleId
  whenJust prevPerson $ \_ -> throwError VehicleAlreadyLinked
  QPerson.updateVehicleFlow personId $ Just vehicleId
  return APISuccess.Success

changeDriverEnableState :: SP.Person -> Id SP.Person -> Bool -> FlowHandler APISuccess
changeDriverEnableState admin personId isEnabled = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  person <-
    QPerson.findPersonById personId
      >>= fromMaybeM PersonDoesNotExist
  unless (person.organizationId == Just orgId) $ throwError Unauthorized
  DB.runSqlDBTransaction $ do
    QDriverInformation.updateEnabledState driverId isEnabled
    unless isEnabled $ QDriverInformation.updateActivity driverId False
  unless isEnabled $
    Notify.notifyDriver FCM.ACCOUNT_DISABLED notificationTitle notificationMessage person.id person.deviceToken
  return Success
  where
    driverId = cast personId
    notificationTitle = "Account is disabled."
    notificationMessage = "Your account has been disabled. Contact support for more info."

deleteDriver :: SP.Person -> Id SP.Person -> FlowHandler APISuccess
deleteDriver admin driverId = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  driver <-
    QPerson.findPersonById driverId
      >>= fromMaybeM PersonDoesNotExist
  unless (driver.organizationId == Just orgId || driver.role == SP.DRIVER) $ throwError Unauthorized
  DB.runSqlDBTransaction $ do
    QPerson.deleteById driverId
  return Success

updateDriver :: Id SP.Person -> DriverAPI.UpdateDriverReq -> FlowHandler DriverAPI.UpdateDriverRes
updateDriver personId req = withFlowHandlerAPI $ do
  runRequestValidation DriverAPI.validateUpdateDriverReq req
  person <- QPerson.findPersonById personId >>= fromMaybeM PersonNotFound
  let updPerson =
        person{firstName = req.firstName <|> person.firstName,
               middleName = req.middleName <|> person.middleName,
               lastName = req.lastName <|> person.lastName,
               deviceToken = req.deviceToken <|> person.deviceToken
              }
  DB.runSqlDB (QPerson.updatePersonRec personId updPerson)
  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  org <-
    QOrganization.findOrganizationById orgId
      >>= fromMaybeM OrgNotFound
  return $ makeDriverInformationRes driverEntity org

sendInviteSms ::
  ( DBFlow m r,
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

buildDriver :: (DBFlow m r, EncFlow m r) => DriverAPI.CreatePerson -> Id Org.Organization -> m SP.Person
buildDriver req orgId = do
  pid <- generateGUID
  now <- getCurrentTime
  mobileNumber <- encrypt (Just req.mobileNumber)
  return
    SP.Person
      { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
        SP.id = pid,
        SP.firstName = req.firstName,
        SP.middleName = req.middleName,
        SP.lastName = req.lastName,
        SP.fullName = Nothing,
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
        SP.udf1 = Nothing,
        SP.udf2 = Nothing,
        SP.organizationId = Just orgId,
        SP.description = Nothing,
        SP.createdAt = now,
        SP.updatedAt = now
      }

buildVehicle :: DBFlow m r => DriverAPI.CreateVehicle -> Id Org.Organization -> m SV.Vehicle
buildVehicle req orgId = do
  vid <- generateGUID
  now <- getCurrentTime
  return $
    SV.Vehicle
      { -- only these below will be updated in the vehicle table. if you want to add something extra please add in queries also
        SV.id = vid,
        SV.capacity = Just req.capacity,
        SV.category = Just req.category,
        SV.make = Nothing,
        SV.model = Just req.model,
        SV.size = Nothing,
        SV.organizationId = orgId,
        SV.variant = Just req.variant,
        SV.color = Just req.color,
        SV.energyType = Nothing,
        SV.registrationNo = req.registrationNo,
        SV.registrationCategory = Nothing,
        SV.createdAt = now,
        SV.updatedAt = now
      }

makeDriverInformationRes :: DriverAPI.DriverEntityRes -> Org.Organization -> DriverAPI.DriverInformationRes
makeDriverInformationRes DriverAPI.DriverEntityRes {..} org =
  DriverAPI.DriverInformationRes
    { organization = Org.makeOrganizationAPIEntity org,
      ..
    }
