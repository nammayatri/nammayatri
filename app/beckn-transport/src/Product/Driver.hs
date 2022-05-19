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
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Validation
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as SV
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Product.Registration as Registration
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Metrics
import qualified Types.API.Driver as DriverAPI
import Types.Error
import Utils.Auth (authTokenCacheKey)
import Utils.Common (fromMaybeM, throwError, withFlowHandlerAPI)
import qualified Utils.Notifications as Notify

createDriver :: SP.Person -> DriverAPI.OnboardDriverReq -> FlowHandler DriverAPI.OnboardDriverRes
createDriver admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation DriverAPI.validateOnboardDriverReq req
  let personEntity = req.person
  duplicateCheck
    (QVehicle.findByRegistrationNo req.vehicle.registrationNo)
    "Vehicle with this registration number already exists."
  duplicateCheck
    (QPerson.findByMobileNumber personEntity.mobileCountryCode personEntity.mobileNumber)
    "Person with this mobile number already exists"
  person <- buildDriver req.person orgId
  vehicle <- buildVehicle req.vehicle orgId
  Esq.runTransaction $ do
    QPerson.create person
    createDriverDetails (person.id)
    QVehicle.create vehicle
    QPerson.updateVehicle person.id $ Just vehicle.id
  org <-
    QOrganization.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
  decPerson <- decrypt person
  let mobNum = personEntity.mobileNumber
      mobCounCode = personEntity.mobileCountryCode
  smsCfg <- asks (.smsCfg)
  inviteSmsTemplate <- asks (.inviteSmsTemplate)
  sendInviteSms smsCfg inviteSmsTemplate (mobCounCode <> mobNum) (org.name)
    >>= SF.checkSmsResult
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ DriverAPI.OnboardDriverRes personAPIEntity
  where
    duplicateCheck cond err = whenM (isJust <$> cond) $ throwError $ InvalidRequest err

createDriverDetails :: Id SP.Person -> Esq.SqlDB ()
createDriverDetails personId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            active = False,
            onRide = False,
            enabled = True,
            createdAt = now,
            updatedAt = now,
            canDowngradeToSedan = False,
            canDowngradeToHatchback = False
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

getInformation :: Id SP.Person -> App.FlowHandler DriverAPI.DriverInformationRes
getInformation personId = withFlowHandlerAPI $ do
  _ <- Registration.checkPersonExists $ getId personId
  let driverId = cast personId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  organization <-
    QOrganization.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
  pure $ makeDriverInformationRes driverEntity organization

setActivity :: Id SP.Person -> Bool -> App.FlowHandler APISuccess.APISuccess
setActivity personId isActive = withFlowHandlerAPI $ do
  _ <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  when isActive $ do
    driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
    unless driverInfo.enabled $ throwError DriverAccountDisabled
  Esq.runTransaction $
    QDriverInformation.updateActivity driverId isActive
  pure APISuccess.Success

listDriver :: SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DriverAPI.ListDriverRes
listDriver admin mbSearchString mbLimit mbOffset = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  personList <- QDriverInformation.findAllWithLimitOffsetByOrgId mbSearchString mbLimit mbOffset orgId
  respPersonList <- traverse buildDriverEntityRes personList
  return $ DriverAPI.ListDriverRes respPersonList

buildDriverEntityRes :: (EsqDBFlow m r, EncFlow m r) => (SP.Person, DriverInformation) -> m DriverAPI.DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  vehicleM <- case person.udf1 of
    Nothing -> return Nothing
    Just udf -> QVehicle.findById $ Id udf
  decMobNum <- mapM decrypt person.mobileNumber
  return $
    DriverAPI.DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        rating = round <$> person.rating,
        linkedVehicle = SV.makeVehicleAPIEntity <$> vehicleM,
        active = driverInfo.active,
        onRide = driverInfo.onRide,
        enabled = driverInfo.enabled,
        registeredAt = person.createdAt,
        canDowngradeToSedan = driverInfo.canDowngradeToSedan,
        canDowngradeToHatchback = driverInfo.canDowngradeToHatchback
      }

linkVehicle :: SP.Person -> Id SP.Person -> Id SV.Vehicle -> FlowHandler DriverAPI.LinkVehicleRes
linkVehicle admin personId vehicleId = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  person <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  vehicle <-
    QVehicle.findById vehicleId
      >>= fromMaybeM (VehicleDoesNotExist vehicleId.getId)
  unless
    ( person.organizationId == Just orgId
        && vehicle.organizationId == orgId
    )
    (throwError Unauthorized)
  prevPerson <- QPerson.findByVehicleId vehicleId
  whenJust prevPerson $ \_ -> throwError VehicleAlreadyLinked
  Esq.runTransaction $ do
    QPerson.updateVehicle personId $ Just vehicleId
    QDriverInformation.resetDowngradingOptions (cast personId)
  return APISuccess.Success

changeDriverEnableState :: SP.Person -> Id SP.Person -> Bool -> FlowHandler APISuccess
changeDriverEnableState admin personId isEnabled = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  person <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (person.organizationId == Just orgId) $ throwError Unauthorized
  Esq.runTransaction $ do
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
    QPerson.findById driverId
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.organizationId == Just orgId || driver.role == SP.DRIVER) $ throwError Unauthorized
  clearDriverSession driverId
  Esq.runTransaction $ do
    QR.deleteByPersonId personId
    whenJust driver.udf1 $ QVehicle.deleteById . Id
    QPerson.deleteById driverId
  return Success
  where
    clearDriverSession personId = do
      regTokens <- QR.findAllByPersonId personId
      for_ regTokens $ \regToken -> do
        void $ Redis.deleteKeyRedis $ authTokenCacheKey regToken.token

updateDriver :: Id SP.Person -> DriverAPI.UpdateDriverReq -> FlowHandler DriverAPI.UpdateDriverRes
updateDriver personId req = withFlowHandlerAPI $ do
  runRequestValidation DriverAPI.validateUpdateDriverReq req
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let updPerson =
        person{firstName = fromMaybe person.firstName req.firstName,
               middleName = req.middleName <|> person.middleName,
               lastName = req.lastName <|> person.lastName,
               deviceToken = req.deviceToken <|> person.deviceToken
              }

  mbVehicle <- forM person.udf1 $ \vehicleId -> QVehicle.findById (Id vehicleId) >>= fromMaybeM (VehicleNotFound vehicleId)
  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  let wantToDowngrade = req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True
  case mbVehicle of
    Nothing -> when wantToDowngrade $ throwError $ InvalidRequest "Driver without vehicle can't downgrade"
    Just vehicle -> do
      when (vehicle.variant == SV.SEDAN && req.canDowngradeToSedan == Just True) $
        throwError $ InvalidRequest "Driver with sedan can't downgrade to sedan"
      when (vehicle.variant == SV.HATCHBACK && wantToDowngrade) $
        throwError $ InvalidRequest "Driver with hatchback can't downgrade"
  let updDriverInfo =
        driverInfo{canDowngradeToSedan = fromMaybe driverInfo.canDowngradeToSedan req.canDowngradeToSedan,
                   canDowngradeToHatchback = fromMaybe driverInfo.canDowngradeToHatchback req.canDowngradeToHatchback
                  }
  Esq.runTransaction $ do
    QPerson.updatePersonRec personId updPerson
    QDriverInformation.updateDowngradingOptions (cast person.id) updDriverInfo.canDowngradeToSedan updDriverInfo.canDowngradeToHatchback
  driverEntity <- buildDriverEntityRes (updPerson, updDriverInfo)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  org <-
    QOrganization.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
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

buildDriver :: (EncFlow m r) => DriverAPI.CreatePerson -> Id Org.Organization -> m SP.Person
buildDriver req orgId = do
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
        SP.udf1 = Nothing,
        SP.udf2 = Nothing,
        SP.organizationId = Just orgId,
        SP.description = Nothing,
        SP.createdAt = now,
        SP.updatedAt = now
      }

buildVehicle :: MonadFlow m => DriverAPI.CreateVehicle -> Id Org.Organization -> m SV.Vehicle
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
        SV.model = req.model,
        SV.size = Nothing,
        SV.organizationId = orgId,
        SV.variant = req.variant,
        SV.color = req.color,
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
