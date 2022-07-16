module Product.Driver
  ( getInformation,
    setActivity,
    listDriver,
    changeDriverEnableState,
    createDriver,
    deleteDriver,
    updateDriver,
    getNearbySearchRequests,
    offerQuote,
  )
where

import Beckn.External.Encryption
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config (SmsConfig)
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import Beckn.Types.Id
import Beckn.Utils.Validation
import Core.ACL.OnSelect
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.DriverQuote as DDrQuote
import qualified Domain.Types.FareParams as Fare
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import qualified Domain.Types.SearchRequest as DSReq
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.Vehicle as SV
import Environment
import EulerHS.Prelude hiding (id, state)
import ExternalAPI.Flow
import GHC.Records.Extra
import qualified Product.Registration as Registration
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Metrics
import Types.API.Driver as DriverAPI
import Types.Error
import Utils.Auth (authTokenCacheKey)
import Utils.Common
import Utils.Context (contextTemplate)
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
  vehicle <- buildVehicle req.vehicle person.id orgId
  Esq.runTransaction $ do
    QPerson.create person
    createDriverDetails (person.id)
    QVehicle.create vehicle
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> createDriver : ") (show person.id)
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
            updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

getInformation :: Id SP.Person -> FlowHandler DriverAPI.DriverInformationRes
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

setActivity :: Id SP.Person -> Bool -> FlowHandler APISuccess.APISuccess
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

buildDriverEntityRes :: (Esq.Transactionable m, EncFlow m r) => (SP.Person, DriverInformation) -> m DriverAPI.DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  vehicle <- QVehicle.findById person.id >>= fromMaybeM (VehicleNotFound person.id.getId)
  decMobNum <- mapM decrypt person.mobileNumber
  return $
    DriverAPI.DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        rating = round <$> person.rating,
        linkedVehicle = SV.makeVehicleAPIEntity vehicle,
        active = driverInfo.active,
        onRide = driverInfo.onRide,
        enabled = driverInfo.enabled,
        registeredAt = person.createdAt
      }

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
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> changeDriverEnableState : ") (show (driverId, isEnabled))
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
    QR.deleteByPersonId driverId
    QVehicle.deleteById driverId
    QPerson.deleteById driverId
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteDriver : ") (show driverId)
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

  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  Esq.runTransaction $ do
    QPerson.updatePersonRec personId updPerson
  driverEntity <- buildDriverEntityRes (updPerson, driverInfo)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  org <-
    QOrganization.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
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
        SP.registered = False,
        SP.rating = Nothing,
        SP.deviceToken = Nothing,
        SP.organizationId = Just orgId,
        SP.description = Nothing,
        SP.createdAt = now,
        SP.updatedAt = now
      }

buildVehicle :: MonadFlow m => DriverAPI.CreateVehicle -> Id SP.Person -> Id Org.Organization -> m SV.Vehicle
buildVehicle req personId orgId = do
  now <- getCurrentTime
  return $
    SV.Vehicle
      { SV.driverId = personId,
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

getNearbySearchRequests ::
  Id SP.Person ->
  FlowHandler GetNearbySearchRequestsRes
getNearbySearchRequests driverId = withFlowHandlerAPI $ do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  _ <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  nearbyReqs <- QSRD.findByDriver driverId
  pure $ GetNearbySearchRequestsRes $ map mkSearchRequestForDriverAPIEntity nearbyReqs

offerQuote ::
  Id SP.Person ->
  DriverOfferReq ->
  FlowHandler APISuccess
offerQuote driverId req = withFlowHandlerAPI $ do
  logDebug $ "offered fare: " <> show req.offeredFare
  sReq <- QSReq.findById req.searchRequestId >>= fromMaybeM (SearchRequestNotFound req.searchRequestId.getId)
  organization <- QOrg.findById sReq.providerId >>= fromMaybeM (OrgDoesNotExist sReq.providerId.getId)
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  searchRequestForDriver <-
    QSRD.findByDriverAndSearchReq driverId sReq.id
      >>= fromMaybeM (InvalidRequest "no calculated request pool")
  driverQuote <- buildDriverQuote driver sReq searchRequestForDriver req.offeredFare
  Esq.runTransaction $ QDrQt.create driverQuote
  context <- contextTemplate organization Context.SELECT sReq.bapId sReq.bapUri (Just sReq.transactionId) sReq.messageId
  let callbackUrl = sReq.bapUri
      action = do
        onSelectReq <- buildOnSelectReq organization sReq [driverQuote] <&> mkOnSelectMessage
        logDebug $ "on_select request bpp: " <> show onSelectReq
        pure onSelectReq
  void $ withCallback' withRetry organization Context.SELECT API.onSelectAPI context callbackUrl action
  pure Success
  where
    buildDriverQuote ::
      (MonadFlow m) =>
      SP.Person ->
      DSReq.SearchRequest ->
      SearchRequestForDriver ->
      Maybe Double ->
      m DDrQuote.DriverQuote
    buildDriverQuote driver s sd offeredFare = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        DDrQuote.DriverQuote
          { id = guid,
            status = DDrQuote.Active,
            searchRequestId = s.id,
            driverId,
            driverName = driver.firstName,
            driverRating = driver.rating,
            vehicleVariant = sd.vehicleVariant,
            distance = sd.distance,
            distanceToPickup = sd.distanceToPickup,
            durationToPickup = sd.durationToPickup,
            createdAt = now,
            updatedAt = now,
            validTill = s.validTill, -- what should be here? FIXME
            fareParams = s.fareParams {Fare.driverSelectedFare = fmap (Amount . toRational) offeredFare}
          }

buildOnSelectReq ::
  (MonadTime m, HasPrettyLogger m r) =>
  Org.Organization ->
  DSReq.SearchRequest ->
  [DDrQuote.DriverQuote] ->
  m DOnSelectReq
buildOnSelectReq org searchRequest quotes = do
  now <- getCurrentTime
  logPretty DEBUG "on_select: searchRequest" searchRequest
  logPretty DEBUG "on_select: quotes" quotes
  let transporterInfo =
        TransporterInfo
          { shortId = org.shortId,
            name = org.name,
            contacts = fromMaybe "" org.mobileNumber,
            ridesInProgress = 0, -- FIXME
            ridesCompleted = 0, -- FIXME
            ridesConfirmed = 0 -- FIXME
          }
  pure $
    DOnSelectReq
      { transporterInfo,
        quotes,
        now,
        searchRequest
      }
