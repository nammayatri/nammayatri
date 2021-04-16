{-# LANGUAGE OverloadedLabels #-}

module Product.Person
  ( createPerson,
    createDriverDetails,
    listPerson,
    updatePerson,
    getPerson,
    deletePerson,
    linkEntity,
    calculateAverageRating,
    mkPersonRes,
    getDriverPool,
    setDriverPool,
    calculateDriverPool,
  )
where

import App.Types
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.TypeClass.Transform
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Storage.Location (Location)
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import qualified Beckn.Types.Storage.Rating as Rating
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as SV
import Beckn.Utils.Common
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import EulerHS.Prelude
import qualified Models.Case as Case
import qualified Models.ProductInstance as PI
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.TransporterConfig as QTC
import qualified Storage.Queries.Vehicle as QV
import Types.API.Location (LatLong (..))
import Types.API.Person
import Types.App (ConfigKey (..), Driver)
import Types.Error
import qualified Types.Storage.DriverInformation as DriverInformation

updatePerson :: SR.RegistrationToken -> Id SP.Person -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson SR.RegistrationToken {..} (Id personId) req = withFlowHandler $ do
  verifyPerson _EntityId
  person <- QP.findPersonById (Id _EntityId)
  isValidUpdate person
  updatedPerson <- modifyTransform req person
  QP.updatePersonRec (Id _EntityId) updatedPerson
  return $ UpdatePersonRes updatedPerson
  where
    verifyPerson entityId =
      when (personId /= entityId) $
        throwError AccessDenied
    isValidUpdate person =
      when (isJust (req ^. #_role) && person ^. #_role /= SP.ADMIN) $
        throwError Unauthorized

createPerson :: Text -> CreatePersonReq -> FlowHandler UpdatePersonRes
createPerson orgId req = withFlowHandler $ do
  validateDriver req
  person <- addOrgId orgId <$> createTransform req
  QP.create person
  when (person ^. #_role == SP.DRIVER) $ createDriverDetails (person ^. #_id)
  org <- OQ.findOrganizationById (Id orgId)
  case (req ^. #_role, req ^. #_mobileNumber, req ^. #_mobileCountryCode) of
    (Just SP.DRIVER, Just mobileNumber, Just countryCode) -> do
      smsCfg <- smsCfg <$> ask
      inviteSmsTemplate <- inviteSmsTemplate <$> ask
      sendInviteSms smsCfg inviteSmsTemplate (countryCode <> mobileNumber) (org ^. #_name)
      return $ UpdatePersonRes person
    _ -> return $ UpdatePersonRes person
  where
    validateDriver :: CreatePersonReq -> Flow ()
    validateDriver preq =
      when (preq ^. #_role == Just SP.DRIVER) $
        case (preq ^. #_mobileNumber, req ^. #_mobileCountryCode) of
          (Just mobileNumber, Just countryCode) ->
            whenM (isJust <$> QP.findByMobileNumber countryCode mobileNumber) $
              throwErrorWithInfo InvalidRequest "Person with this mobile number already exists."
          _ -> throwErrorWithInfo InvalidRequest "You should pass mobile number and country code."

createDriverDetails :: Id SP.Person -> Flow ()
createDriverDetails personId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInformation.DriverInformation
          { _driverId = driverId,
            _active = False,
            _onRide = False,
            _createdAt = now,
            _updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

listPerson :: Text -> [SP.Role] -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson orgId roles limitM offsetM = withFlowHandler $ do
  personList <- QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [orgId]
  respList <- traverse mkPersonRes personList
  return $ ListPersonRes respList

getPerson ::
  SR.RegistrationToken ->
  Maybe (Id SP.Person) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe SP.IdentifierType ->
  FlowHandler PersonEntityRes
getPerson SR.RegistrationToken {..} idM mobileM countryCodeM emailM identifierM identifierTypeM =
  withFlowHandler $ do
    user <- QP.findPersonById (Id _EntityId)
    -- TODO: fix this to match based on identifierType
    -- And maybe have a way to handle the case when Id is
    -- passed and identifierType is null. Throw validation errors
    person <- case identifierTypeM of
      Nothing -> QP.findPersonById (fromJust idM)
      Just SP.MOBILENUMBER -> do
        countryCode <- fromMaybeMWithInfo InvalidRequest "You should pass country code." countryCodeM
        mobile <- fromMaybeMWithInfo InvalidRequest "You should pass mobile number." mobileM
        QP.findByMobileNumber countryCode mobile
          >>= fromMaybeM PersonDoesNotExist
      Just SP.EMAIL ->
        fromMaybeMWithInfo InvalidRequest "You should pass email." emailM
          >>= QP.findByEmail
          >>= fromMaybeM PersonDoesNotExist
      Just SP.AADHAAR ->
        fromMaybeMWithInfo InvalidRequest "You should pass identifier." identifierM
          >>= QP.findByIdentifier
          >>= fromMaybeM PersonDoesNotExist
    hasAccess user person
    mkPersonRes person
  where
    hasAccess :: SP.Person -> SP.Person -> Flow ()
    hasAccess user person =
      when
        ( (user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id)
            || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ throwError Unauthorized

deletePerson :: Text -> Id SP.Person -> FlowHandler DeletePersonRes
deletePerson orgId (Id personId) = withFlowHandler $ do
  person <- QP.findPersonById (Id personId)
  if person ^. #_organizationId == Just orgId
    then do
      QP.deleteById (Id personId)
      QDriverStats.deleteById $ Id personId
      QDriverInformation.deleteById $ Id personId
      QR.deleteByEntitiyId personId
      return $ DeletePersonRes personId
    else throwError Unauthorized

linkEntity :: Text -> Id SP.Person -> LinkReq -> FlowHandler PersonEntityRes
linkEntity orgId (Id personId) req = withFlowHandler $ do
  person <- QP.findPersonById (Id personId)
  _ <- case req ^. #_entityType of
    VEHICLE ->
      QV.findVehicleById (Id (req ^. #_entityId))
        >>= fromMaybeM VehicleNotFound
    _ -> throwErrorWithInfo InvalidRequest "Unsupported entity type."
  when
    (person ^. #_organizationId /= Just orgId)
    (throwError Unauthorized)
  prevPerson <- QP.findByEntityId (req ^. #_entityId)
  whenJust prevPerson (\p -> QP.updateEntity (p ^. #_id) T.empty T.empty)
  QP.updateEntity (Id personId) (req ^. #_entityId) (T.pack $ show $ req ^. #_entityType)
  updatedPerson <- QP.findPersonById $ person ^. #_id
  mkPersonRes updatedPerson

-- Utility Functions

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}

mkPersonRes :: SP.Person -> Flow PersonEntityRes
mkPersonRes person = do
  entity <- case person ^. #_udf2 >>= mapEntityType of
    Just VEHICLE -> do
      vehicle <- QV.findVehicleById $ Id $ fromMaybe "" (person ^. #_udf1)
      return $ Just $ LinkedEntity VEHICLE (Just $ encodeToText vehicle)
    _ -> return Nothing
  return $
    PersonEntityRes
      { _id = person ^. #_id,
        _firstName = person ^. #_firstName,
        _middleName = person ^. #_middleName,
        _lastName = person ^. #_lastName,
        _fullName = person ^. #_fullName,
        _role = person ^. #_role,
        _gender = person ^. #_gender,
        _email = person ^. #_email,
        _identifier = person ^. #_identifier,
        _identifierType = person ^. #_identifierType,
        _mobileNumber = person ^. #_mobileNumber,
        _mobileCountryCode = person ^. #_mobileCountryCode,
        _verified = person ^. #_verified,
        _rating = person ^. #_rating,
        _status = person ^. #_status,
        _deviceToken = person ^. #_deviceToken,
        _udf1 = person ^. #_udf1,
        _udf2 = person ^. #_udf2,
        _organizationId = person ^. #_organizationId,
        _description = person ^. #_description,
        _locationId = person ^. #_locationId,
        _createdAt = person ^. #_createdAt,
        _updatedAt = person ^. #_updatedAt,
        _linkedEntity = entity
      }

sendInviteSms :: SmsConfig -> Text -> Text -> Text -> Flow ()
sendInviteSms smsCfg inviteTemplate phoneNumber orgName = do
  let url = smsCfg ^. #url
  let smsCred = smsCfg ^. #credConfig
  let sender = smsCfg ^. #sender
  res <-
    SF.submitSms
      url
      SMS.SubmitSms
        { SMS._username = smsCred ^. #username,
          SMS._password = smsCred ^. #password,
          SMS._from = sender,
          SMS._to = phoneNumber,
          SMS._text = SF.constructInviteSms orgName inviteTemplate
        }
  whenLeft res $ \_err -> return () -- ignore error silently

mapEntityType :: Text -> Maybe EntityType
mapEntityType entityType = case entityType of
  "VEHICLE" -> Just VEHICLE
  _ -> Nothing

calculateAverageRating :: Id SP.Person -> Flow ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratings = sum $ Rating._ratingValue <$> allRatings
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  when (ratingCount > 0) $ do
    let newAverage = ratings `div` ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
    QP.updateAverageRating personId $ encodeToText newAverage

driverPoolKey :: Id ProductInstance -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getDriverPool :: Id ProductInstance -> Flow [Id Driver]
getDriverPool piId =
  Redis.getKeyRedis (driverPoolKey piId)
    >>= maybe calcDriverPool (pure . map Id)
  where
    calcDriverPool = do
      prodInst <- PI.findById piId
      case_ <- Case.findById (prodInst ^. #_caseId)
      vehicleVariant :: SV.Variant <-
        (case_ ^. #_udf1 >>= readMaybe . T.unpack)
          & fromMaybeM CaseVehicleVariantNotPresent
      pickupPoint <-
        Id <$> prodInst ^. #_fromLocation
          & fromMaybeM PIFromLocationIdNotPresent
      let orgId = Id (prodInst ^. #_organizationId)
      calculateDriverPool pickupPoint orgId vehicleVariant

setDriverPool :: Id ProductInstance -> [Id Driver] -> Flow ()
setDriverPool piId ids =
  Redis.setExRedis (driverPoolKey piId) (map getId ids) (60 * 10)

calculateDriverPool ::
  Id Location ->
  Id Organization ->
  SV.Variant ->
  Flow [Id Driver]
calculateDriverPool locId orgId variant = do
  location <- QL.findLocationById locId >>= fromMaybeM LocationNotFound
  lat <- location ^. #_lat & fromMaybeM LocationLongLatNotFound
  long <- location ^. #_long & fromMaybeM LocationLongLatNotFound
  radius <- getRadius
  getNearestDriversStartTime <- getCurrentTime
  driverPool <-
    map fst
      <$> QP.getNearestDrivers
        (LatLong lat long)
        radius
        orgId
        variant
  getNearestDriversEndTime <- getCurrentTime
  let getNearestDriversTime = diffUTCTime getNearestDriversEndTime getNearestDriversStartTime
  logTagInfo "calculateDriverPool" $ show getNearestDriversTime <> " time spent for getNearestDrivers"
  pure driverPool
  where
    getRadius =
      QTC.findValueByOrgIdAndKey orgId (ConfigKey "radius")
        >>= maybe
          (asks (defaultRadiusOfSearch . driverAllocationConfig))
          radiusFromTransporterConfig
    radiusFromTransporterConfig conf =
      fromMaybeMWithInfo CommonInternalError "The radius is not a number."
        . readMaybe
        . toString
        $ conf ^. #_value
