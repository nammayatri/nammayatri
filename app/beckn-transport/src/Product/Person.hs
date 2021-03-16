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
import qualified Types.Storage.DriverInformation as DriverInformation

updatePerson :: SR.RegistrationToken -> Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson SR.RegistrationToken {..} personId req = withFlowHandler $ do
  verifyPerson _EntityId
  person <- QP.findPersonById (Id _EntityId)
  isValidUpdate person
  updatedPerson <- modifyTransform req person
  QP.updatePersonRec (Id _EntityId) updatedPerson
  return $ UpdatePersonRes updatedPerson
  where
    verifyPerson entityId =
      when (personId /= entityId) $
        throwError400 "PERSON_ID_MISMATCH"
    isValidUpdate person =
      when (isJust (req ^. #_role) && person ^. #_role /= SP.ADMIN) $
        throwError401 "ADMIN ACCESS REQUIRED"

createPerson :: Text -> CreatePersonReq -> FlowHandler UpdatePersonRes
createPerson orgId req = withFlowHandler $ do
  validateDriver req
  person <- addOrgId orgId <$> createTransform req
  QP.create person
  when (person ^. #_role == SP.DRIVER) $ createDriverDetails (person ^. #_id)
  org <- OQ.findOrganizationById (Id orgId)
  case (req ^. #_role, req ^. #_mobileNumber, req ^. #_mobileCountryCode) of
    (Just SP.DRIVER, Just mobileNumber, Just countryCode) -> do
      credCfg <- credConfig . smsCfg <$> ask
      sendInviteSms credCfg (countryCode <> mobileNumber) (org ^. #_name)
      return $ UpdatePersonRes person
    _ -> return $ UpdatePersonRes person
  where
    validateDriver :: CreatePersonReq -> Flow ()
    validateDriver preq =
      when (preq ^. #_role == Just SP.DRIVER) $
        case (preq ^. #_mobileNumber, req ^. #_mobileCountryCode) of
          (Just mobileNumber, Just countryCode) ->
            whenM (isJust <$> QP.findByMobileNumber countryCode mobileNumber) $
              throwError400 "DRIVER_ALREADY_CREATED"
          _ -> throwError400 "MOBILE_NUMBER_AND_COUNTRY_CODE_MANDATORY"

createDriverDetails :: Id SP.Person -> Flow ()
createDriverDetails personId = do
  now <- getCurrTime
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
  Maybe Text ->
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
      Nothing -> QP.findPersonById (Id $ fromJust idM)
      Just SP.MOBILENUMBER -> do
        countryCode <- fromMaybeM400 "MOBILE_COUNTRY_CODE_REQUIRED" countryCodeM
        mobile <- fromMaybeM400 "MOBILE_NUMBER_REQUIRED" mobileM
        QP.findByMobileNumber countryCode mobile
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
      Just SP.EMAIL ->
        fromMaybeM400 "EMAIL_REQUIRED" emailM
          >>= QP.findByEmail
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
      Just SP.AADHAAR ->
        fromMaybeM400 "IDENTIFIER_REQUIRED" identifierM
          >>= QP.findByIdentifier
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
    hasAccess user person
    mkPersonRes person
  where
    hasAccess :: SP.Person -> SP.Person -> Flow ()
    hasAccess user person =
      when
        ( (user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id)
            || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ throwError401 "Unauthorized"

deletePerson :: Text -> Text -> FlowHandler DeletePersonRes
deletePerson orgId personId = withFlowHandler $ do
  person <- QP.findPersonById (Id personId)
  if person ^. #_organizationId == Just orgId
    then do
      QP.deleteById (Id personId)
      QDriverStats.deleteById $ Id personId
      QDriverInformation.deleteById $ Id personId
      QR.deleteByEntitiyId personId
      return $ DeletePersonRes personId
    else throwError401 "Unauthorized"

linkEntity :: Text -> Text -> LinkReq -> FlowHandler PersonEntityRes
linkEntity orgId personId req = withFlowHandler $ do
  person <- QP.findPersonById (Id personId)
  _ <- case req ^. #_entityType of
    VEHICLE ->
      QV.findVehicleById (Id (req ^. #_entityId))
        >>= fromMaybeM400 "VEHICLE NOT REGISTERED"
    _ -> throwError400 "UNSUPPORTED ENTITY TYPE"
  when
    (person ^. #_organizationId /= Just orgId)
    (throwError401 "Unauthorized")
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

sendInviteSms :: SmsCredConfig -> Text -> Text -> Flow ()
sendInviteSms SmsCredConfig {..} phoneNumber orgName = do
  res <-
    SF.submitSms
      SF.defaultBaseUrl
      SMS.SubmitSms
        { SMS._username = username,
          SMS._password = password,
          SMS._from = SMS.JUSPAY,
          SMS._to = phoneNumber,
          SMS._category = SMS.BULK,
          SMS._text = SF.constructInviteSms orgName
        }
  whenLeft res $ \_err -> return () -- ignore error silently

mapEntityType :: Text -> Maybe EntityType
mapEntityType entityType = case entityType of
  "VEHICLE" -> Just VEHICLE
  _ -> Nothing

calculateAverageRating :: Id SP.Person -> Flow ()
calculateAverageRating personId = do
  logInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratings = sum $ Rating._ratingValue <$> allRatings
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logInfo "PersonAPI" "No rating found to calculate"
  when (ratingCount > 0) $ do
    let newAverage = ratings `div` ratingCount
    logInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
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
          & fromMaybeM500 "NO_VEHICLE_VARIANT"
      pickupPoint <-
        Id <$> prodInst ^. #_fromLocation
          & fromMaybeM500 "NO_FROM_LOCATION"
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
  location <- QL.findLocationById locId >>= fromMaybeM500 "NO_LOCATION_FOUND"
  lat <- location ^. #_lat & fromMaybeM500 "NO_LATITUDE"
  long <- location ^. #_long & fromMaybeM500 "NO_LONGITUDE"
  radius <- getRadius
  getNearestDriversStartTime <- getCurrTime
  driverPool <-
    map fst
      <$> QP.getNearestDrivers
        (LatLong lat long)
        radius
        orgId
        variant
  getNearestDriversEndTime <- getCurrTime
  let getNearestDriversTime = diffUTCTime getNearestDriversEndTime getNearestDriversStartTime
  logInfo "calculateDriverPool" $ show getNearestDriversTime <> " time spent for getNearestDrivers"
  pure driverPool
  where
    getRadius =
      QTC.findValueByOrgIdAndKey orgId (ConfigKey "radius")
        >>= maybe
          (asks (defaultRadiusOfSearch . driverAllocationConfig))
          radiusFromTransporterConfig
    radiusFromTransporterConfig conf =
      fromMaybeM500 "THE_RADIUS_IS_NOT_A_NUMBER"
        . readMaybe
        . toString
        $ conf ^. #_value
