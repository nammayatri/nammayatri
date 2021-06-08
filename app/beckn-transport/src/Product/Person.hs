{-# LANGUAGE OverloadedLabels #-}

module Product.Person
  ( createPerson,
    createDriverDetails,
    listPerson,
    updatePerson,
    deletePerson,
    linkEntity,
    calculateAverageRating,
    mkPersonRes,
    getDriverPool,
    setDriverPool,
    calculateDriverPool,
    getPersonDetails,
  )
where

import App.Types
import Beckn.External.Encryption
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.TypeClass.Transform
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Storage.Location (Location)
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import qualified Beckn.Types.Storage.Rating as Rating
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as SV
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import EulerHS.Prelude hiding (id)
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
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.DriverInformation as DriverInformation
import Types.Storage.TransporterConfig (ConfigKey (ConfigKey))
import Utils.Common

updatePerson :: SR.RegistrationToken -> Id SP.Person -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson SR.RegistrationToken {..} (Id personId) req = withFlowHandlerAPI $ do
  verifyPerson entityId
  person <- QP.findPersonById (Id entityId)
  isValidUpdate person
  updatedPerson <- modifyTransform req person
  encryptedPerson <- encrypt updatedPerson
  DB.runSqlDB (QP.updatePersonRec (Id entityId) encryptedPerson)
  return $ UpdatePersonRes updatedPerson
  where
    verifyPerson entityId_ =
      when (personId /= entityId_) $
        throwError AccessDenied
    isValidUpdate person =
      when (isJust (req ^. #role) && person ^. #role /= SP.ADMIN) $
        throwError Unauthorized

createPerson :: Text -> CreatePersonReq -> FlowHandler UpdatePersonRes
createPerson orgId req = withFlowHandlerAPI $ do
  validateDriver req
  person <- addOrgId (Id orgId) <$> createTransform req
  QP.create person
  when (person ^. #role == SP.DRIVER) $ createDriverDetails (person ^. #id)
  org <- OQ.findOrganizationById (Id orgId)
  case (req ^. #role, req ^. #mobileNumber, req ^. #mobileCountryCode) of
    (Just SP.DRIVER, Just mobileNumber, Just countryCode) -> do
      smsCfg <- smsCfg <$> ask
      inviteSmsTemplate <- inviteSmsTemplate <$> ask
      sendInviteSms smsCfg inviteSmsTemplate (countryCode <> mobileNumber) (org ^. #name)
      return $ UpdatePersonRes person
    _ -> return $ UpdatePersonRes person
  where
    validateDriver :: CreatePersonReq -> Flow ()
    validateDriver preq =
      when (preq ^. #role == Just SP.DRIVER) $
        case (preq ^. #mobileNumber, req ^. #mobileCountryCode) of
          (Just mobileNumber, Just countryCode) ->
            whenM (isJust <$> QP.findByMobileNumber countryCode mobileNumber) $
              throwError $ InvalidRequest "Person with this mobile number already exists."
          _ -> throwError $ InvalidRequest "You should pass mobile number and country code."

createDriverDetails :: Id SP.Person -> Flow ()
createDriverDetails personId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInformation.DriverInformation
          { driverId = personId,
            active = False,
            onRide = False,
            createdAt = now,
            updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

getPersonDetails :: SR.RegistrationToken -> FlowHandler GetPersonDetailsRes
getPersonDetails regToken = withFlowHandlerAPI $ do
  SP.Person {..} <- QP.findPersonById (Id $ regToken ^. #entityId)
  pure $
    GetPersonDetailsRes {..}

listPerson :: Text -> [SP.Role] -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson orgId roles limitM offsetM = withFlowHandlerAPI $ do
  personList <- QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [Id orgId]
  respList <- traverse mkPersonRes personList
  return $ ListPersonRes respList

deletePerson :: Text -> Id SP.Person -> FlowHandler DeletePersonRes
deletePerson orgId (Id personId) = withFlowHandlerAPI $ do
  person <- QP.findPersonById (Id personId)
  unless (person ^. #organizationId == Just (Id orgId)) $ throwError Unauthorized
  DB.runSqlDBTransaction $ do
    QP.deleteById (Id personId)
    QDriverStats.deleteById $ Id personId
    QDriverInformation.deleteById $ Id personId
    QR.deleteByEntitiyId personId
  return $ DeletePersonRes personId

linkEntity :: Text -> Id SP.Person -> LinkReq -> FlowHandler PersonEntityRes
linkEntity orgId (Id personId) req = withFlowHandlerAPI $ do
  person <- QP.findPersonById (Id personId)
  _ <- case req ^. #entityType of
    VEHICLE ->
      QV.findVehicleById (Id (req ^. #entityId))
        >>= fromMaybeM VehicleNotFound
    _ -> throwError $ InvalidRequest "Unsupported entity type."
  when
    (person ^. #organizationId /= Just (Id orgId))
    (throwError Unauthorized)
  prevPerson <- QP.findByEntityId (req ^. #entityId)
  whenJust prevPerson (\p -> QP.updateEntity (p ^. #id) T.empty T.empty)
  QP.updateEntity (Id personId) (req ^. #entityId) (T.pack $ show $ req ^. #entityType)
  updatedPerson <- QP.findPersonById $ person ^. #id
  mkPersonRes updatedPerson

-- Utility Functions

addOrgId :: Id Organization -> SP.Person -> SP.Person
addOrgId orgId person = person {SP.organizationId = Just orgId}

mkPersonRes :: SP.Person -> Flow PersonEntityRes
mkPersonRes person = do
  entity <- case person ^. #udf2 >>= mapEntityType of
    Just VEHICLE -> do
      vehicle <- QV.findVehicleById $ Id $ fromMaybe "" (person ^. #udf1)
      return $ Just $ LinkedEntity VEHICLE (Just $ encodeToText vehicle)
    _ -> return Nothing
  return $
    PersonEntityRes
      { id = person ^. #id,
        firstName = person ^. #firstName,
        middleName = person ^. #middleName,
        lastName = person ^. #lastName,
        fullName = person ^. #fullName,
        role = person ^. #role,
        gender = person ^. #gender,
        email = person ^. #email,
        identifier = person ^. #identifier,
        identifierType = person ^. #identifierType,
        mobileNumber = person ^. #mobileNumber,
        mobileCountryCode = person ^. #mobileCountryCode,
        verified = person ^. #verified,
        rating = person ^. #rating,
        status = person ^. #status,
        deviceToken = person ^. #deviceToken,
        udf1 = person ^. #udf1,
        udf2 = person ^. #udf2,
        organizationId = person ^. #organizationId,
        description = person ^. #description,
        locationId = person ^. #locationId,
        createdAt = person ^. #createdAt,
        updatedAt = person ^. #updatedAt,
        linkedEntity = entity
      }

sendInviteSms :: SmsConfig -> Text -> Text -> Text -> Flow ()
sendInviteSms smsCfg inviteTemplate phoneNumber orgName = do
  let url = smsCfg ^. #url
  let smsCred = smsCfg ^. #credConfig
  let sender = smsCfg ^. #sender
  SF.submitSms
    url
    SMS.SubmitSms
      { SMS.username = smsCred ^. #username,
        SMS.password = smsCred ^. #password,
        SMS.from = sender,
        SMS.to = phoneNumber,
        SMS.text = SF.constructInviteSms orgName inviteTemplate
      }

mapEntityType :: Text -> Maybe EntityType
mapEntityType entityType = case entityType of
  "VEHICLE" -> Just VEHICLE
  _ -> Nothing

calculateAverageRating :: Id SP.Person -> Flow ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratings = sum $ Rating.ratingValue <$> allRatings
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
      case_ <- Case.findById (prodInst ^. #caseId)
      vehicleVariant :: SV.Variant <-
        (case_ ^. #udf1 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf1")
      pickupPoint <-
        prodInst ^. #fromLocation
          & fromMaybeM (PIFieldNotPresent "location_id")
      let orgId = prodInst ^. #organizationId
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
  lat <- location ^. #lat & fromMaybeM (LocationFieldNotPresent "lat")
  long <- location ^. #long & fromMaybeM (LocationFieldNotPresent "lon")
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
          (asks defaultRadiusOfSearch)
          radiusFromTransporterConfig
    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "The radius is not a number.")
        . readMaybe
        . toString
        $ conf ^. #value
