module Product.Person
  ( listPerson,
    updatePerson,
    deletePerson,
    calculateAverageRating,
    mkPersonRes,
    getDriverPool,
    setDriverPool,
    calculateDriverPool,
    getPersonDetails,
    sendInviteSms,
  )
where

import App.Types
import Beckn.External.Encryption
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Utils.Validation (runRequestValidation)
import Data.Maybe
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.TransporterConfig as QTC
import qualified Storage.Queries.Vehicle as QV
import Types.API.Person
import Types.API.Registration (makeUserInfoRes)
import Types.App (Driver)
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.Location (Location)
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as SP
import Types.Storage.ProductInstance (ProductInstance)
import qualified Types.Storage.Rating as Rating
import Types.Storage.TransporterConfig (ConfigKey (ConfigKey))
import qualified Types.Storage.Vehicle as SV
import Utils.Common

updatePerson :: Id SP.Person -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson personId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdatePersonReq req
  person <- QP.findPersonById personId >>= fromMaybeM PersonNotFound
  isValidUpdate person
  updatedPerson <- modifyPerson req person
  DB.runSqlDB (QP.updatePersonRec personId updatedPerson)
  decPerson <- decrypt updatedPerson
  return . UpdatePersonRes $ makeUserInfoRes decPerson
  where
    isValidUpdate person =
      when (isJust (req.role) && person.role /= SP.ADMIN) $
        throwError Unauthorized

getPersonDetails :: Id SP.Person -> FlowHandler GetPersonDetailsRes
getPersonDetails personId = withFlowHandlerAPI $ do
  SP.Person {..} <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  pure $
    GetPersonDetailsRes {..}

listPerson :: Text -> [SP.Role] -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson orgId roles limitM offsetM = withFlowHandlerAPI $ do
  personList <- QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [Id orgId]
  respList <- traverse mkPersonRes personList
  return $ ListPersonRes respList

deletePerson :: Text -> Id SP.Person -> FlowHandler DeletePersonRes
deletePerson orgId (Id personId) = withFlowHandlerAPI $ do
  person <-
    QP.findPersonById (Id personId)
      >>= fromMaybeM PersonDoesNotExist
  unless (person.organizationId == Just (Id orgId)) $ throwError Unauthorized
  DB.runSqlDBTransaction $ do
    QP.deleteById (Id personId)
    QDriverStats.deleteById $ Id personId
    QDriverInformation.deleteById $ Id personId
    QR.deleteByEntitiyId personId
  return $ DeletePersonRes personId

-- Utility Functions

mkPersonRes :: (DBFlow m r, EncFlow m r) => SP.Person -> m PersonEntityRes
mkPersonRes person = do
  entity <- case person.udf2 >>= mapEntityType of
    Just VEHICLE -> do
      vehicle <- QV.findVehicleById $ Id $ fromMaybe "" (person.udf1)
      return $ Just $ LinkedEntity VEHICLE (Just $ encodeToText vehicle)
    _ -> return Nothing
  decMobNum <- decrypt person.mobileNumber
  return $
    PersonEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        fullName = person.fullName,
        role = person.role,
        gender = person.gender,
        email = person.email,
        identifier = person.identifier,
        identifierType = person.identifierType,
        mobileNumber = decMobNum,
        mobileCountryCode = person.mobileCountryCode,
        verified = person.verified,
        rating = person.rating,
        status = person.status,
        deviceToken = person.deviceToken,
        udf1 = person.udf1,
        udf2 = person.udf2,
        organizationId = person.organizationId,
        description = person.description,
        locationId = person.locationId,
        createdAt = person.createdAt,
        updatedAt = person.updatedAt,
        linkedEntity = entity
      }

sendInviteSms ::
  ( DBFlow m r,
    CoreMetrics m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  Text ->
  m ()
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

mapEntityType :: Text -> Maybe EntityType
mapEntityType entityType = case entityType of
  "VEHICLE" -> Just VEHICLE
  _ -> Nothing

calculateAverageRating ::
  (DBFlow m r, EncFlow m r) =>
  Id SP.Person ->
  m ()
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

getDriverPool ::
  (DBFlow m r, HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meter]) =>
  Id ProductInstance ->
  m [Id Driver]
getDriverPool piId =
  Redis.getKeyRedis (driverPoolKey piId)
    >>= maybe calcDriverPool (pure . map Id)
  where
    calcDriverPool = do
      prodInst <- QPI.findById piId >>= fromMaybeM PIDoesNotExist
      case_ <- Case.findById (prodInst.caseId) >>= fromMaybeM CaseNotFound
      vehicleVariant :: SV.Variant <-
        (case_.udf1 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf1")
      pickupPoint <-
        prodInst.fromLocation
          & fromMaybeM (PIFieldNotPresent "location_id")
      let orgId = prodInst.organizationId
      map fst <$> calculateDriverPool pickupPoint orgId vehicleVariant

setDriverPool :: DBFlow m r => Id ProductInstance -> [Id Driver] -> m ()
setDriverPool piId ids =
  Redis.setExRedis (driverPoolKey piId) (map getId ids) (60 * 10)

calculateDriverPool ::
  (DBFlow m r, HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meter]) =>
  Id Location ->
  Id Organization ->
  SV.Variant ->
  m [(Id Driver, Double)]
calculateDriverPool locId orgId variant = do
  location <- QL.findLocationById locId >>= fromMaybeM LocationNotFound
  lat <- location.lat & fromMaybeM (LocationFieldNotPresent "lat")
  long <- location.long & fromMaybeM (LocationFieldNotPresent "lon")
  radius <- getRadius
  measuringDurationToLog INFO "calculateDriverPool" $
    QP.getNearestDrivers
      (LatLong lat long)
      radius
      orgId
      variant
  where
    getRadius =
      QTC.findValueByOrgIdAndKey orgId (ConfigKey "radius")
        >>= maybe
          (fromIntegral <$> asks (.defaultRadiusOfSearch))
          radiusFromTransporterConfig
    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "The radius is not a number.")
        . readMaybe
        . toString
        $ conf.value
