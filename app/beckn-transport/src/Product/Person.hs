module Product.Person
  ( updatePerson,
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
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.SearchReqLocation as QL
import qualified Storage.Queries.TransporterConfig as QTC
import Types.API.Person
import Types.API.Registration (makeUserInfoRes)
import Types.App (Driver)
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as SP
import Types.Storage.ProductInstance (ProductInstance)
import Types.Storage.SearchReqLocation (SearchReqLocation)
import Types.Storage.TransporterConfig (ConfigKey (ConfigKey))
import qualified Types.Storage.Vehicle as SV
import Utils.Common

updatePerson :: Id SP.Person -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson personId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdatePersonReq req
  person <- QP.findPersonById personId >>= fromMaybeM PersonNotFound
  isValidUpdate person
  let updatedPerson = modifyPerson req person
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

deletePerson :: Id Organization -> Id SP.Person -> FlowHandler DeletePersonRes
deletePerson orgId (Id personId) = withFlowHandlerAPI $ do
  person <-
    QP.findPersonById (Id personId)
      >>= fromMaybeM PersonDoesNotExist
  unless (person.organizationId == Just orgId) $ throwError Unauthorized
  DB.runSqlDBTransaction $ do
    QP.deleteById (Id personId)
    QDriverStats.deleteById $ Id personId
    QDriverInformation.deleteById $ Id personId
    QR.deleteByEntitiyId personId
  return $ DeletePersonRes personId

-- Utility Functions

mkPersonRes :: EncFlow m r => SP.Person -> m PersonEntityRes
mkPersonRes SP.Person {..} = do
  decMobNum <- decrypt mobileNumber
  return $
    PersonEntityRes
      { mobileNumber = decMobNum,
        rating = round <$> rating,
        ..
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

calculateAverageRating ::
  (DBFlow m r, EncFlow m r, HasFlowEnv m r '["minimumDriverRatesCount" ::: Int]) =>
  Id SP.Person ->
  m ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratingsSum :: Double = fromIntegral $ sum (allRatings <&> (.ratingValue))
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  minimumDriverRatesCount <- asks (.minimumDriverRatesCount)
  when (ratingCount >= minimumDriverRatesCount) $ do
    let newAverage = ratingsSum / fromIntegral ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
    QP.updateAverageRating personId newAverage

driverPoolKey :: Id ProductInstance -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
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
  ( DBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id SearchReqLocation ->
  Id Organization ->
  SV.Variant ->
  m [(Id Driver, Double)]
calculateDriverPool locId orgId variant = do
  location <- QL.findLocationById locId >>= fromMaybeM LocationNotFound
  let lat = location.lat
      long = location.long
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
