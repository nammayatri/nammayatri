{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Person
  ( createPerson,
    listPerson,
    updatePerson,
    getPerson,
    deletePerson,
    linkEntity,
    calculateAverageRating,
  )
where

import App.Types
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config
import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.Rating as Rating
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Data.Maybe
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Vehicle as QV
import Types.API.Person
import Types.App (DriverId (..))
import qualified Types.Storage.DriverInformation as DriverInformation

updatePerson :: SR.RegistrationToken -> Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson SR.RegistrationToken {..} personId req = withFlowHandler $ do
  verifyPerson _EntityId
  person <- QP.findPersonById (PersonId _EntityId)
  isValidUpdate person
  updatedPerson <- modifyTransform req person
  QP.updatePersonRec (PersonId _EntityId) updatedPerson
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
  when (person ^. #_role == SP.DRIVER) $ mkDriver (person ^. #_id) >>= QDriverInformation.create
  org <- OQ.findOrganizationById (OrganizationId orgId)
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
    mkDriver personId = do
      now <- getCurrTime
      pure
        DriverInformation.DriverInformation
          { _driverId = DriverId $ _getPersonId personId,
            _active = False,
            _createdAt = now,
            _updatedAt = now
          }

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
    user <- QP.findPersonById (PersonId _EntityId)
    -- TODO: fix this to match based on identifierType
    -- And maybe have a way to handle the case when ID is
    -- passed and identifierType is null. Throw validation errors
    person <- case identifierTypeM of
      Nothing -> QP.findPersonById (PersonId $ fromJust idM)
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
  person <- QP.findPersonById (PersonId personId)
  if person ^. #_organizationId == Just orgId
    then do
      QP.deleteById (PersonId personId)
      QDriverInformation.deleteById $ DriverId personId
      QR.deleteByEntitiyId personId
      return $ DeletePersonRes personId
    else throwError401 "Unauthorized"

linkEntity :: Text -> Text -> LinkReq -> FlowHandler PersonEntityRes
linkEntity orgId personId req = withFlowHandler $ do
  person <- QP.findPersonById (PersonId personId)
  _ <- case req ^. #_entityType of
    VEHICLE ->
      QV.findVehicleById (VehicleId (req ^. #_entityId))
        >>= fromMaybeM400 "VEHICLE NOT REGISTERED"
    _ -> throwError400 "UNSUPPORTED ENTITY TYPE"
  when
    (person ^. #_organizationId /= Just orgId)
    (throwError401 "Unauthorized")
  prevPerson <- QP.findByEntityId (req ^. #_entityId)
  whenJust prevPerson (\p -> QP.updateEntity (p ^. #_id) T.empty T.empty)
  QP.updateEntity (PersonId personId) (req ^. #_entityId) (T.pack $ show $ req ^. #_entityType)
  updatedPerson <- QP.findPersonById $ person ^. #_id
  mkPersonRes updatedPerson

-- Utility Functions

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}

mkPersonRes :: SP.Person -> Flow PersonEntityRes
mkPersonRes person = do
  entity <- case person ^. #_udf2 >>= mapEntityType of
    Just VEHICLE -> do
      vehicle <- QV.findVehicleById $ VehicleId $ fromMaybe "" (person ^. #_udf1)
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

calculateAverageRating :: PersonId -> Flow ()
calculateAverageRating personId = do
  L.logInfo @Text "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratings = sum $ Rating._ratingValue <$> allRatings
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    L.logInfo @Text "PersonAPI" "No rating found to calculate"
  when (ratingCount > 0) $ do
    let newAverage = ratings `div` ratingCount
    L.logInfo @Text "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
    QP.updateAverageRating personId $ encodeToText newAverage
