{-# LANGUAGE OverloadedLabels #-}

module Product.Person where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Data.Maybe
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Vehicle as QV
import Types.API.Person

updatePerson :: SR.RegistrationToken -> Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson SR.RegistrationToken {..} personId req = withFlowHandler $ do
  verifyPerson personId _EntityId
  person <- QP.findPersonById (PersonId _EntityId)
  updatedPerson <- modifyTransform req person
  QP.updatePersonRec (PersonId _EntityId) updatedPerson
  return $ UpdatePersonRes updatedPerson
  where
    verifyPerson personId entityId =
      when (personId /= entityId) $
        L.throwException $
          err400 {errBody = "PERSON_ID_MISMATCH"}

createPerson :: Text -> CreatePersonReq -> FlowHandler UpdatePersonRes
createPerson orgId req = withFlowHandler $ do
  validateDriver req
  person <- addOrgId orgId <$> createTransform req
  QP.create person
  return $ UpdatePersonRes person
  where
    validateDriver :: CreatePersonReq -> Flow ()
    validateDriver req =
      when (req ^. #_role == Just SP.DRIVER) $
        case (req ^. #_mobileNumber, req ^. #_mobileCountryCode) of
          (Just mobileNumber, Just countryCode) ->
            whenM (isJust <$> QP.findByMobileNumber countryCode mobileNumber) $
              L.throwException $
                err400 {errBody = "DRIVER_ALREADY_CREATED"}
          _ -> L.throwException $ err400 {errBody = "MOBILE_NUMBER_AND_COUNTRY_CODE_MANDATORY"}

listPerson :: Text -> [SP.Role] -> Maybe EntityType -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson orgId roles entityType limitM offsetM = withFlowHandler $ do
  personList <- QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [orgId]
  respList <- traverse (mkPersonRes entityType) personList
  return $ ListPersonRes respList

getPerson ::
  SR.RegistrationToken ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe SP.IdentifierType ->
  FlowHandler PersonRes
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
    return $ PersonRes person
  where
    hasAccess :: SP.Person -> SP.Person -> Flow ()
    hasAccess user person =
      when
        ( (user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id)
            || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ L.throwException $
          err401 {errBody = "Unauthorized"}

deletePerson :: Text -> Text -> FlowHandler DeletePersonRes
deletePerson orgId personId = withFlowHandler $ do
  person <- QP.findPersonById (PersonId personId)
  if person ^. #_organizationId == Just orgId
    then do
      QP.deleteById (PersonId personId)
      QR.deleteByEntitiyId personId
      return $ DeletePersonRes personId
    else L.throwException $ err401 {errBody = "Unauthorized"}

linkEntity :: Text -> Text -> LinkReq -> FlowHandler PersonRes
linkEntity orgId personId req = withFlowHandler $ do
  person <- QP.findPersonById (PersonId personId)
  vehicle <-
    QV.findVehicleById (VehicleId (req ^. #_vehicleId))
      >>= fromMaybeM400 "VEHICLE NOT REGISTERED"
  when
    (person ^. #_organizationId /= Just orgId)
    (L.throwException $ err401 {errBody = "Unauthorized"})
  QP.updateEntity (PersonId personId) (req ^. #_vehicleId)
  return $ PersonRes $ person {SP._udf1 = Just (req ^. #_vehicleId)}

-- Utility Functions

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}

mkPersonRes :: Maybe EntityType -> SP.Person -> Flow PersonEntityRes
mkPersonRes entityType person = do
  entity <- case entityType of
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
