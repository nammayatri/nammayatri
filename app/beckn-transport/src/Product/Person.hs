{-# LANGUAGE OverloadedLabels #-}

module Product.Person where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Data.Generics.Labels
import Data.Maybe
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Person

updatePerson :: Text -> RegToken -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson personId regToken req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  verifyPerson personId _EntityId
  person <- QP.findPersonById (PersonId _EntityId)
  updatedPerson <- transformFlow2 req person
  QP.updatePersonRec (PersonId _EntityId) updatedPerson
  return $ UpdatePersonRes updatedPerson
  where
    verifyPerson personId entityId =
      when (personId /= entityId)
        $ L.throwException
        $ err400 {errBody = "PERSON_ID_MISMATCH"}

createPerson :: RegToken -> CreatePersonReq -> FlowHandler UpdatePersonRes
createPerson regToken req = withFlowHandler $ do
  orgId <- validate regToken
  validateDriver req
  person <- addOrgId orgId <$> transformFlow req
  QP.create person
  return $ UpdatePersonRes person
  where
    validateDriver req =
      when (req ^. #_role == Just SP.DRIVER) $
        case (req ^. #_mobileNumber, req ^. #_mobileCountryCode) of
          (Just mobileNumber, Just countryCode) ->
            whenM (isJust <$> QP.findByMobileNumber countryCode mobileNumber)
              $ L.throwException
              $ err400 {errBody = "DRIVER_ALREADY_CREATED"}
          _ -> L.throwException $ err400 {errBody = "MOBILE_NUMBER_AND_COUNTRY_CODE_MANDATORY"}

listPerson :: RegToken -> [SP.Role] -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson regToken roles limitM offsetM = withFlowHandler $ do
  orgId <- validate regToken
  ListPersonRes <$> QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [orgId]

getPerson ::
  RegToken ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe SP.IdentifierType ->
  FlowHandler PersonRes
getPerson regToken idM mobileM countryCodeM emailM identifierM identifierTypeM =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- QR.verifyToken regToken
    user <- QP.findPersonById (PersonId _EntityId)
    -- TODO: fix this to match based on identifierType
    -- And maybe have a way to handle the case when ID is
    -- passed and identifierType is null. Throw validation errors
    person <- case identifierTypeM of
      Nothing -> QP.findPersonById (PersonId $ fromJust idM)
      Just SP.MOBILENUMBER ->
        QP.findByMobileNumber
          (fromJust countryCodeM)
          (fromJust mobileM)
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
      Just SP.EMAIL ->
        QP.findByEmail (fromJust emailM)
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
      Just SP.AADHAAR ->
        QP.findByIdentifier (fromJust identifierM)
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
    hasAccess user person
    return $ PersonRes person
  where
    hasAccess user person =
      when
        ( (user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id)
            || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ L.throwException
        $ err401 {errBody = "Unauthorized"}

deletePerson :: Text -> RegToken -> FlowHandler DeletePersonRes
deletePerson personId regToken = withFlowHandler $ do
  orgId <- validate regToken
  person <- QP.findPersonById (PersonId personId)
  if person ^. #_organizationId == Just orgId
    then do
      QP.deleteById (PersonId personId)
      QR.deleteByEntitiyId personId
      return $ DeletePersonRes personId
    else L.throwException $ err401 {errBody = "Unauthorized"}

-- Core Utility methods
verifyAdmin :: SP.Person -> L.Flow Text
verifyAdmin user = do
  when (user ^. #_role /= SP.ADMIN) $ L.throwException $
    err400 {errBody = "NEED_ADMIN_ACCESS"}
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing ->
      L.throwException $
        err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}

validate :: RegToken -> L.Flow Text
validate regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (PersonId _EntityId)
  verifyAdmin user

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}
