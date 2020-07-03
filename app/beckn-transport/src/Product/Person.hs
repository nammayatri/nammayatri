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

updatePerson :: SR.RegistrationToken -> Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson SR.RegistrationToken {..} personId req = withFlowHandler $ do
  verifyPerson personId _EntityId
  person <- QP.findPersonById (PersonId _EntityId)
  updatedPerson <- transformFlow2 req person
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
  person <- addOrgId orgId <$> transformFlow req
  QP.create person
  return $ UpdatePersonRes person
  where
    validateDriver req =
      when (req ^. #_role == Just SP.DRIVER) $
        case req ^. #_mobileNumber of
          Just mobileNumber ->
            whenM (isJust <$> QP.findByMobileNumber mobileNumber) $ L.throwException $ err400 {errBody = "DRIVER_ALREADY_CREATED"}
          Nothing -> L.throwException $ err400 {errBody = "MOBILE_NUMBER_MANDATORY"}

listPerson :: Text -> [SP.Role] -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson orgId roles limitM offsetM =
  withFlowHandler $
    ListPersonRes <$> QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [orgId]

getPerson :: SR.RegistrationToken -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler PersonRes
getPerson SR.RegistrationToken {..} idM mobileM emailM identifierM = withFlowHandler $ do
  user <- QP.findPersonById (PersonId _EntityId)
  person <- case (idM, mobileM, emailM, identifierM) of
    (Nothing, Nothing, Nothing, Nothing) -> L.throwException $ err400 {errBody = "Invalid Request"}
    _ ->
      QP.findByAnyOf idM mobileM emailM identifierM
        >>= fromMaybeM400 "PERSON NOT FOUND"
  hasAccess user person
  return $ PersonRes person
  where
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

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}
