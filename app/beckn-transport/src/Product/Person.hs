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

updatePerson :: Text -> Maybe Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson personId token req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  verifyPerson personId _EntityId
  person <- QP.findPersonById (PersonId _EntityId)
  updatedPerson <- transformFlow2 req person
  QP.updatePersonRec (PersonId _EntityId) updatedPerson
  return $ UpdatePersonRes updatedPerson
  where
    verifyPerson personId entityId = whenM (return $ personId /= entityId) $ L.throwException $ err400 {errBody = "PERSON_ID_MISMATCH"}

createPerson :: Maybe Text -> CreatePersonReq -> FlowHandler UpdatePersonRes
createPerson token req = withFlowHandler $ do
  orgId <- validate token
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

listPerson :: Maybe Text -> ListPersonReq -> FlowHandler ListPersonRes
listPerson token req = withFlowHandler $ do
  orgId <- validate token
  ListPersonRes <$> QP.findAllWithLimitOffsetByOrgIds (req ^. #_limit) (req ^. #_offset) (req ^. #_roles) [orgId]

getPerson :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler PersonRes
getPerson token idM mobileM emailM identifierM = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth token
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
      whenM
        ( return $
            ((user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id))
              || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ L.throwException
        $ err401 {errBody = "Unauthorized"}

deletePerson :: Text -> Maybe Text -> FlowHandler DeletePersonRes
deletePerson personId token = withFlowHandler $ do
  orgId <- validate token
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
  whenM (return $ (user ^. #_role) /= SP.ADMIN) $ L.throwException $ err400 {errBody = "NEED_ADMIN_ACCESS"}
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}

validate :: Maybe Text -> L.Flow Text
validate token = do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  user <- QP.findPersonById (PersonId _EntityId)
  verifyAdmin user
