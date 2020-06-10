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
      if (req ^. #_role == Just SP.DRIVER)
        then do
          let mobileNumber = fromMaybe "MOBILE_NUMBER_NULL" (req ^. #_mobileNumber)
          whenM (return $ mobileNumber == "MOBILE_NUMBER_NULL") $ L.throwException $ err400 {errBody = "MOBILE_NUMBER_MANDATORY"}
          whenM (isJust <$> (QP.findByMobileNumber mobileNumber)) $ L.throwException $ err400 {errBody = "DRIVER_ALREADY_CREATED"}
        else return ()

listPerson :: Maybe Text -> [SP.Role] -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson token roles limitM offsetM = withFlowHandler $ do
  orgId <- validate token
  ListPersonRes <$> QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [orgId]

-- Core Utility methods
verifyAdmin :: SP.Person -> L.Flow Text
verifyAdmin user = do
  whenM (return $ (user ^. #_role) /= SP.ADMIN) $ L.throwException $ err400 {errBody = "NEED_ADMIN_ACCESS"}
  let mOrgId = user ^. #_organizationId
  whenM (return $ isNothing mOrgId) $ L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}
  return $ fromMaybe "NEVER_SHOULD_BE_HERE" mOrgId

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}

validate :: Maybe Text -> L.Flow Text
validate token = do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  user <- QP.findPersonById (PersonId _EntityId)
  verifyAdmin user
