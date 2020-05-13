{-# LANGUAGE OverloadedLabels      #-}
module Product.Person where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Person
import Data.Generics.Labels
import Servant
import Data.Maybe
import qualified EulerHS.Language as L

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
  SR.RegistrationToken {..} <- QR.verifyAuth token
  verifyAdmin (req ^. #_organizationId) _EntityId
  whenM (return $ ((req ^. #_role) == Just SP.DRIVER) && (isNothing $ req ^. #_mobileNumber )) $ L.throwException $ err400 {errBody = "MOBILE_NUMBER_MANDATORY"}
  person <- transformFlow req
  QP.create person
  return $ UpdatePersonRes person

listPerson :: Maybe Text -> ListPersonReq -> FlowHandler ListPersonRes
listPerson token req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  verifyAdmin (Just $ req ^. #_organizationId) _EntityId
  ListPersonRes <$> QP.findAllWithLimitOffsetByOrgIds (req ^. #_limit) (req ^. #_offset) (req ^. #_roles) [req ^. #_organizationId]

verifyAdmin orgIdM entityId = do
  user <- QP.findPersonById (PersonId entityId)
  whenM (return $ SP._role user /= SP.ADMIN) $ L.throwException $ err400 {errBody = "NEED_ADMIN_ACCESS"}
  whenM (return $ orgIdM /= user ^. #_organizationId)  $ L.throwException $ err400 {errBody = "USER_NOT_BELONG_TO_ORGANIZATION"}
