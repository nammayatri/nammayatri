{-# LANGUAGE OverloadedLabels #-}

module Product.Transporter where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Transporter
import Types.App

createTransporter :: Maybe Text -> TransporterReq -> FlowHandler TransporterRes
createTransporter auth req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth auth
  person <- QP.findPersonById (PersonId _EntityId)
  validation person
  organization <- transformFlow req
  QO.create organization
  QP.updateOrganizationIdAndMakeAdmin (PersonId _EntityId) (_getOrganizationId $ SO._id organization)
  updatedPerson <- QP.findPersonById (PersonId _EntityId)
  return $ TransporterRes updatedPerson organization
  where
    validation person = do
      whenM (return $ not $ SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
      whenM (return $ SP._organizationId person /= Nothing) $ L.throwException $ err400 {errBody = "user already registered an organization"}

createGateway :: Maybe Text -> TransporterReq -> FlowHandler GatewayRes
createGateway auth req = withFlowHandler $ do
  QO.verifyAuth auth
  organization <- transformFlow req
  QO.create organization
  return $ TransporterRec organization

updateTransporter :: Text -> Maybe Text -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter orgId auth req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth auth
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId (PersonId _EntityId) SP.ADMIN orgId
  now <- getCurrentTimeUTC
  case maybePerson of
    Just person -> do
      validation person
      org <- QO.findOrganizationById $ OrganizationId orgId
      organization <- if not (fromMaybe True (req ^. #enabled))
                        then transformFlow2 req org >>= addTime (Just now)
                        else transformFlow2 req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> L.throwException $ err400 {errBody = "user not eligible"}
  where
    validation person = do
      whenM (return $ not $ SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
    addTime fromTime org =
      return $ org {SO._fromTime = fromTime}

getTransporter :: Maybe Text -> FlowHandler TransporterRec
getTransporter auth = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth auth
  person <- QP.findPersonById (PersonId _EntityId)
  validation person
  org <- QO.findOrganizationById (OrganizationId (fetchMaybeValue $ person ^. #_organizationId))
  return $ TransporterRec org
  where
    validation person = do
      whenM (return $ not $ SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
      whenM (return $ SP._organizationId person == Nothing) $ L.throwException $ err400 {errBody = "user not registered an organization"}
