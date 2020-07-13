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

createTransporter :: SR.RegistrationToken -> TransporterReq -> FlowHandler TransporterRes
createTransporter SR.RegistrationToken {..} req = withFlowHandler $ do
  person <- QP.findPersonById (PersonId _EntityId)
  validate person
  organization <- createTransform req
  QO.create organization
  QP.updateOrganizationIdAndMakeAdmin (PersonId _EntityId) (_getOrganizationId $ SO._id organization)
  updatedPerson <- QP.findPersonById (PersonId _EntityId)
  return $ TransporterRes updatedPerson organization
  where
    validate person = do
      unless (SP._verified person) $
        L.throwException $
          err400 {errBody = "user not verified"}
      when (isJust $ SP._organizationId person) $
        L.throwException $
          err400 {errBody = "user already registered an organization"}

createGateway :: SO.Organization -> TransporterReq -> FlowHandler GatewayRes
createGateway _ req = withFlowHandler $ do
  organization <- createTransform req
  QO.create organization
  return $ TransporterRec organization

updateTransporter :: SR.RegistrationToken -> Text -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter SR.RegistrationToken {..} orgId req = withFlowHandler $ do
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId (PersonId _EntityId) SP.ADMIN orgId
  now <- getCurrentTimeUTC
  case maybePerson of
    Just person -> do
      validate person
      org <- QO.findOrganizationById $ OrganizationId orgId
      organization <-
        if req ^. #enabled /= Just False
          then modifyTransform req org >>= addTime (Just now)
          else modifyTransform req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> L.throwException $ err400 {errBody = "user not eligible"}
  where
    validate person =
      unless (SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
    addTime fromTime org =
      return $ org {SO._fromTime = fromTime}

getTransporter :: SR.RegistrationToken -> FlowHandler TransporterRec
getTransporter SR.RegistrationToken {..} = withFlowHandler $ do
  person <- QP.findPersonById (PersonId _EntityId)
  validate person
  case person ^. #_organizationId of
    Just orgId -> TransporterRec <$> QO.findOrganizationById (OrganizationId orgId)
    Nothing -> L.throwException $ err400 {errBody = "user not registered an organization"}
  where
    validate person =
      unless (SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
