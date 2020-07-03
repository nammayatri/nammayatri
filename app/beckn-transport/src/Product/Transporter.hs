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

createTransporter :: RegToken -> TransporterReq -> FlowHandler TransporterRes
createTransporter regToken req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  person <- QP.findPersonById (PersonId _EntityId)
  validate person
  organization <- transformFlow req
  QO.create organization
  QP.updateOrganizationIdAndMakeAdmin (PersonId _EntityId) (_getOrganizationId $ SO._id organization)
  updatedPerson <- QP.findPersonById (PersonId _EntityId)
  return $ TransporterRes updatedPerson organization
  where
    validate person = do
      unless (SP._verified person)
        $ L.throwException
        $ err400 {errBody = "user not verified"}
      when (isJust $ SP._organizationId person)
        $ L.throwException
        $ err400 {errBody = "user already registered an organization"}

createGateway :: RegToken -> TransporterReq -> FlowHandler GatewayRes
createGateway regToken req = withFlowHandler $ do
  QO.verifyToken regToken
  organization <- transformFlow req
  QO.create organization
  return $ TransporterRec organization

updateTransporter :: Text -> RegToken -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter orgId regToken req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId (PersonId _EntityId) SP.ADMIN orgId
  now <- getCurrentTimeUTC
  case maybePerson of
    Just person -> do
      validate person
      org <- QO.findOrganizationById $ OrganizationId orgId
      organization <-
        if req ^. #enabled /= Just False
          then transformFlow2 req org >>= addTime (Just now)
          else transformFlow2 req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> L.throwException $ err400 {errBody = "user not eligible"}
  where
    validate person = do
      unless (SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
    addTime fromTime org =
      return $ org {SO._fromTime = fromTime}

getTransporter :: RegToken -> FlowHandler TransporterRec
getTransporter regToken = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  person <- QP.findPersonById (PersonId _EntityId)
  validate person
  case person ^. #_organizationId of
    Just orgId -> TransporterRec <$> QO.findOrganizationById (OrganizationId orgId)
    Nothing -> L.throwException $ err400 {errBody = "user not registered an organization"}
  where
    validate person =
      unless (SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
