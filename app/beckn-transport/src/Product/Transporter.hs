module Product.Transporter where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import Types.API.Transporter
import Types.Error
import qualified Types.Storage.Organization as SO
import qualified Types.Storage.Person as SP
import Utils.Common

updateTransporter :: Id SP.Person -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter personId orgId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateTransporterReq req
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId personId SP.ADMIN orgId
  now <- getCurrentTime
  case maybePerson of
    Just _ -> do
      org <-
        QO.findOrganizationById orgId
          >>= fromMaybeM OrgDoesNotExist
      organization <-
        if req.enabled /= Just False
          then modifyOrganization req org >>= addTime (Just now)
          else modifyOrganization req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> throwError PersonDoesNotExist
  where
    addTime fromTime org =
      return $ org {SO.fromTime = fromTime}

getTransporter :: Id SP.Person -> FlowHandler TransporterRec
getTransporter personId = withFlowHandlerAPI $ do
  person <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  case person.organizationId of
    Just orgId -> TransporterRec <$> (QO.findOrganizationById orgId >>= fromMaybeM OrgNotFound)
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
