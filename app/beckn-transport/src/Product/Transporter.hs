module Product.Transporter where

import App.Types
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

updateTransporter :: SP.Person -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler UpdateTransporterRes
updateTransporter admin orgId req = withFlowHandlerAPI $ do
  unless (Just orgId == admin.organizationId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  org <-
    QO.findOrganizationById orgId
      >>= fromMaybeM OrgDoesNotExist
  let updOrg =
        org{SO.name = fromMaybe (org.name) (req.name),
            SO.description = (req.description) <|> (org.description),
            SO.enabled = fromMaybe (org.enabled) (req.enabled)
           }
  QO.updateOrganizationRec updOrg
  return $ SO.makeOrganizationAPIEntity updOrg

getTransporter :: Id SP.Person -> FlowHandler TransporterRec
getTransporter personId = withFlowHandlerAPI $ do
  person <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  case person.organizationId of
    Just orgId -> TransporterRec . SO.makeOrganizationAPIEntity <$> (QO.findOrganizationById orgId >>= fromMaybeM OrgNotFound)
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
