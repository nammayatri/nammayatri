module Product.Transporter where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import qualified Domain.Types.Organization as SO
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import Types.API.Transporter
import Types.Error
import Utils.Common

updateTransporter :: SP.Person -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler UpdateTransporterRes
updateTransporter admin orgId req = withFlowHandlerAPI $ do
  unless (Just orgId == admin.organizationId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  org <-
    QO.findById orgId
      >>= fromMaybeM (OrgDoesNotExist orgId.getId)
  let updOrg =
        org{SO.name = fromMaybe (org.name) (req.name),
            SO.description = (req.description) <|> (org.description),
            SO.enabled = fromMaybe (org.enabled) (req.enabled)
           }
  Esq.runTransaction $ QO.updateOrganizationRec updOrg
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateTransporter : ") (show updOrg)
  return $ SO.makeOrganizationAPIEntity updOrg

getTransporter :: Id SP.Person -> FlowHandler TransporterRec
getTransporter personId = withFlowHandlerAPI $ do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  case person.organizationId of
    Just orgId -> TransporterRec . SO.makeOrganizationAPIEntity <$> (QO.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId))
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
