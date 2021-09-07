module Product.OrgAdmin where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Queries as DB
import Data.Maybe
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.OrgAdmin as API
import Types.Error
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as SP
import Utils.Common

getProfile :: SP.Person -> FlowHandler API.OrgAdminProfileRes
getProfile admin = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  org <- QOrg.findOrganizationById orgId >>= fromMaybeM OrgNotFound
  decAdmin <- decrypt admin
  return $ makeOrgAdminProfileRes (SP.makePersonAPIEntity decAdmin) (Org.makeOrganizationAPIEntity org)

updateProfile :: SP.Person -> API.UpdateOrgAdminProfileReq -> FlowHandler API.UpdateOrgAdminProfileRes
updateProfile admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
      updAdmin =
        admin{firstName = if isJust req.firstName then req.firstName else admin.firstName,
              middleName = if isJust req.middleName then req.middleName else admin.middleName,
              lastName = if isJust req.lastName then req.lastName else admin.lastName,
              deviceToken = if isJust req.deviceToken then req.deviceToken else admin.deviceToken
             }
  DB.runSqlDBTransaction $
    QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QOrg.findOrganizationById orgId >>= fromMaybeM OrgNotFound
  decUpdAdmin <- decrypt updAdmin
  return $ makeOrgAdminProfileRes (SP.makePersonAPIEntity decUpdAdmin) (Org.makeOrganizationAPIEntity org)

makeOrgAdminProfileRes :: SP.PersonAPIEntity -> Org.OrganizationAPIEntity -> API.OrgAdminProfileRes
makeOrgAdminProfileRes SP.PersonAPIEntity {..} org =
  API.OrgAdminProfileRes
    { organization = org,
      ..
    }