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
  personAPIEntity <- SP.buildPersonAPIEntity decAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

updateProfile :: SP.Person -> API.UpdateOrgAdminProfileReq -> FlowHandler API.UpdateOrgAdminProfileRes
updateProfile admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
      updAdmin =
        admin{firstName = req.firstName <|> admin.firstName,
              middleName = req.middleName <|> admin.middleName,
              lastName = req.lastName <|> admin.lastName,
              deviceToken = req.deviceToken <|> admin.deviceToken
             }
  DB.runSqlDBTransaction $
    QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QOrg.findOrganizationById orgId >>= fromMaybeM OrgNotFound
  decUpdAdmin <- decrypt updAdmin
  personAPIEntity <- SP.buildPersonAPIEntity decUpdAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

makeOrgAdminProfileRes :: SP.PersonAPIEntity -> Org.OrganizationAPIEntity -> API.OrgAdminProfileRes
makeOrgAdminProfileRes SP.PersonAPIEntity {..} org =
  API.OrgAdminProfileRes
    { organization = org,
      ..
    }