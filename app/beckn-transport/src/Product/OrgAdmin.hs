module Product.OrgAdmin where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Esqueleto as Esq
import Data.Maybe
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.OrgAdmin as API
import Types.Error
import Utils.Common

getProfile :: SP.Person -> FlowHandler API.OrgAdminProfileRes
getProfile admin = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  org <- QOrg.findById orgId >>= fromMaybeM OrgNotFound
  decAdmin <- decrypt admin
  let personAPIEntity = SP.makePersonAPIEntity decAdmin
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
  Esq.runTransaction $
    QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QOrg.findById orgId >>= fromMaybeM OrgNotFound
  decUpdAdmin <- decrypt updAdmin
  let personAPIEntity = SP.makePersonAPIEntity decUpdAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

makeOrgAdminProfileRes :: SP.PersonAPIEntity -> Org.OrganizationAPIEntity -> API.OrgAdminProfileRes
makeOrgAdminProfileRes SP.PersonAPIEntity {..} org =
  API.OrgAdminProfileRes
    { organization = org,
      ..
    }
