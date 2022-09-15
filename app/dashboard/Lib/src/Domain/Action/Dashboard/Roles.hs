module Domain.Action.Dashboard.Roles where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified Storage.Queries.Role as QRole
import Tools.Error (RoleError (RoleNameExists))

data CreateRoleReq = CreateRoleReq
  { name :: Text,
    description :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createRole ::
  EsqDBFlow m r =>
  Id DP.Person ->
  CreateRoleReq ->
  m DRole.RoleAPIEntity
createRole _ req = do
  mbExistingRole <- QRole.findByName req.name
  whenJust mbExistingRole $ \_ -> throwError (RoleNameExists req.name)
  role <- buildRole req
  Esq.runTransaction $
    QRole.create role
  pure $ DRole.mkRoleAPIEntity role

buildRole ::
  MonadFlow m =>
  CreateRoleReq ->
  m DRole.Role
buildRole req = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DRole.Role
      { id = uid,
        name = req.name,
        dashboardAccessType = DRole.DASHBOARD_USER,
        description = req.description,
        createdAt = now,
        updatedAt = now
      }
