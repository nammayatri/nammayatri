module Domain.Action.Dashboard.Person where

import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common (fromMaybeM)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Role as QRole
import Tools.Error

newtype ListPersonRes = ListPersonRes
  {list :: [DP.PersonAPIEntity]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

listPerson ::
  (EsqDBFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListPersonRes
listPerson _ mbSearchString mbLimit mbOffset = do
  personAndRoleList <- QP.findAllWithLimitOffset mbSearchString mbLimit mbOffset
  res <- forM personAndRoleList $ \(encPerson, role) -> do
    decPerson <- decrypt encPerson
    pure $ DP.makePersonAPIEntity decPerson role
  pure $ ListPersonRes res

assignRole ::
  EsqDBFlow m r =>
  Id DP.Person ->
  Id DP.Person ->
  Id DRole.Role ->
  m APISuccess
assignRole _ personId roleId = do
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  _role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  Esq.runTransaction $
    QP.updatePersonRole personId roleId
  pure Success
