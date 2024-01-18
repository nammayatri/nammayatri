{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Roles where

import Dashboard.Common
import "lib-dashboard" Domain.Action.Dashboard.Roles (ListRoleRes (..))
import "lib-dashboard" Domain.Types.Role
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Common
import "lib-dashboard" Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth

listRoles ::
  ( BeamFlow m r,
    EncFlow m r
  ) =>
  TokenInfo ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListRoleRes
listRoles _ mbSearchString mbLimit mbOffset = do
  personAndRoleList <- B.runInReplica $ QRole.findAllWithLimitOffset mbLimit mbOffset mbSearchString
  res <- forM personAndRoleList $ \role -> do
    pure $ mkRoleAPIEntity role
  let count = length res
  let summary = Summary {totalCount = 10000, count}
  pure $ ListRoleRes {list = res, summary = summary}
