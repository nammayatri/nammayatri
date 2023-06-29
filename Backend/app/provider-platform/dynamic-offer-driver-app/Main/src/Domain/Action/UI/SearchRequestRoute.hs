{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SearchRequestRoute
  ( searchRequestRoute,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SearchRequest
import Kernel.External.Encryption
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto (runInReplica, runTransaction)
-- import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Person ()

-- import Tools.Error

searchRequestRoute :: (EsqDBFlow m r, EncFlow m r) => Id SearchRequest.SearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> m APISuccess.APISuccess
searchRequestRoute _ _ = do
  --   SearchRequestRouteDetails <- runInReplica $ QSearchRequestRoute.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)

  --   unless (personId == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"

  --   runTransaction $ QSos.updateStatus sosId (req.status)
  pure APISuccess.Success
