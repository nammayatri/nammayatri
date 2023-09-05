{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RegistryMap where

import Domain.Types.RegistryMapFallback
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Utils.Common
import Storage.CachedQueries.RegistryMapFallback as CQR

updateRegistry :: RegistryMapReq -> Flow APISuccess
updateRegistry regMapReq = do
  _ <- parseBaseUrl regMapReq.registryUrl -- will throw error in case of invalid url
  case (regMapReq.subscriberId, regMapReq.uniqueId) of
    (Just subId, Just uId) -> CQR.updateBySubscriberIdAndUniqueId subId uId regMapReq.registryUrl
    (Just subId, _) -> CQR.updateBySubscriberId subId regMapReq.registryUrl
    (_, Just uId) -> CQR.updateByUniqueId uId regMapReq.registryUrl
    (_, _) -> throwError $ InvalidRequest "UniqueId and SubscriberId Both cannot be null"
  pure Success
