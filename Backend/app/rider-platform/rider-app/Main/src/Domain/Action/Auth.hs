 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Auth (auth) where

import EulerHS.Prelude
import Kernel.InternalAPI.Auth.API
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Utils.Common
import Tools.Auth

auth :: (EsqDBFlow m r, HedisFlow m r, HasField "authTokenCacheExpiry" r Seconds) => Token -> m PersonId
auth token = do
  verifyPerson token <&> (.getId)
