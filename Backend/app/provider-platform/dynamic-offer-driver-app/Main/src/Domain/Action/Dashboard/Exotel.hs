{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Exotel
  ( exotelHeartbeat,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Exotel as Common
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Exophone as CQExophone

---------------------------------------------------------------------
exotelHeartbeat ::
  Common.ExotelHeartbeatReq ->
  Flow APISuccess
exotelHeartbeat req = do
  let affectedPhones = (req.incomingAffected <&> (.phoneNumber)) <> (req.outgoingAffected <&> (.phoneNumber))
  Esq.runTransactionF $ \finalize -> do
    CQExophone.updateAffectedPhones finalize affectedPhones
  logTagInfo "dashboard -> exotelHeartbeat: " $ show req.statusType
  pure Success
