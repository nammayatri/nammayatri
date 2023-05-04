{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.MultipleRideEnd where

import qualified Domain.Action.Dashboard.MultipleRideEnd as DMR
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data MultipleRideEndEndpoint
  = MultipleRideEndEndpoint
  deriving (Show, Read)

derivePersistField "MultipleRideEndEndpoint"

---------------------------------------------------------
-- Ride cancel --------------------------------------

type API =
  "end"
    :> ReqBody '[JSON] DMR.MultipleRideEndReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = multipleRideEnd

multipleRideEnd ::
  DMR.MultipleRideEndReq ->
  FlowHandler APISuccess
multipleRideEnd req = withFlowHandlerAPI $ DMR.multipleRideEnd req
