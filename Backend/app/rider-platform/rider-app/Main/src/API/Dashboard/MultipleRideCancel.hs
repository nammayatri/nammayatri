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

module API.Dashboard.MultipleRideCancel where

import qualified Domain.Action.Dashboard.MultipleRideCancel as DMR
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Summary)

data MultipleRideCancelEndpoint
  = MultipleRideCancelEndpoint
  deriving (Show, Read)

derivePersistField "MultipleRideCancelEndpoint"

---------------------------------------------------------
-- Ride cancel --------------------------------------

type API =
  "cancel"
    :> ReqBody '[JSON] DMR.MultipleRideCancelReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = multipleRideCancel

multipleRideCancel ::
  DMR.MultipleRideCancelReq ->
  FlowHandler APISuccess
multipleRideCancel req = withFlowHandlerAPI $ DMR.multipleRideCancel req
