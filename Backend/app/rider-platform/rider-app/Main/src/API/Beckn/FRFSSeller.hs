{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Seller-side ONDC:TRV11 route tree.
--
-- rider-app is a buyer on this network ("API.FRFS" mounts the @on_*@ callbacks it
-- receives). These routes are the other half of the same protocol: the requests a
-- buyer app sends US when we sell metro tickets. The request types have always
-- existed in @BecknV2.FRFS.APIs@ — they were simply never mounted.
--
-- The path is deliberately distinct from the buyer's @beckn/frfs/v1@: both are
-- registered with the ONDC registry against different subscriber ids, and the
-- registry entry is keyed on the URL.
module API.Beckn.FRFSSeller
  ( API,
    handler,
  )
where

import qualified API.Beckn.FRFSSeller.Search as Search
import Environment
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "beckn" :> "frfs-seller" :> "v1"
    :> SignatureAuth 'Domain.PUBLIC_TRANSPORT "Authorization"
    :> Search.API

handler :: FlowServer API
handler = Search.handler
