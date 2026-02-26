{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Recon where

import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Servant (JSON, Post, ReqBody, (:>))

-- | Recon request type
type ReconReqV2 = ReconSpec.ReconReq

-- | On_recon request type
type OnReconReqV2 = ReconSpec.OnReconReq

-- | Response type for both recon and on_recon
type ReconRes = AckResponse

-- | BECKN recon API endpoint (BAP -> BPP)
type ReconAPIV2 =
  "recon"
    :> ReqBody '[JSON] ReconReqV2
    :> Post '[JSON] ReconRes

reconAPIV2 :: Proxy ReconAPIV2
reconAPIV2 = Proxy

-- | BECKN on_recon API endpoint (BPP -> BAP)
type OnReconAPIV2 =
  "on_recon"
    :> ReqBody '[JSON] OnReconReqV2
    :> Post '[JSON] ReconRes

onReconAPIV2 :: Proxy OnReconAPIV2
onReconAPIV2 = Proxy
