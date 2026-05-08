{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Mounts the AWS-egress forwarder under the @/internal/@ namespace.
-- Reads forwarder config and the shared 'Http.Manager' off 'AppEnv'.
module API.Internal.MasterCloudForward
  ( API,
    handler,
  )
where

import Environment
import qualified Kernel.External.MasterCloudForward as MCF
import Servant

type API = MCF.ForwardAPI

handler :: AppEnv -> FlowServer API
handler env _mbSecret _mbDest =
  Tagged (MCF.forwardEgressApp env.masterCloudProxyConfig env.masterCloudForwarderManager)
