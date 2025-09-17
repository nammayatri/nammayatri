{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.GPS
  ( API,
    handler,
  )
where

import Data.OpenApi (ToSchema)
import Domain.Action.Internal.GPS as DInternal
import Environment
import EulerHS.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common as Common
import Servant

type API =
  "internal"
    :> "fleet"
    :> "drivers-by-plates"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] DriversByPlatesReq
    :> Post '[JSON] [DInternal.DriverByPlateResp]

newtype DriversByPlatesReq = DriversByPlatesReq
  { plateNumbers :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

handler :: FlowServer API
handler = driversByPlatesHandler

driversByPlatesHandler :: Maybe Text -> DriversByPlatesReq -> FlowHandler [DInternal.DriverByPlateResp]
driversByPlatesHandler mbApiKey req = withFlowHandlerAPI $ do
  case mbApiKey of
    Nothing -> Common.throwError $ InvalidRequest "Missing X-API-Key header"
    Just apiKey -> do
      when (apiKey == "") $ Common.throwError $ InvalidRequest "Invalid API key"
      DInternal.getDriversByPlates apiKey req.plateNumbers
