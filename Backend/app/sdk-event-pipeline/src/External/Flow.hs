{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module External.Flow where

import Environment
import External.API.DriverAppAuth
import External.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Utils.Common
import Servant.Client

getDriverId ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "driverAppConfig" r DriverAppConfig,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  m (Either ClientError Text)
getDriverId token = do
  driverAppConfig <- asks (.driverAppConfig)
  resp <- callAPI driverAppConfig.url (driverAppAuth token driverAppConfig.apiKey) "driverAppAuth" driverAppAuthAPI
  return $ case resp of
    Right DriverAppAuthResp {..} -> Right driverId
    Left err -> Left err
