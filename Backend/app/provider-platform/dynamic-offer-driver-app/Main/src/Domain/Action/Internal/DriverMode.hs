{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.DriverMode where

import Data.OpenApi (ToSchema)
import Domain.Action.UI.Driver (processingChangeOnline)
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverInformation as SQDI
import qualified Storage.Queries.Person as QPerson
import Tools.Error (DriverInformationError (DriverInfoNotFound))

data DriverModeReq = DriverModeReq
  { driverId :: Id DP.Person,
    mode :: DriverInfo.DriverMode,
    isActive :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

setDriverMode :: (HasField "locationTrackingServiceKey" AppEnv Text) => Maybe Text -> DriverModeReq -> Flow APISuccess
setDriverMode apiKey req = do
  let driverId = req.driverId
      mode = req.mode
      isActive = req.isActive
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ do
    throwError $ InvalidRequest "Invalid API key"
  void $ do
    QDriverInformation.updateActivity isActive (Just mode) driverId
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
    transporterConfig <-
      SCTC.findByMerchantOpCityId driver.merchantOperatingCityId Nothing
        >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
    driverInfo <- SQDI.findByPrimaryKey driverId >>= fromMaybeM DriverInfoNotFound
    processingChangeOnline (driverId, driver.merchantId, driver.merchantOperatingCityId) transporterConfig.timeDiffFromUtc driverInfo $ Just mode
  pure Success
