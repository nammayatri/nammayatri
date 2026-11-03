{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Processor.FleetAnalytics.Realtime
  ( processFleetAnalytics,
  )
where

import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import "config-pilot" Lib.ConfigPilot.Interface.Types (getConfig)
import qualified "dynamic-offer-driver-app" SharedLogic.Analytics as Analytics
import "dynamic-offer-driver-app" SharedLogic.FleetAnalytics.Realtime (FleetRealtimeEvent (..))
import "dynamic-offer-driver-app" Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

processFleetAnalytics :: FleetRealtimeEvent Analytics.FleetOperatorAnalytics -> Text -> Flow ()
processFleetAnalytics event _key = do
  transporterConfig <-
    getConfig (TransporterConfigDimensions {merchantOperatingCityId = event.merchantOperatingCityId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound event.merchantOperatingCityId)
  Analytics.applyPublishedFleetAnalytics transporterConfig event
