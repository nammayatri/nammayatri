{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Registry.Beckn.Interface.Nammayatri where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Utils.Common
import qualified Registry.Beckn.Interface.Types as Types
import qualified Registry.Beckn.Nammayatri.Flow as Flow
import Registry.Beckn.Nammayatri.Types

updateCities :: (MonadFlow m, CoreMetrics m, HasRequestId r, MonadReader r m) => Types.UpdateSubscriberReq -> UpdData -> m UpdateCitiesRes
updateCities Types.RegistryReq {..} updData =
  Flow.updateCities updData.apiKey registryUrl buildUpdCitiesReq
  where
    buildUpdCitiesReq =
      UpdateCitiesReq
        { appendCities = updData.appendCities <&> mapMaybe (A.decode . A.encode),
          replaceCities = updData.replaceCities <&> mapMaybe (A.decode . A.encode),
          ..
        }
