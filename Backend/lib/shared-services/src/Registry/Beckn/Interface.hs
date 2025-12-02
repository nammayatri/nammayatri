{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Registry.Beckn.Interface where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Utils.Common
import qualified Registry.Beckn.Interface.Nammayatri as Ny
import Registry.Beckn.Interface.Types
import qualified Registry.Beckn.Nammayatri.Flow as Flow
import Registry.Beckn.Nammayatri.Types as NyRegistry

updateSubscriber :: (MonadFlow m, CoreMetrics m, HasRequestId r, MonadReader r m) => UpdateSubscriberReq -> m UpdateSubscriberResp
updateSubscriber updSubReq = case updSubReq._data of
  UpdSubNY _data -> UpdSubResNY <$> Ny.updateCities updSubReq _data

createSubscriber :: (MonadFlow m, CoreMetrics m, HasRequestId r, MonadReader r m) => BaseUrl -> NyRegistry.Subscriber -> m AckResponse
createSubscriber = Flow.createSubscriber
