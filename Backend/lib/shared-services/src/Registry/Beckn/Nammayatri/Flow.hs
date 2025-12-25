{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Registry.Beckn.Nammayatri.Flow where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Registry.Beckn.Nammayatri.Types

updateCities :: (MonadFlow m, CoreMetrics m, HasRequestId r, MonadReader r m) => Text -> BaseUrl -> UpdateCitiesReq -> m UpdateCitiesRes
updateCities apiKey url req = do
  callAPI url (updateCitiesClient (Just apiKey) req) "updateCitiesNammayatriRegistry" updateCitiesAPI
    >>= fromEitherM (ExternalAPICallError (Just "UPDATE_CITIES_NAMMAYATRI_REGISTRY_FAILED") url)

createSubscriber :: (MonadFlow m, CoreMetrics m, HasRequestId r, MonadReader r m) => BaseUrl -> Subscriber -> m AckResponse
createSubscriber url req = do
  callAPI url (createSubscriberClient req) "createSubscriberNammayatriRegistry" createAPI
    >>= fromEitherM (ExternalAPICallError (Just "CREATE_SUBSCRIBER_NAMMAYATRI_REGISTRY_FAILED") url)
