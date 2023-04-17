{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.CancellationReason
  ( API,
    handler,
    getCancellationReasons,
  )
where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import Domain.Types.Merchant
import Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (parseBaseUrl)
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CancellationReasons as SCR
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Person as QPerson
import Tools.Auth

type API =
  "get_cancellation_reason"
    :> TokenAuth
    :> MandatoryQueryParam "providerUrl" Text
    :> Get '[JSON] CancellationReasons

handler :: FlowServer API
handler = getCancellationReasons

getCancellationReasons :: (Id Person, Id Merchant) -> Text -> FlowHandler CancellationReasons
getCancellationReasons (personId, _) providerUrlText =
  withFlowHandlerAPI $ do
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    merchant <- QMerchant.findById (person.merchantId) >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
    providerUrl <- parseBaseUrl providerUrlText
    SCR.getCancellationReasons providerUrl merchant.shortId.getShortId
