{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Whatsapp (module Domain.Action.UI.Whatsapp, module Reexport) where

import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.External.Whatsapp.Interface.Types as Reexport
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import Tools.Metrics (CoreMetrics)
import Tools.Whatsapp as Whatsapp

newtype OptAPIRequest = OptAPIRequest
  { status :: OptApiMethods
  }
  deriving (Show, Eq, Generic, ToSchema, FromJSON, ToJSON)

whatsAppOptAPI :: forall m r. (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DP.Person -> OptAPIRequest -> m APISuccess
whatsAppOptAPI personId OptAPIRequest {..} = do
  DP.Person {..} <- QP.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNo <- mapM decrypt mobileNumber >>= fromMaybeM (InvalidRequest "Person is not linked with any mobile number")
  unless (whatsappNotificationEnrollStatus == Just status) $
    void $ Whatsapp.whatsAppOptAPI merchantId $ OptApiReq {phoneNumber = mobileNo, method = status}
  DB.runTransaction $
    void $ QP.updateWhatsappNotificationEnrollStatus @m personId $ Just status
  return Success
