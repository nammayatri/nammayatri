{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSearch (API, handler) where

import qualified Beckn.ACL.Bus.OnSearch as BusACL
import qualified Beckn.ACL.OnSearch as TaxiACL
import Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Tools.Error

type API =
  SignatureAuth "X-Gateway-Authorization"
    :> OnSearch.OnSearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
onSearch _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  transactionId <- fromMaybeM (InvalidRequest "Missing transaction_id") req.context.transaction_id
  mbDOnSearchReq <- buildOnSearchReq req
  whenJust mbDOnSearchReq $ \request -> do
    Redis.whenWithLockRedis (onSearchLockKey req.context.message_id) 60 $ do
      validatedRequest <- DOnSearch.validateRequest request
      fork "on search processing" $ do
        Redis.whenWithLockRedis (onSearchProcessingLockKey req.context.message_id) 60 $
          DOnSearch.onSearch transactionId validatedRequest
  pure Ack

buildOnSearchReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  OnSearch.OnSearchReq ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReq req = do
  let domain = req.context.domain
  case domain of
    Context.MOBILITY -> TaxiACL.buildOnSearchReq req
    Context.PUBLIC_TRANSPORT -> BusACL.buildOnSearchReq req
    _ -> throwError (InvalidRequest $ "Unsupported Domain: " <> show domain)

onSearchLockKey :: Text -> Text
onSearchLockKey id = "Customer:OnSearch:MessageId-" <> id

onSearchProcessingLockKey :: Text -> Text
onSearchProcessingLockKey id = "Customer:OnSearch:Processing:MessageId-" <> id
