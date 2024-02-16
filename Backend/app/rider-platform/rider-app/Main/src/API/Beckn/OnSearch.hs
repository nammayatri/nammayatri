{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSearch (API, handler) where

import qualified Beckn.ACL.OnSearch as TaxiACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Data.Text as T
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

type API =
  SignatureAuth 'Domain.MOBILITY "X-Gateway-Authorization"
    :> OnSearch.OnSearchAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReqV2 ->
  FlowHandler AckResponse
onSearch _ _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onSearchReqContext
  mbDOnSearchReq <- Utils.withTransactionIdLogTag transactionId $ TaxiACL.buildOnSearchReqV2 reqV2
  messageId <- Utils.getMessageIdText reqV2.onSearchReqContext
  txnId <- Utils.getTransactionId reqV2.onSearchReqContext

  Utils.withTransactionIdLogTag txnId $ do
    logInfo $ "OnSearch received:-" <> show reqV2

    whenJust mbDOnSearchReq $ \request -> do
      Redis.whenWithLockRedis (onSearchLockKey messageId) 60 $ do
        validatedRequest <- DOnSearch.validateRequest request
        fork "on search processing" $ do
          Redis.whenWithLockRedis (onSearchProcessingLockKey messageId) 60 $
            DOnSearch.onSearch messageId validatedRequest
    pure Ack

onSearchLockKey :: Text -> Text
onSearchLockKey id = "Customer:OnSearch:MessageId-" <> id

onSearchProcessingLockKey :: Text -> Text
onSearchProcessingLockKey id = "Customer:OnSearch:Processing:MessageId-" <> id
