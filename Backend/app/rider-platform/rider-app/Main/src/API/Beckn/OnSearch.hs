{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSearch (API, handler) where

import qualified Beckn.ACL.Metro.Search as MetroACL
import qualified Beckn.ACL.OnSearch as TaxiACL
import Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.Servant.JSONBS
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Tools.Error

type API =
  -- SignatureAuth "X-Gateway-Authorization"
  --   :>
  -- OnSearch.OnSearchAPI
  "on_search"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] OnSearchRes

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch ::
  -- SignatureAuthResult ->
  SignatureAuthResult ->
  ByteString ->
  FlowHandler AckResponse
onSearch _ rawReq = do
  -- let rawReq = T.encodeUtf8 rawReq'
  let decodedReq :: Either String MetroOnSearchReq = A.eitherDecodeStrict rawReq
  case decodedReq of
    Right contextV2 -> withFlowHandlerBecknAPI $ do
      logInfo $ "Decoded Metro OnSearch Callback, rawReq: " <> T.decodeUtf8 rawReq <> ", contextV2: " <> show contextV2
      throwError $ InvalidRequest "Not implemented, v2 migration going on"
    Left err -> withFlowHandlerBecknAPI $ do
      logError $ "Failed to decode Metro OnSearch Callback, rawReq: " <> T.decodeUtf8 rawReq <> ", err: " <> T.pack err
      req :: OnSearch.OnSearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
      withTransactionIdLogTag req $ do
        mbDOnSearchReq <- TaxiACL.buildOnSearchReq req
        whenJust mbDOnSearchReq $ \request -> do
          Redis.whenWithLockRedis (onSearchLockKey req.context.message_id) 60 $ do
            validatedRequest <- DOnSearch.validateRequest request
            fork "on search processing" $ do
              Redis.whenWithLockRedis (onSearchProcessingLockKey req.context.message_id) 60 $
                DOnSearch.onSearch req.context.message_id validatedRequest
        pure Ack

onSearchLockKey :: Text -> Text
onSearchLockKey id = "Customer:OnSearch:MessageId-" <> id

onSearchProcessingLockKey :: Text -> Text
onSearchProcessingLockKey id = "Customer:OnSearch:Processing:MessageId-" <> id

newtype MetroOnSearchReq = MetroOnSearchReq
  { context :: MetroACL.MetroContext
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
