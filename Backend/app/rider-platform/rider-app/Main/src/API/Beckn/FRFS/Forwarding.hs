{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.Forwarding
  ( maybeForwardOnSearch,
    maybeForwardOnInit,
    maybeForwardOnConfirm,
    maybeForwardOnUpdate,
    maybeForwardOnStatus,
    maybeForwardOnCancel,
  )
where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude (ByteString)
import qualified EulerHS.Types as ET
import Kernel.External.BapHostRedirect (shouldRedirectBapHost)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Kernel.Utils.SignatureAuth as HttpSig

maybeForwardOnSearch :: Maybe (Id DM.Merchant) -> SignatureAuthResult -> Spec.OnSearchReq -> ByteString -> Flow (Maybe Spec.AckResponse)
maybeForwardOnSearch mbMerchantId authResult req reqBS =
  withRedirect mbMerchantId (req.onSearchReqContext.contextBapUri) $ \url ->
    forwardOnSearch url authResult reqBS

maybeForwardOnInit :: Maybe (Id DM.Merchant) -> SignatureAuthResult -> Spec.OnInitReq -> ByteString -> Flow (Maybe Spec.AckResponse)
maybeForwardOnInit mbMerchantId authResult req reqBS =
  withRedirect mbMerchantId (req.onInitReqContext.contextBapUri) $ \url ->
    forwardOnInit url authResult reqBS

maybeForwardOnConfirm :: Maybe (Id DM.Merchant) -> SignatureAuthResult -> Spec.OnConfirmReq -> ByteString -> Flow (Maybe Spec.AckResponse)
maybeForwardOnConfirm mbMerchantId authResult req reqBS =
  withRedirect mbMerchantId (req.onConfirmReqContext.contextBapUri) $ \url ->
    forwardOnConfirm url authResult reqBS

maybeForwardOnUpdate :: Maybe (Id DM.Merchant) -> SignatureAuthResult -> Spec.OnUpdateReq -> Flow (Maybe Spec.AckResponse)
maybeForwardOnUpdate mbMerchantId authResult req = do
  let reqBS = BSL.toStrict (A.encode req)
  withRedirect mbMerchantId (req.onUpdateReqContext.contextBapUri) $ \url ->
    forwardOnUpdate url authResult reqBS

maybeForwardOnStatus :: Maybe (Id DM.Merchant) -> SignatureAuthResult -> Spec.OnStatusReq -> ByteString -> Flow (Maybe Spec.AckResponse)
maybeForwardOnStatus mbMerchantId authResult req reqBS =
  withRedirect mbMerchantId (req.onStatusReqContext.contextBapUri) $ \url ->
    forwardOnStatus url authResult reqBS

maybeForwardOnCancel :: Maybe (Id DM.Merchant) -> SignatureAuthResult -> Spec.OnCancelReq -> Flow (Maybe Spec.AckResponse)
maybeForwardOnCancel mbMerchantId authResult req = do
  let reqBS = BSL.toStrict (A.encode req)
  withRedirect mbMerchantId (req.onCancelReqContext.contextBapUri) $ \url ->
    forwardOnCancel url authResult reqBS

withRedirect ::
  Maybe (Id DM.Merchant) ->
  Maybe Text ->
  (BaseUrl -> Flow Spec.AckResponse) ->
  Flow (Maybe Spec.AckResponse)
withRedirect mbMerchantId mbBapUriText act = do
  redirectMap <- asks (.bapHostRedirectMap)
  case mbBapUriText >>= parseBaseUrl of
    Nothing -> pure Nothing
    Just bapUri ->
      case shouldRedirectBapHost redirectMap bapUri of
        Just (Just url) -> Just <$> act (redirected mbMerchantId url)
        _ -> pure Nothing

frfsBasePath :: String
frfsBasePath = "/beckn/frfs/v1"

redirected :: Maybe (Id DM.Merchant) -> BaseUrl -> BaseUrl
redirected mbMerchantId url =
  let merchantSeg = maybe "" (\m -> "/" <> T.unpack m.getId) mbMerchantId
   in url {baseUrlPath = baseUrlPath url <> frfsBasePath <> merchantSeg}

signed :: SignatureAuthResult -> ET.EulerClient a -> ET.EulerClient a
signed authResult =
  withHeaders [("Authorization", decodeUtf8 $ HttpSig.encode authResult.signature)]

logForward :: Text -> BaseUrl -> Flow ()
logForward tag url = logInfo $ "Forwarding FRFS " <> tag <> " to " <> showBaseUrl url

forwardOnSearch :: BaseUrl -> SignatureAuthResult -> ByteString -> Flow Spec.AckResponse
forwardOnSearch baseUrl authResult reqBS = do
  let url = baseUrl
  logForward "on_search" url
  let client = signed authResult $ ET.client Spec.onSearchAPIBS reqBS
  callAPI url client "on_search" Spec.onSearchAPIBS
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_FRFS_CALLBACK") url)

forwardOnInit :: BaseUrl -> SignatureAuthResult -> ByteString -> Flow Spec.AckResponse
forwardOnInit baseUrl authResult reqBS = do
  let url = baseUrl
  logForward "on_init" url
  let client = signed authResult $ ET.client Spec.onInitAPIBS reqBS
  callAPI url client "on_init" Spec.onInitAPIBS
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_FRFS_CALLBACK") url)

forwardOnConfirm :: BaseUrl -> SignatureAuthResult -> ByteString -> Flow Spec.AckResponse
forwardOnConfirm baseUrl authResult reqBS = do
  let url = baseUrl
  logForward "on_confirm" url
  let client = signed authResult $ ET.client Spec.onConfirmAPIBS reqBS
  callAPI url client "on_confirm" Spec.onConfirmAPIBS
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_FRFS_CALLBACK") url)

forwardOnUpdate :: BaseUrl -> SignatureAuthResult -> ByteString -> Flow Spec.AckResponse
forwardOnUpdate baseUrl authResult reqBS = do
  let url = baseUrl
  logForward "on_update" url
  let client = signed authResult $ ET.client Spec.onUpdateAPIBS reqBS
  callAPI url client "on_update" Spec.onUpdateAPIBS
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_FRFS_CALLBACK") url)

forwardOnStatus :: BaseUrl -> SignatureAuthResult -> ByteString -> Flow Spec.AckResponse
forwardOnStatus baseUrl authResult reqBS = do
  let url = baseUrl
  logForward "on_status" url
  let client = signed authResult $ ET.client Spec.onStatusAPIBS reqBS
  callAPI url client "on_status" Spec.onStatusAPIBS
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_FRFS_CALLBACK") url)

forwardOnCancel :: BaseUrl -> SignatureAuthResult -> ByteString -> Flow Spec.AckResponse
forwardOnCancel baseUrl authResult reqBS = do
  let url = baseUrl
  logForward "on_cancel" url
  let client = signed authResult $ ET.client Spec.onCancelAPIBS reqBS
  callAPI url client "on_cancel" Spec.onCancelAPIBS
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_FRFS_CALLBACK") url)
