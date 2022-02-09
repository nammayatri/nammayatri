module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Core.Ack
import Beckn.Types.Error
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (callBecknAPI')
import Beckn.Utils.Servant.BaseUrl
import Beckn.Utils.Servant.Client
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..), signatureAuthManagerKey)
import qualified Data.Aeson as A
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.ProviderRegistry as BP
import qualified Types.API.Gateway.Search as ExternalAPI
import Types.API.Search (OnSearchReq, SearchReq (..))
import Types.Error
import Utils.Common

search ::
  SignatureAuthResult ->
  ByteString ->
  FlowHandler AckResponse
search (SignatureAuthResult proxySign _) rawReq = withFlowHandlerBecknAPI do
  req :: SearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  withTransactionIdLogTag req $ do
    let gatewaySearchSignAuth = ET.client ExternalAPI.searchAPI
        context = req.context
    providers <- BP.lookup context
    when (null providers) $ throwError NoProviders
    forM_ providers $ \provider -> fork "Provider search" . withLogTag "search_req" $ do
      let providerUrl = provider.subscriber_url
      withLogTag ("providerUrl_" <> showBaseUrlText providerUrl) . withRetry $
        -- TODO maybe we should explicitly call sign request here instead of using callAPIWithTrail'?
        void $
          callBecknAPI'
            (Just signatureAuthManagerKey)
            Nothing
            providerUrl
            (gatewaySearchSignAuth (Just proxySign) rawReq)
            "search"
    return Ack

searchCb ::
  SignatureAuthResult ->
  ByteString ->
  FlowHandler AckResponse
searchCb (SignatureAuthResult proxySign _subscriber) rawReq = withFlowHandlerBecknAPI do
  req :: OnSearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  withTransactionIdLogTag req . withLogTag "search_cb" $ do
    -- TODO: source providerUrl from _subscriber
    providerUrl <- req.context.bpp_uri & fromMaybeM (InvalidRequest "Missing context.bpp_uri")
    withLogTag ("providerUrl_" <> showBaseUrlText providerUrl) do
      let gatewayOnSearchSignAuth = ET.client ExternalAPI.onSearchAPI
      let bapUri = req.context.bap_uri
      void . withRetry $
        callBecknAPI'
          (Just signatureAuthManagerKey)
          Nothing
          bapUri
          (gatewayOnSearchSignAuth (Just proxySign) rawReq)
          "on_search"
      return Ack
