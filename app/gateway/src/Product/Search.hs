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
import Product.Validation
import qualified Types.API.Gateway.Search as ExternalAPI
import Types.API.Search (OnSearchReq, SearchReq (..))
import Types.Error
import qualified Types.Storage.Organization as Org
import Utils.Common

search ::
  SignatureAuthResult Org.Organization ->
  ByteString ->
  FlowHandler AckResponse
search (SignatureAuthResult proxySign _) rawReq = withFlowHandlerBecknAPI do
  req :: SearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  withTransactionIdLogTag req $ do
    validateContext "search" (req.context)
    unless (isJust (req.context.bap_uri)) $
      throwError $ InvalidRequest "No bap URI in context."
    let gatewaySearchSignAuth = ET.client ExternalAPI.searchAPI
        context = req.context
    providers <- BP.lookup context
    when (null providers) $ throwError NoProviders
    forM_ providers $ \provider -> fork "Provider search" . withLogTag "search_req" $ do
      providerUrl <- provider.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url") -- Already checked for existance
      withLogTag ("providerUrl_" <> showBaseUrlText providerUrl) . withRetry $
        -- TODO maybe we should explicitly call sign request here instead of using callAPIWithTrail'?
        callBecknAPI'
          (Just signatureAuthManagerKey)
          Nothing
          providerUrl
          (gatewaySearchSignAuth (Just proxySign) rawReq)
          "search"
    return Ack

searchCb ::
  SignatureAuthResult Org.Organization ->
  ByteString ->
  FlowHandler AckResponse
searchCb (SignatureAuthResult proxySign org) rawReq = withFlowHandlerBecknAPI do
  req :: OnSearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  withTransactionIdLogTag req . withLogTag "search_cb" $ do
    providerUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    withLogTag ("providerUrl_" <> showBaseUrlText providerUrl) do
      validateContext "on_search" req.context
      let gatewayOnSearchSignAuth = ET.client ExternalAPI.onSearchAPI
      bap_uri <- req.context.bap_uri & fromMaybeM (InvalidRequest "Bap_uri not present")
      withRetry $
        callBecknAPI'
          (Just signatureAuthManagerKey)
          Nothing
          bap_uri
          (gatewayOnSearchSignAuth (Just proxySign) rawReq)
          "on_search"
      return Ack
