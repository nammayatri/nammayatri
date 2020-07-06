{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import Beckn.Types.API.Search (OnSearchReq, OnSearchRes, SearchReq, SearchRes, searchAPI)
import Beckn.Types.App (FlowHandler)
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.Core.Ack
import Beckn.Utils.Common (withFlowHandler)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.ProviderRegistry as Provider
import Servant.Client (BaseUrl (..), Scheme (..))

-- TODO: auth

search :: SearchReq -> FlowHandler SearchRes
search req = withFlowHandler $ do
  (url, port) <- Provider.lookup $ req ^. #context
  let search' = ET.client searchAPI
      baseUrl =
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = url,
            baseUrlPort = port,
            baseUrlPath = "/v1"
          }
  eRes <- EL.callAPI baseUrl $ search' req
  let ack = either (Ack "Error" . show) (^. #_message) eRes
      resp = AckResponse (req ^. #context) ack
  EL.logDebug @Text "gateway" $ "req: " <> show req <> ", resp: " <> show resp
  return resp

searchCb :: OnSearchReq -> FlowHandler OnSearchRes
searchCb = error "Not implemented"
