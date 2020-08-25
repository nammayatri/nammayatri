{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Trigger
  ( TriggerFlow (..),
    trigger,
  )
where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.Core.FmdError
import Beckn.Types.FMD.API.Search
import Beckn.Utils.Common
import Beckn.Utils.Mock
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List (lookup)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant hiding (Context)

data TriggerFlow
  = SimpleConfirm
  | NoSearchResult
  | SearchErrorFMD FmdError
  deriving (Eq, Show, Generic, ToJSON)

instance FromHttpApiData TriggerFlow where
  -- FIXME: there must be some clever way to derive this mechanically
  parseQueryParam flow =
    maybeToRight noMatch $
      lookup flow flows
    where
      flows =
        mconcat
          [ [ ("simple-confirm", SimpleConfirm),
              ("no-search-result", NoSearchResult)
            ],
            M.toList $ SearchErrorFMD <$> allFmdErrorFlowIds
          ]
      noMatch = "Invalid flow. Specify one of: " <> T.intercalate ", " (fst <$> flows)

instance ToHttpApiData TriggerFlow where
  toQueryParam SimpleConfirm = "simple-confirm"
  toQueryParam NoSearchResult = "no-search-result"
  toQueryParam (SearchErrorFMD err) = fmdErrorFlowId err
  toHeader = BSL.toStrict . encode
  toUrlPiece = DT.decodeUtf8 . toHeader

trigger :: TriggerFlow -> FlowHandler AckResponse
trigger flow = withFlowHandler $ do
  baseUrl <- xGatewayUri <$> ask
  transactionId <-
    case flow of
      SimpleConfirm -> EL.generateGUID
      NoSearchResult -> pure noSearchResultId
      SearchErrorFMD err -> pure $ fmdErrorFlowId err
  req <- buildSearchReq transactionId
  eRes <-
    callClient "search" baseUrl $
      client searchAPI "test-app-2-key" req
  EL.logDebug @Text "mock_app_backend" $ "context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = req ^. #context,
        _message = ack "ACK",
        _error = Nothing
      }
