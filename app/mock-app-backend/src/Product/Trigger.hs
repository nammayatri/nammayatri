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
import Beckn.Types.FMD.API.Search
import Beckn.Utils.Common
import Beckn.Utils.Mock
import Data.List (lookup)
import qualified Data.Text as T
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant hiding (Context)
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified System.Environment as SE

data TriggerFlow
  = SimpleConfirm
  | NoSearchResult
  | SearchErrorFMD001
  deriving (Eq, Show, Generic)

instance FromHttpApiData TriggerFlow where
  -- FIXME: there must be some clever way to derive this mechanically
  parseQueryParam flow =
    maybeToRight noMatch $
      lookup flow flows
    where
      flows =
        [ ("simple-confirm", SimpleConfirm),
          ("no-search-result", NoSearchResult),
          ("search-error-fmd-001", SearchErrorFMD001)
        ]
      noMatch = "Invalid flow. Specify one of: " <> T.intercalate ", " (fst <$> flows)

gatewayLookup :: FlowR r (String, Int)
gatewayLookup =
  EL.runIO $
    (,)
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "GATEWAY_HOST")
      <*> (fromMaybe 8015 . (>>= readMaybe) <$> SE.lookupEnv "GATEWAY_PORT")

gatewayBaseUrl :: FlowR r BaseUrl
gatewayBaseUrl = do
  (host, port) <- gatewayLookup
  return
    BaseUrl
      { baseUrlScheme = Http,
        baseUrlHost = host,
        baseUrlPort = port,
        baseUrlPath = "v1"
      }

trigger :: TriggerFlow -> FlowHandler AckResponse
trigger flow = withFlowHandler $ do
  baseUrl <- gatewayBaseUrl
  transactionId <-
    case flow of
      SimpleConfirm -> EL.generateGUID
      NoSearchResult -> pure noSearchResultId
      SearchErrorFMD001 -> pure searchPickupLocationNotServiceableId
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
