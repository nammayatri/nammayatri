{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Flow.CompleteFlow where

import Beckn.Types.Core.Ack (AckResponse)
import qualified Beckn.Types.Core.ReqTypes as API
import Common (gatewayBaseUrl, signRequest)
import Control.Concurrent.MVar (isEmptyMVar)
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import Fmd (buildContext, buildFMDSearchReq, fmdWrapperBaseUrl)
import FmdWrapper.Common (assertAck, withNewUUID)
import qualified FmdWrapper.Fixtures as Fixtures
import FmdWrapper.Flow.Confirm (runConfirm)
import FmdWrapper.Flow.Search (processResults, runSearch, verifyDunzoCatalog)
import FmdWrapper.Server (CallbackData, waitForCallback, withCallbackApp)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Header, (:>))
import Servant.Client (Client, ClientEnv (..), ClientError, ClientM, HasClient, client, mkClientEnv)
import Test.Hspec hiding (example)
import qualified "fmd-wrapper" Types.Beckn.API.Cancel as CancelAPI
import qualified "fmd-wrapper" Types.Beckn.API.Confirm as ConfirmAPI
import qualified "fmd-wrapper" Types.Beckn.API.Search as SearchAPI
import qualified "fmd-wrapper" Types.Beckn.API.Status as StatusAPI
import qualified "fmd-wrapper" Types.Beckn.API.Track as TrackAPI
import "fmd-wrapper" Types.Beckn.Catalog (Catalog)
import "fmd-wrapper" Types.Beckn.Context (Action (..), Context (..))
import "fmd-wrapper" Types.Beckn.Order (OrderItem (..))
import Utils (runClient)

mkConfirmOrderFromSearchData :: SearchAPI.Intent -> Catalog -> ConfirmAPI.OrderObject
mkConfirmOrderFromSearchData intent catalog =
  Fixtures.confirmOrderObject
    & #order . #items .~ [OrderItem {id = clothesAndAccessoriesItemId}]
    & #order . #fulfillment . #start . #location . #gps .~ intent.fulfillment.start.location.gps
    & #order . #fulfillment . #end . #location . #gps .~ intent.fulfillment.end.location.gps
  where
    clothesAndAccessoriesItemId =
      let (provider : _xs) = catalog.bpp_providers
          (_item1 : item2 : _items) = provider.items
       in item2.id

runClientCall ::
  (ToJSON req, HasClient ClientM api, Client ClientM api ~ (Maybe Text -> req -> ClientM resp)) =>
  ClientEnv ->
  Text ->
  Proxy api ->
  req ->
  IO (Either ClientError resp)
runClientCall clientEnv orgId api req = do
  now <- getPOSIXTime
  let signature :: Text = decodeUtf8 $ signRequest req now orgId (orgId <> "-key")
  runClient clientEnv $ client api (Just signature) req

runStatus :: ClientEnv -> Text -> API.BecknReq StatusAPI.OrderId -> IO (Either ClientError AckResponse)
runStatus clientEnv orgId statusReq = do
  let statusAPI = Proxy :: Proxy (Header "Authorization" Text :> StatusAPI.StatusAPI)
  runClientCall clientEnv orgId statusAPI statusReq

runTrack :: ClientEnv -> Text -> API.BecknReq TrackAPI.TrackInfo -> IO (Either ClientError AckResponse)
runTrack clientEnv orgId trackReq = do
  let trackAPI = Proxy :: Proxy (Header "Authorization" Text :> TrackAPI.TrackAPI)
  runClientCall clientEnv orgId trackAPI trackReq

runCancel :: ClientEnv -> Text -> API.BecknReq CancelAPI.CancellationInfo -> IO (Either ClientError AckResponse)
runCancel clientEnv orgId cancelReq = do
  let cancelAPI = Proxy :: Proxy (Header "Authorization" Text :> CancelAPI.CancelAPI)
  runClientCall clientEnv orgId cancelAPI cancelReq

successfulFlow :: ClientEnv -> CallbackData -> IO ()
successfulFlow clientEnv callbackData = withNewUUID $ \transactionId -> do
  ctx <- buildContext SEARCH transactionId
  let searchReq = buildFMDSearchReq ctx Fixtures.validDunzoGps1 Fixtures.validDunzoGps2
  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  assertAck gatewayResponse

  waitForCallback
  searchResults <- processResults transactionId callbackData
  let dunzoResults = filter isDunzoResult searchResults

  let searchRes = rights (map API.contents dunzoResults)
  case searchRes of
    [] -> expectationFailure "Expected search result from Dunzo."
    (latestSearchCatalog : _xs) -> do
      verifyDunzoCatalog latestSearchCatalog

      let confirmCtx = ctx {action = CONFIRM}
      let confirmReq =
            API.BecknReq confirmCtx $
              mkConfirmOrderFromSearchData searchReq.message.intent latestSearchCatalog.catalog
      let fmdClientEnv = clientEnv {baseUrl = fmdWrapperBaseUrl}
      confirmResponse <- runConfirm fmdClientEnv "fmd-test-app" confirmReq
      assertAck confirmResponse

      waitForCallback
      mbConfirmCb <- tryGetCallback callbackData.onConfirmCb
      case mbConfirmCb of
        Nothing -> expectationFailure "Expected successful confirm callback from BPP."
        Just confirmCb -> do
          let orderId = confirmCb.order.id
          let trackReq = API.BecknReq ctx {action = TRACK} (TrackAPI.TrackInfo orderId Nothing)
          trackResponse <- runTrack fmdClientEnv "fmd-test-app" trackReq
          checkResponseAndCallback trackResponse "track" callbackData.onTrackCb

          let statusReq = API.BecknReq ctx {action = STATUS} (StatusAPI.OrderId orderId)
          statusResponse <- runStatus fmdClientEnv "fmd-test-app" statusReq
          checkResponseAndCallback statusResponse "status" callbackData.onStatusCb

          let cancelReq = API.BecknReq ctx {action = CANCEL} (CancelAPI.CancellationInfo orderId "Not specified." Nothing)
          cancelResponse <- runCancel fmdClientEnv "fmd-test-app" cancelReq
          checkResponseAndCallback cancelResponse "cancel" callbackData.onCancelCb
  where
    isDunzoResult result =
      result.context.bpp_uri == Just fmdWrapperBaseUrl
    tryGetCallback cbMVar = do
      callbackNotReceived <- isEmptyMVar cbMVar
      if callbackNotReceived
        then pure Nothing
        else readMVar cbMVar <&> rightToMaybe . (.result.contents)
    checkResponseAndCallback response api callbackMVar = do
      assertAck response
      waitForCallback
      mbCallback <- tryGetCallback callbackMVar
      whenNothing_ mbCallback $
        expectationFailure ("Expected successful " <> api <> " callback from BPP.")

spec :: Spec
spec = do
  around withCallbackApp $ do
    appManager <- runIO $ Client.newManager tlsManagerSettings
    let appClientEnv = mkClientEnv appManager gatewayBaseUrl
    it "Successful flow pass" $ successfulFlow appClientEnv
