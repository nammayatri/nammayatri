module FmdWrapper.Search where

import Beckn.Types.FMD.API.Search
import Common
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude
import Fmd
import FmdWrapper.Common
import FmdWrapper.Server
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Utils

spec :: Spec
spec = do
  around withCallbackApp $ do
    appManager <- runIO $ Client.newManager tlsManagerSettings
    let appClientEnv = mkClientEnv appManager gatewayBaseUrl
    describe "Search API" $
      it "Search" $ \callbackData -> do
        value <- UUID.nextUUID
        case value of
          Nothing -> expectationFailure "Could not generate UUID."
          Just uuid -> do
            let transactionId = UUID.toText uuid
            ctx <- buildContext "search" transactionId (Just fmdTestAppBaseUrl) Nothing
            let searchReq = buildFMDSearchReq ctx

            _ <- runClient appClientEnv $ client searchAPI "fmd-test-app-key" searchReq
            _ <- waitForCallback (onSearchEndMVar callbackData)
            _ <- readTVarIO (onSearchTVar callbackData)

            pure ()
