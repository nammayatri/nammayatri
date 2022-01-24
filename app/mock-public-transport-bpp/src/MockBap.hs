{-# LANGUAGE TypeApplications #-}

module MockBap where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import DebugRequest
import qualified Network.Wai.Handler.Warp as NWW
import Servant

type OnSearchAPI = "on_search" :> Post '[JSON] AckResponse

type OnConfirmAPI = "on_confirm" :> Post '[JSON] AckResponse

type OnStatusAPI = "on_status" :> Post '[JSON] AckResponse

type OnCancelAPI = "on_cancel" :> Post '[JSON] AckResponse

type MockBapAPI = OnSearchAPI :<|> OnConfirmAPI :<|> OnStatusAPI :<|> OnCancelAPI

theBestOfAllServer :: (Monad m) => m AckResponse
theBestOfAllServer = pure Ack

totalServer :: Application
totalServer =
  serve
    (Proxy @MockBapAPI)
    (theBestOfAllServer :<|> theBestOfAllServer :<|> theBestOfAllServer :<|> theBestOfAllServer)

main :: IO ()
main = NWW.run 10666 $ logRequest totalServer
