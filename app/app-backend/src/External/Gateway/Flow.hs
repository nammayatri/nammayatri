module External.Gateway.Flow where

import qualified Beckn.Types.API.Confirm as Confirm
import Beckn.Types.API.Search
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import System.Environment

search ::
  BaseUrl -> SearchReq -> L.Flow (Either Text ())
search url req = do
  res <- L.callAPI url $ API.search req
  whenRight res $ \_ ->
    L.logInfo "Search" "Search successfully delivered"
  whenLeft res $ \err ->
    L.logError "Search" ("error occurred while search: " <> (show err))
  return $ first show res

confirm :: BaseUrl -> Confirm.ConfirmReq -> L.Flow (Either Text ())
confirm url req = do
  res <- L.callAPI url $ API.confirm req
  whenLeft res $ \err ->
    L.logError "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError "Search" ("error occurred while search: " <> (show err))
  return $ first show res

getBaseUrl :: L.Flow BaseUrl
getBaseUrl = do
  envUrl <- L.runIO loadGatewayUrl
  case envUrl of
    Nothing -> do
      L.logInfo "Gateway Url" "Using defaults"
      return $
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8014,
            baseUrlPath = "/v1"
          }
    Just url -> return url

loadGatewayUrl :: IO (Maybe BaseUrl)
loadGatewayUrl = do
  mhost <- lookupEnv "GATEWAY_HOST"
  mport <- lookupEnv "GATEWAY_PORT"
  mpath <- lookupEnv "GATEWAY_PATH"
  pure $ do
    host <- mhost
    port <- mport
    path <- mpath
    p <- readMaybe port
    Just $
      BaseUrl
        { baseUrlScheme = Http, -- TODO: make Https when required
          baseUrlHost = host,
          baseUrlPort = p,
          baseUrlPath = path
        }
