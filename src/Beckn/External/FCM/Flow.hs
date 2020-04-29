module Beckn.External.FCM.Flow where


import           Beckn.External.FCM.Types
import qualified Beckn.External.FCM.API as API
import qualified Data.Text as T
import           EulerHS.Prelude
import qualified EulerHS.Language as L
import           Servant.Client
import           System.Environment

sendNotification :: BaseUrl -> Text -> SubmitNotification -> L.Flow (Either Text ())
sendNotification url param req = do
  auth <- L.runIO $ T.pack <$> (getEnv "FCM_TOKEN")
  res <- L.callAPI url $ API.sendNotification (Just $ FAuth auth) param req
  whenRight res $ \_ ->
    L.logInfo "Notification" $ "Submitted notification successfully "
  return $ first show res

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "fcm.googleapis.com"
    , baseUrlPort = 443
    , baseUrlPath = ""
    }
