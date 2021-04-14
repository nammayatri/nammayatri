module Utils.SES (sendEmail) where

import qualified Beckn.SesConfig as SesConfig
import Control.Lens ((?~))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text
import EulerHS.Prelude
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Text as AWS
import qualified Network.AWS.SES.SendEmail as AWS
import qualified Network.AWS.SES.Types as AWS

sendEmail :: (Monad m, MonadIO m, MonadCatch m, MonadUnliftIO m) => SesConfig.EmailRequestConfig -> Text -> Text -> m (Maybe Text)
sendEmail sesConfig@SesConfig.EmailRequestConfig {..} subject body = do
  let sesQuery = mkSendEmailQuery sesConfig subject body
  env <- mkSesEnv region
  result <- AWS.runResourceT $ AWS.runAWS env (AWS.send sesQuery)
  let responseStatusCode = result ^. AWS.sersResponseStatus
  let responseError =
        case responseStatusCode of
          200 -> Nothing
          errorStatusCode ->
            Just $
              "Issue email sending error: status " <> show errorStatusCode <> " - "
                <> result ^. AWS.sersMessageId
  return responseError

mkSendEmailQuery :: SesConfig.EmailRequestConfig -> Text -> Text -> AWS.SendEmail
mkSendEmailQuery SesConfig.EmailRequestConfig {..} subject body =
  let dest = AWS.dCCAddresses .~ cc $ AWS.dToAddresses .~ to $ AWS.destination
      msg = AWS.message (AWS.content subject) (AWS.bText ?~ AWS.content body $ AWS.body)
   in AWS.seReplyToAddresses .~ replyTo $ AWS.sendEmail from dest msg

mkSesEnv :: (MonadIO m, MonadCatch m) => Text -> m AWS.Env
mkSesEnv region = do
  env <- AWS.newEnv AWS.Discover
  case AWS.fromText region of
    Right r -> pure $ AWS.envRegion .~ r $ env
    Left _err -> pure env
