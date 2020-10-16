module Utils.SES (sendEmail) where

import qualified Beckn.SesConfig as SesConfig
import Control.Lens ((?~))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text
import EulerHS.Prelude
import qualified Network.AWS as AWS
import qualified Network.AWS.SES.SendEmail as AWS
import qualified Network.AWS.SES.Types as AWS

sendEmail :: (Monad m, MonadIO m, MonadCatch m, MonadUnliftIO m) => SesConfig.EmailRequestConfig -> Text -> Text -> m (Maybe Text)
sendEmail SesConfig.EmailRequestConfig {..} subject body = do
  let dest = AWS.dToAddresses .~ [to] $ AWS.destination
  let msg =
        AWS.message
          (AWS.content subject)
          (AWS.bText ?~ AWS.content body $ AWS.body)
  let sendEmailRequest = AWS.sendEmail from dest msg
  env <- AWS.newEnv AWS.Discover
  result <- AWS.runResourceT $ AWS.runAWS env (AWS.send sendEmailRequest)
  let responseStatusCode = result ^. AWS.sersResponseStatus
  let responseError =
        case responseStatusCode of
          200 -> Nothing
          errorStatusCode ->
            Just $
              "Issue email sending error: status " <> show errorStatusCode <> " - "
                <> result ^. AWS.sersMessageId
  return responseError
