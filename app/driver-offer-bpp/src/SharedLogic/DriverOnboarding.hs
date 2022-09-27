module SharedLogic.DriverOnboarding where

import qualified Beckn.External.Slack.Flow as SF
import Beckn.Prelude
import qualified Data.Text as T
import Environment

notifyErrorToSupport ::
  Maybe T.Text ->
  T.Text ->
  Flow ()
notifyErrorToSupport driverPhone orgName = do
  onboardSupportSmsTemplate <- asks (.onboardSupportSmsTemplate)
  let message = T.replace "{#driver-phone#}" (fromMaybe "" driverPhone) $ T.replace "{#org#}" orgName onboardSupportSmsTemplate
  _ <- SF.postMessage message
  return ()
