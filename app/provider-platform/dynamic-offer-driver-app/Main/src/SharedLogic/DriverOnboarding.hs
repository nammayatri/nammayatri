module SharedLogic.DriverOnboarding where

import qualified Data.Text as T
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.Image as Domain
import Environment
import qualified Kernel.External.Slack.Flow as SF
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverOnboarding.Image as Query

notifyErrorToSupport ::
  Maybe T.Text ->
  T.Text ->
  [Maybe DriverOnboardingError] ->
  Flow ()
notifyErrorToSupport driverPhone orgName errs = do
  let reasons = catMaybes $ catMaybes $ toMsg <$> errs
  onboardSupportSmsTemplate <- asks (.driverOnboardingConfigs.onboardSupportSmsTemplate)
  let message =
        T.replace "{#reasons#}" (show reasons) $
          T.replace "{#driver-phone#}" (fromMaybe "" driverPhone) $
            T.replace "{#org#}" orgName onboardSupportSmsTemplate
  _ <- SF.postMessage message
  return ()
  where
    toMsg e = toMessage <$> e

throwImageError :: Id Domain.Image -> DriverOnboardingError -> Flow b
throwImageError id_ err = do
  runTransaction $ Query.addFailureReason id_ err
  throwError err
