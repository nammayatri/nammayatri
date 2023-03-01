{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
  runTransaction $ Query.addFailureReason @Flow id_ err
  throwError err
