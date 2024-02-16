{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.FlowHandling where

import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Error.BaseError.HTTPError hiding (Error)
import Kernel.Types.Flow
import Kernel.Utils.Error.FlowHandling hiding (apiHandler, withFlowHandlerAPI)
import Kernel.Utils.Logging
import Types.Common

toError :: Maybe Text -> Error
toError mbMessage =
  Error
    { message = fromMaybe "unknown_error" mbMessage
    }

withFlowHandlerAPI ::
  ( Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    Log (FlowR r)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withFlowHandlerAPI = withFlowHandler' . apiHandler . handleIfUp

apiHandler ::
  ( MonadCatch m,
    Log m
  ) =>
  m a ->
  m a
apiHandler = (`catch` someExceptionToAPIErrorThrow)

someExceptionToAPIErrorThrow ::
  ( MonadCatch m,
    Log m
  ) =>
  SomeException ->
  m a
someExceptionToAPIErrorThrow exc
  | Just err <- fromException @Error exc = throwHTTPError err
  | Just (BaseException err) <- fromException exc = throwHTTPError (toError . toMessage $ err)
  | otherwise = throwHTTPError (toError . Just . show $ exc)

throwHTTPError ::
  ( Log m,
    MonadThrow m
  ) =>
  Error ->
  m b
throwHTTPError err = do
  let someExc = toException err
  logError $ makeLogSomeException someExc
  throwServantError (toHttpCode err) (toCustomHeaders err) err
