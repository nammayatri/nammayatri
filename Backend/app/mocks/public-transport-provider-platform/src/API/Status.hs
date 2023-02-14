 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TypeApplications #-}

module API.Status where

import API.Utils
import "public-transport-rider-platform" Beckn.Spec.OnStatus
import "public-transport-rider-platform" Beckn.Spec.Status
import qualified Control.Monad.Catch as C
import Environment
import ExternalAPI
import Kernel.Mock.App
import Kernel.Mock.Utils
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Time
import qualified Redis
import Relude

statusServer :: BecknReq StatusMessage -> MockM AppEnv AckResponse
statusServer statusReq@(BecknReq ctx msg) = do
  logOutput INFO $ "got confirm request: " <> show statusReq
  context' <- buildOnActionContext ON_STATUS ctx
  let orderId = msg.order.id
  logOutput INFO $ "reading order with orderId=" <> orderId
  eithCtxOrd <- C.try @(MockM AppEnv) @SomeException (Redis.readOrder orderId)

  _ <- fork "call on_status" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    let eithOnStatusMsg = bimap (textToError . show) (OnStatusMessage . snd) eithCtxOrd
        onStatusReq = BecknCallbackReq context' eithOnStatusMsg
    callBapOnStatus onStatusReq
  pure Ack
