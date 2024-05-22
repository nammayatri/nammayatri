{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Init where

import qualified Beckn.OnDemand.Transformer.Init as TInit
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified BecknV2.OnDemand.Utils.Context as Utils
import qualified Domain.Action.Beckn.Init as DInit
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Field
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Error.Throwing

buildInitReqV2 ::
  ( MonadThrow m,
    (HasFlowEnv m r '["_version" ::: Text])
  ) =>
  Subscriber.Subscriber ->
  Init.InitReqV2 ->
  Bool ->
  m DInit.InitReq
buildInitReqV2 subscriber req isValueAddNP = do
  let context = req.initReqContext
  Utils.validateContext Context.INIT context
  bap_id <- context.contextBapId & fromMaybeM (InvalidRequest "Missing bap_id")
  unless (subscriber.subscriber_id == bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  bapUri <- mapM parseBaseUrl context.contextBapUri >>= fromMaybeM (InvalidRequest "bap_uri not found")
  unless (subscriber.subscriber_url == bapUri) $
    throwError (InvalidRequest "Invalid bap_uri")
  items <- req.initReqMessage.confirmReqMessageOrder.orderItems & fromMaybeM (InvalidRequest "items not found")
  case items of
    [_it] -> return ()
    _ -> throwError $ InvalidRequest "There must be exactly one item in init request"

  TInit.buildDInitReq subscriber req isValueAddNP
