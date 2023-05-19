{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Select (buildSelectReq) where

import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified Domain.Action.Beckn.Select as DSelect
import Kernel.Prelude hiding (error, setField)
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error
import Tools.Metrics (CoreMetrics)

buildSelectReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    CoreMetrics m
  ) =>
  Subscriber.Subscriber ->
  Select.SelectReq ->
  m DSelect.DSelectReq
buildSelectReq subscriber req = do
  let context = req.context
  validateContext Context.SELECT context
  let order = req.message.order
  let pickup = order.fulfillment.start
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let messageId = context.message_id
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  item <- case order.items of
    [item] -> pure item
    _ -> throwError $ InvalidRequest "There should be only one item"
  let customerExtraFee = listToMaybe order.quote.breakup <&> roundToIntegral . (.price.value)
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupTime = pickup.time.timestamp,
        customerExtraFee = customerExtraFee,
        estimateId = Id item.id
      }
