{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Select (buildSelectReqV1, buildSelectReqV2) where

import Beckn.ACL.Common (getTag, getTagV2)
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified Beckn.Types.Core.Taxi.Common.Tags as Select
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Select as DSelect
import Kernel.Prelude hiding (error, setField)
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error
import Tools.Metrics (CoreMetrics)

buildSelectReqV1 ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    CoreMetrics m
  ) =>
  Subscriber.Subscriber ->
  Select.SelectReq ->
  m DSelect.DSelectReq
buildSelectReqV1 subscriber req = do
  let context = req.context
  validateContext Context.SELECT context
  now <- getCurrentTime
  let order = req.message.order
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let messageId = context.message_id
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  item <- case order.items of
    [item] -> pure item
    _ -> throwError $ InvalidRequest "There should be only one item"
  let customerExtraFee = getCustomerExtraFee =<< item.tags

  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupTime = now,
        autoAssignEnabled = isJust context.max_callbacks && (fromMaybe 0 context.max_callbacks == 1),
        customerExtraFee = customerExtraFee,
        estimateId = Id order.fulfillment.id
      }

buildSelectReqV2 ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    CoreMetrics m
  ) =>
  Subscriber.Subscriber ->
  Select.SelectReqV2 ->
  m DSelect.DSelectReq
buildSelectReqV2 subscriber req = do
  let context = req.context
  validateContext Context.SELECT context
  now <- getCurrentTime
  let order = req.message.order
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let messageId = context.message_id
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  item <- case order.items of
    [item] -> pure item
    _ -> throwError $ InvalidRequest "There should be only one item"
  let customerExtraFee = getCustomerExtraFeeV2 =<< item.tags
  fulfillment <- case order.fulfillments of
    [fulfillment] -> pure fulfillment
    _ -> throwError $ InvalidRequest "There should be only one fulfillment in /select Order"
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupTime = now,
        autoAssignEnabled = isJust context.max_callbacks && (fromMaybe 0 context.max_callbacks == 1),
        customerExtraFee = customerExtraFee,
        estimateId = Id fulfillment.id
      }

getCustomerExtraFee :: Select.TagGroups -> Maybe Money
getCustomerExtraFee tagGroups = do
  tagValue <- getTag "customer_tip_info" "customer_tip" tagGroups
  customerExtraFee <- readMaybe $ T.unpack tagValue
  Just $ Money customerExtraFee

getCustomerExtraFeeV2 :: [Select.TagGroupV2] -> Maybe Money
getCustomerExtraFeeV2 tagGroups = do
  tagValue <- getTagV2 "customer_tip_info" "customer_tip" tagGroups
  customerExtraFee <- readMaybe $ T.unpack tagValue
  Just $ Money customerExtraFee
