module API.Confirm.Coerce where

import qualified Core.OnCancel as OnCancel
import qualified Core.OnConfirm as OnConfirm
import qualified Core.OnStatus as OnStatus
import Core.Payment
import Relude hiding (id, state)

coerceItemStatus :: OnConfirm.Item -> OnStatus.Item
coerceItemStatus OnConfirm.Item {..} = OnStatus.Item {..}

coerceParamsStatus :: OnConfirm.Params -> OnStatus.Params
coerceParamsStatus OnConfirm.Params {..} = OnStatus.Params {..}

coerceOrderStatus :: OnConfirm.Order -> OnStatus.Order
coerceOrderStatus order@OnConfirm.Order {..} = do
  let items' = map coerceItemStatus order.items
      params' = coerceParamsStatus order.payment.params
      payment' = payment {params = params'}
  OnStatus.Order {items = items', payment = payment', ..}

coerceItemCancel :: OnConfirm.Item -> OnCancel.Item
coerceItemCancel OnConfirm.Item {..} = OnCancel.Item {..}

coerceParamsCancel :: OnConfirm.Params -> OnCancel.Params
coerceParamsCancel OnConfirm.Params {..} = OnCancel.Params {..}

coerceOrderCancel :: OnConfirm.Order -> OnCancel.Order
coerceOrderCancel order@OnConfirm.Order {..} = do
  let items' = map coerceItemCancel order.items
      params' = coerceParamsCancel order.payment.params
      payment' = payment {params = params'}
  OnCancel.Order {items = items', payment = payment', ..}
