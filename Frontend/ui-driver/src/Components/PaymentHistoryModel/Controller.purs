module Components.PaymentHistoryModel.Controller where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PaymentHistoryListItem as PaymentHistoryListItem

data Action
  = NoAction
  | GenericHeaderAC GenericHeader.Action
  | PaymentHistoryListItemAC PaymentHistoryListItem.Action
  | ErrorModalActionController ErrorModal.Action
