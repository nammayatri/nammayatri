module Components.PaymentHistoryModel.Controller where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PaymentHistoryListItem as PaymentHistoryListItem
import Prelude (class Show, show, (<>))

instance showAction :: Show Action where
  show (NoAction) = "NoAction"
  show (GenericHeaderAC var1) = "GenericHeaderAC_" <> show var1
  show (PaymentHistoryListItemAC var1) = "PaymentHistoryListItemAC_" <> show var1
  show (ErrorModalActionController var1) = "ErrorModalActionController_" <> show var1

data Action
  = NoAction
  | GenericHeaderAC GenericHeader.Action
  | PaymentHistoryListItemAC PaymentHistoryListItem.Action
  | ErrorModalActionController ErrorModal.Action
