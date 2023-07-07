module Components.PaymentHistoryModel.Controller where

import Prelude

import Components.GenericHeader as GenericHeader
import Components.PaymentHistoryListItem as PaymentHistoryListItem

data Action = NoAction | GenericHeaderAC GenericHeader.Action | PaymentHistoryListItemAC PaymentHistoryListItem.Action