module Screens.DriverEarningsScreen.Transformer where

import Prelude
import Common.Types.App (PaymentStatus(..), APIPaymentStatus(..))
import Components.PaymentHistoryListItem.Controller as PaymentHistoryListItem
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Services.API (DriverFeeStatus(..), PaymentBreakUp(..), PaymentDetailsEntity(..), TxnInfo(..))
