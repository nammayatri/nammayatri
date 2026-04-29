module Domain.Types.Beckn.FRFS.OnCancel where

import qualified BecknV2.FRFS.Enums as Spec
import Kernel.Prelude
import Kernel.Utils.Common

data DOnCancel = DOnCancel
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    bppOrderId :: Text,
    bppItemId :: Text,
    transactionId :: Text,
    messageId :: Text,
    orderStatus :: Spec.OrderStatus,
    refundAmount :: Maybe HighPrecMoney,
    baseFare :: HighPrecMoney,
    cancellationCharges :: Maybe HighPrecMoney
  }
