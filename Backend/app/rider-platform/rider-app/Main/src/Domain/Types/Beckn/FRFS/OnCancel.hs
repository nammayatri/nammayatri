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
    cancellationCharges :: Maybe HighPrecMoney,
    cancelledBy :: Maybe Text,
    cancellationTime :: Maybe UTCTime,
    operatorCancellation :: Maybe OperatorCancellation
  }

data OperatorCancellation = OperatorCancellation
  { reason :: Text,
    operationId :: Text
  }
  deriving (Eq, Show)

data CancelMode = RiderOrSystemCancel | ProviderCancel | OperatorCancel OperatorCancellation
  deriving (Eq, Show)

isCounterCancellation :: DOnCancel -> Bool
isCounterCancellation dOnCancel = dOnCancel.cancelledBy == Just "PROVIDER"

cancelModeOf :: DOnCancel -> CancelMode
cancelModeOf dOnCancel = case dOnCancel.operatorCancellation of
  Just operatorCancellation -> OperatorCancel operatorCancellation
  Nothing
    | isCounterCancellation dOnCancel -> ProviderCancel
    | otherwise -> RiderOrSystemCancel
