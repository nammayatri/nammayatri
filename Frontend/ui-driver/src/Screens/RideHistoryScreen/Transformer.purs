module Screens.RideHistoryScreen.Transformer where

import Prelude
import Common.Types.App (PaymentStatus(..), APIPaymentStatus(..))
import Components.PaymentHistoryListItem.Controller as PaymentHistoryListItem
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Services.API (DriverFeeStatus(..), PaymentBreakUp(..), PaymentDetailsEntity(..), TxnInfo(..))

getPaymentHistoryItemList :: Array PaymentDetailsEntity -> Array PaymentHistoryListItem.Config
getPaymentHistoryItemList arr = appendArray $ getFinalPaymentHistory $ map (\item -> getPaymentHistoryItem item) arr

getPaymentHistoryItem :: PaymentDetailsEntity -> OrderWithTxn
getPaymentHistoryItem (PaymentDetailsEntity item) =
  { order:
      { isSelected: false
      , charges: item.charges
      , totalEarning: item.totalEarnings
      , totalRides: item.totalRides
      , date: item.date
      , status:
          case item.status of
            EXEMPTED -> Success
            COLLECTED_CASH -> Success
            _ -> Pending
      , id: item.driverFeeId
      , paymentBreakUp:
          ( \(PaymentBreakUp charge) ->
              { description: charge.component
              , amount: charge.amount
              }
          )
            <$> item.chargesBreakup
      }
  , txns: (map (\txn -> getTransactionItem (PaymentDetailsEntity item) txn) item.txnInfo)
  }

type OrderWithTxn
  = { order :: PaymentHistoryListItem.Config
    , txns :: Array PaymentHistoryListItem.Config
    }

appendArray :: Array (Array PaymentHistoryListItem.Config) -> Array PaymentHistoryListItem.Config
appendArray item = case (DA.head item) of
  Nothing -> []
  Just currItem -> currItem <> (appendArray $ fromMaybe [] (DA.tail item))

getFinalPaymentHistory :: Array OrderWithTxn -> Array (Array PaymentHistoryListItem.Config)
getFinalPaymentHistory list = map (\item -> if DA.null item.txns then [ item.order ] else item.txns) list

getTransactionItem :: PaymentDetailsEntity -> TxnInfo -> PaymentHistoryListItem.Config
getTransactionItem (PaymentDetailsEntity item) (TxnInfo txn) =
  { isSelected: false
  , charges: item.charges
  , totalEarning: item.totalEarnings
  , totalRides: item.totalRides
  , date: item.date
  , status:
      case txn.status of
        NEW -> Pending
        PENDING_VBV -> Pending
        CHARGED -> Success
        AUTHENTICATION_FAILED -> Failed
        AUTHORIZATION_FAILED -> Failed
        JUSPAY_DECLINED -> Failed
        AUTHORIZING -> Pending
        COD_INITIATED -> Pending
        STARTED -> Pending
        AUTO_REFUNDED -> Pending
  , id: txn.id
  , paymentBreakUp:
      ( \(PaymentBreakUp charge) ->
          { description: charge.component
          , amount: charge.amount
          }
      )
        <$> item.chargesBreakup
  }
