module Screens.PaymentHistoryScreen.ComponentConfig
  where

import Prelude

import Common.Styles.Colors as Color
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), visibility)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..))
import Screens.Types as ST


genericHeaderConfig :: PaymentHistoryScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , height = V 25
      , width = V 25
      , margin = Margin 16 16 16 16
      } 
    , padding = PaddingVertical 5 5
    , textConfig {
        text = case state.props.subView of
                  PaymentHistory -> (getString PAYMENT_HISTORY)
                  TransactionDetails -> (getString TRANSACTION_DETAILS)
                  RideDetails -> (getString RIDE_DETAILS)
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

primaryButtonViewConfig :: ST.PaymentHistoryScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString ENABLE_AUTOPAY_NOW) }
      , id = "PaymentHistorySetupAutoPay"
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = MarginTop 24
      , visibility = if state.props.autoPayHistory && not state.props.autoPaySetup then VISIBLE else GONE
      , enableLoader = JB.getBtnLoader "PaymentHistorySetupAutoPay"
      }
  in primaryButtonConfig'

getStatusConfig :: Common.PaymentStatus -> {color :: String, backgroundColor :: String, name :: String}
getStatusConfig status = case status of
                              Common.Success -> {color : Color.green900, backgroundColor : "#2953BB6F", name : getString SUCCESS}
                              Common.Pending -> {color : Color.yellow900, backgroundColor : "#29FCC32C", name : getString PENDING_CAPS}
                              Common.Failed  -> {color : Color.red, backgroundColor : "#29E55454", name : getString FAILURE}
                              Common.Scheduled  -> {color : Color.red, backgroundColor : "#29E55454", name : getString FAILURE}

getTransactionConfig :: Common.PaymentStatus -> {image :: String, title :: String}
getTransactionConfig status = case status of
                              Common.Success -> {image : "ny_ic_green_tick", title : getString PAYMENT_SUCCESSFUL}
                              Common.Pending -> {image : "ny_ic_transaction_pending", title : getString PAYMENT_PENDING}
                              Common.Failed  -> {image : "ny_ic_payment_failed", title : getString PAYMENT_FAILED}
                              Common.Scheduled  -> {image : "ny_ic_transaction_pending", title : "Payment Scheduled"}
                              -- {image : "ny_ic_pending", title : getString NOTIFICATION_SCHEDULED}