{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.ComponentConfig
  where

import Prelude

import Common.Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (getAssetStoreLink)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), visibility)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..))
import Screens.Types as ST
import Services.API as API


genericHeaderConfig :: PaymentHistoryScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = "ny_ic_chevron_left," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
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

getStatusConfig :: Common.PaymentStatus -> {color :: String, backgroundColor :: String, name :: String, description :: String}
getStatusConfig status = case status of
                              Common.Success -> {color : Color.green900, backgroundColor : Color.greenOpacity16, name : getString SUCCESS, description : getString DEBITED_ON}
                              Common.Pending -> {color : Color.yellow900, backgroundColor : Color.yellowOpacity16, name : getString PENDING_CAPS, description : getString ATTEMPTED_ON}
                              Common.Failed  -> {color : Color.red, backgroundColor : Color.redOpacity16, name : getString FAILURE, description : getString ATTEMPTED_ON}
                              Common.Scheduled  -> {color : Color.yellow900, backgroundColor : Color.yellowOpacity16, name : getString SCHEDULED, description : getString SCHEDULED_ON}

getTransactionConfig :: ST.TransactionInfo -> {image :: String, title :: String, statusTimeDesc :: String}
getTransactionConfig transactionInfo = do
  let status = transactionInfo.paymentStatus
      feeType = transactionInfo.feeType
      isRegisteredWithDuesClear = transactionInfo.numOfDriverFee > 1
      title' = getString case status, feeType, isRegisteredWithDuesClear of
                  Common.Success, API.AUTOPAY_REGISTRATION, true  ->  AUTOPAY_SETUP_AND_PAYMENT_SUCCESSFUL
                  Common.Success, API.AUTOPAY_REGISTRATION, false ->  AUTOPAY_SETUP_SUCCESSFUL
                  Common.Success, _, _                            ->  PAYMENT_SUCCESSFUL
                  Common.Pending, API.AUTOPAY_REGISTRATION, true  ->  AUTOPAY_SETUP_AND_PAYMENT_PENDING
                  Common.Pending, API.AUTOPAY_REGISTRATION, false ->  AUTOPAY_SETUP_PENDING
                  Common.Pending, _, _                            ->  PAYMENT_PENDING
                  Common.Failed, API.AUTOPAY_REGISTRATION, true   ->  AUTOPAY_SETUP_AND_PAYMENT_FAILED
                  Common.Failed, API.AUTOPAY_REGISTRATION, false  ->  AUTOPAY_SETUP_FAILED
                  Common.Failed, _, _                             ->  PAYMENT_FAILED
                  _, _, _                                         ->  PAYMENT_SCHEDULED
  case status of
    Common.Success -> {image : "ny_ic_green_tick", statusTimeDesc : getString TRANSACTION_DEBITED_ON, title : title'}
    Common.Pending -> {image : "ny_ic_transaction_pending," <> (getAssetStoreLink FunctionCall) <> "ny_ic_transaction_pending.png", statusTimeDesc : getString TRANSACTION_ATTEMPTED_ON, title : title'}
    Common.Failed  -> {image : "ny_ic_payment_failed," <> (getAssetStoreLink FunctionCall) <> "ny_ic_payment_failed.png", statusTimeDesc : getString TRANSACTION_ATTEMPTED_ON, title : title'}
    Common.Scheduled  -> {image : "ny_ic_pending", statusTimeDesc : getString SCHEDULED_AT, title : title'}