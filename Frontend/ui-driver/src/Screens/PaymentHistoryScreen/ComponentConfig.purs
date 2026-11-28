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
import Domain.Payments as PP
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (fetchImage, FetchImageFrom(..))
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
      , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
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

getStatusConfig :: PP.PaymentStatus -> Boolean -> {color :: String, backgroundColor :: String, name :: String, description :: String}
getStatusConfig status isCoinCleared = case status, isCoinCleared of
                              _, true -> {color : Color.green900, backgroundColor : Color.greenOpacity16, name : getString SUCCESS, description : getString DEBITED_ON}
                              PP.Success, _ -> {color : Color.green900, backgroundColor : Color.greenOpacity16, name : getString SUCCESS, description : getString DEBITED_ON}
                              PP.Pending, _ -> {color : Color.yellow900, backgroundColor : Color.yellowOpacity16, name : getString PENDING_CAPS, description : getString ATTEMPTED_ON}
                              PP.Failed, _  -> {color : Color.red, backgroundColor : Color.redOpacity16, name : getString FAILURE, description : getString ATTEMPTED_ON}
                              PP.Scheduled, _  -> {color : Color.yellow900, backgroundColor : Color.yellowOpacity16, name : getString SCHEDULED, description : getString SCHEDULED_ON}

getTransactionConfig :: ST.TransactionInfo -> {image :: String, title :: String, statusTimeDesc :: String}
getTransactionConfig transactionInfo  = do
  let status = transactionInfo.paymentStatus
      feeType = transactionInfo.feeType
      isRegisteredWithDuesClear = transactionInfo.numOfDriverFee > 1
      title' = getString case status, feeType, isRegisteredWithDuesClear, transactionInfo.isCoinCleared of
                  _, _, _ ,true                                     ->  PAYMENT_SUCCESSFUL
                  PP.Success, API.AUTOPAY_REGISTRATION, true ,_ ->  AUTOPAY_SETUP_AND_PAYMENT_SUCCESSFUL
                  PP.Success, API.AUTOPAY_REGISTRATION, false,_ ->  AUTOPAY_SETUP_SUCCESSFUL
                  PP.Success, _, _, _                           ->  PAYMENT_SUCCESSFUL
                  PP.Pending, API.AUTOPAY_REGISTRATION, true ,_ ->  AUTOPAY_SETUP_AND_PAYMENT_PENDING
                  PP.Pending, API.AUTOPAY_REGISTRATION, false,_ ->  AUTOPAY_SETUP_PENDING
                  PP.Pending, _, _, _                           ->  PAYMENT_PENDING
                  PP.Failed, API.AUTOPAY_REGISTRATION, true  ,_ ->  AUTOPAY_SETUP_AND_PAYMENT_FAILED
                  PP.Failed, API.AUTOPAY_REGISTRATION, false ,_ ->  AUTOPAY_SETUP_FAILED
                  PP.Failed, _, _ , _                           ->  PAYMENT_FAILED
                  _, _, _, _                                        ->  PAYMENT_SCHEDULED
  case status, transactionInfo.isCoinCleared of
    _ , true -> {image : fetchImage FF_ASSET "ny_ic_green_tick", statusTimeDesc : getString TRANSACTION_DEBITED_ON, title : title'}
    PP.Success, false -> {image : fetchImage FF_ASSET "ny_ic_green_tick", statusTimeDesc : getString TRANSACTION_DEBITED_ON, title : title'}
    PP.Pending, false -> {image : fetchImage FF_ASSET "ny_ic_transaction_pending", statusTimeDesc : getString TRANSACTION_ATTEMPTED_ON, title : title'}
    PP.Failed, false  -> {image : fetchImage FF_ASSET "ny_ic_payment_failed", statusTimeDesc : getString TRANSACTION_ATTEMPTED_ON, title : title'}
    PP.Scheduled, false  -> {image : fetchImage FF_ASSET "ny_ic_pending", statusTimeDesc : getString SCHEDULED_AT, title : title'}