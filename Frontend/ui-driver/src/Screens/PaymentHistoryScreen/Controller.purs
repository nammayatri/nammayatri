{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.Controller where

import Common.Types.App (PaymentStatus(..))
import Components.DueDetailsList.Controller (Action(..)) as DueDetailsListController
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array (concatMap, nubBy, nubByEq, null, partition, (!!))
import Data.Maybe (Maybe(..))
import Data.Maybe as Mb
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge (copyToClipboard, toast)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, compare, map, not, pure, show, unit, ($), (/=), (<>), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.PaymentHistoryScreen.Transformer (getAutoPayPaymentStatus, getInvoiceStatus)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem)
import Services.API (AutoPayInvoiceHistory(..), FeeType(..), ManualInvoiceHistory(..))
import Services.API (FeeType(..), GetPaymentHistoryResp(..), PaymentDetailsEntity(..), HistoryEntityV2Resp(..)) as SA

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "PaymentHistoryScreen"
    _ -> pure unit


data Action = BackPressed
            | AfterRender
            | NoAction
            | ItemClick
            | GenericHeaderAC GenericHeader.Action
            | ChangeTab Boolean
            | ViewRideDetails
            | ListItemClick PaymentListItem
            | UpdatePaymentHistory SA.HistoryEntityV2Resp
            | DueDetailsListAction DueDetailsListController.Action
            | Copy String
            | PrimaryButtonActionController PrimaryButtonController.Action


data ScreenOutput =  GoBack
                    | SetupAutoPay PaymentHistoryScreenState
                    | ShowSummary PaymentHistoryScreenState String
                    | SwitchTab PaymentHistoryScreenState


eval :: Action -> PaymentHistoryScreenState -> Eval Action ScreenOutput PaymentHistoryScreenState

eval BackPressed state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails}}
                         else exit $ GoBack

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick )) state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails}}
                         else exit $ GoBack

-- eval ItemClick state = exit $ ViewPaymentDetails state

eval (ChangeTab switchToAutoPay) state = exit $ SwitchTab state { props{ autoPayHistory = switchToAutoPay}}

eval (ListItemClick item) state = if item.invoiceId /= "" then exit $ ShowSummary state item.invoiceId
  else continue state

eval ViewRideDetails state = continue state { props{ subView = RideDetails}}

eval (UpdatePaymentHistory response) state = getAllTransactions response state

eval (DueDetailsListAction (DueDetailsListController.SelectDue dueItem)) state = continue state {props {selectedDue = dueItem.id}}

eval (Copy val) state = continueWithCmd state [ do 
    _ <- pure $ copyToClipboard val
    _ <- pure $ toast (getString COPIED)
    pure NoAction
  ]

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = updateAndExit state  $ SetupAutoPay state

eval _ state = continue state

getAllTransactions :: SA.HistoryEntityV2Resp -> PaymentHistoryScreenState -> Eval Action ScreenOutput PaymentHistoryScreenState
getAllTransactions (SA.HistoryEntityV2Resp response) state = do
  let autoPayInvoices = response.autoPayInvoices
      manualPayInvoices = nubByEq (\(ManualInvoiceHistory a) (ManualInvoiceHistory b) -> a.invoiceId == b.invoiceId) response.manualPayInvoices
  continue state {
    data {
      autoPayList = map (\ invoice -> getAutoPayInvoice invoice) autoPayInvoices,
      manualPayList = map (\ invoice -> getManualPayInvoice invoice) manualPayInvoices
    }
  }
getAutoPayInvoice :: AutoPayInvoiceHistory -> PaymentListItem
getAutoPayInvoice (AutoPayInvoiceHistory autoPayInvoice) = {
  paymentStatus : getAutoPayPaymentStatus autoPayInvoice.autoPayStage,
  invoiceId : autoPayInvoice.invoiceId,
  amount : autoPayInvoice.amount,
  description : (getString RIDES_TAKEN_ON) <> " " <> (convertUTCtoISC autoPayInvoice.rideTakenOn "Do MMM YYYY"),
  feeType : AUTOPAY_PAYMENT,
  transactionDate : autoPayInvoice.executionAt
}

getManualPayInvoice :: ManualInvoiceHistory -> PaymentListItem
getManualPayInvoice (ManualInvoiceHistory manualPayInvoice) = {
  invoiceId : manualPayInvoice.invoiceId,
  paymentStatus : getInvoiceStatus (Mb.Just manualPayInvoice.paymentStatus),
  amount : manualPayInvoice.amount,
  description : (getString RIDES_TAKEN_ON) <> " " <> show manualPayInvoice.rideDays <> " days",
  feeType : MANUAL_PAYMENT,
  transactionDate : manualPayInvoice.createdAt
}