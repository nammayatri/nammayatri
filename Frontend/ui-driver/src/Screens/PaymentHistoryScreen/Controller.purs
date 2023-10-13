{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.Controller where

import Common.Types.App (PaymentStatus(..), CalendarModalDateObject(..))
import Components.DueDetailsList.Controller (Action(..)) as DueDetailsListController
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array (concatMap, length, nubBy, nubByEq, null, partition, union, (!!), filter)
import Data.Maybe as Mb
import Engineering.Helpers.Commons (convertUTCtoISC, flowRunner)
import JBridge (copyToClipboard, toast, generateInvoicePDF)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (void, discard, class Show, bind, compare, map, not, pure, show, unit, ($), (/=), (<>), (==), (-), (<), (&&), (||), (>), (/))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.PaymentHistoryScreen.Transformer (getAutoPayPaymentStatus, getInvoiceStatus)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem, InvoiceListItem)
import Services.API (AutoPayInvoiceHistory(..), FeeType(..), ManualInvoiceHistory(..))
import Services.API (FeeType(..), GetPaymentHistoryResp(..), PaymentDetailsEntity(..), HistoryEntityV2Resp(..)) as SA
import Debug (spy)
import Engineering.Helpers.Utils (getWeeksInMonth, getCurrentDay, decrementCalendarMonth, incrementCalendarMonth, selectRangeCalendarDate, initializeCalendar)
import Helpers.Utils (differenceBetweenTwoUTC)
import Components.Calendar as Calendar
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEventWithMultipleParams)
import Foreign.Object (empty)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Transformers.Back.Trans (runBackT)
import Control.Monad.Except (lift, runExcept, runExceptT)
import Services.Backend as Remote
import Data.Either (Either(..))
import Screens.PaymentHistoryScreen.Transformer (getInvoiceDetailsList)
import Services.API (GetInvoiceResp(..))
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)

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
            | LoadMore
            | CalendarAction Calendar.Action
            | ShowCalendarPopup
            | DownloadInvoice (Array InvoiceListItem)
            | NoInvoiceAvailable
            | InvoiceDownloaded


data ScreenOutput =  GoBack
                    | SetupAutoPay PaymentHistoryScreenState
                    | ShowSummary PaymentHistoryScreenState String
                    | SwitchTab PaymentHistoryScreenState
                    | LoadMoreItems PaymentHistoryScreenState
                    | DownloadingInvoice PaymentHistoryScreenState


eval :: Action -> PaymentHistoryScreenState -> Eval Action ScreenOutput PaymentHistoryScreenState

eval BackPressed state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails, selectedDue = ""}}
                         else exit $ GoBack

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick )) state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails, selectedDue = ""}}
                         else exit $ GoBack

-- eval ItemClick state = exit $ ViewPaymentDetails state

eval (ChangeTab switchToAutoPay) state = exit $ SwitchTab state { props{ autoPayHistory = switchToAutoPay}}

eval (ListItemClick item) state = if item.invoiceId /= "" then exit $ ShowSummary state item.invoiceId
  else continue state

eval ViewRideDetails state = continue state { props{ subView = RideDetails}}

eval (UpdatePaymentHistory response) state = getAllTransactions response state

eval (DueDetailsListAction (DueDetailsListController.SelectDue dueItem)) state = continue state {props {selectedDue = if state.props.selectedDue == dueItem.id then "" else dueItem.id}}

eval (Copy val) state = continueWithCmd state [ do 
    _ <- pure $ copyToClipboard val
    _ <- pure $ toast (getString COPIED)
    pure NoAction
  ]

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = updateAndExit state  $ SetupAutoPay state

eval LoadMore state = exit $ LoadMoreItems state

eval ShowCalendarPopup state = do
  let res = initializeCalendar true
  continue state { props{ weeks = res.weeks, invoicePopupVisible = true, selectedTimeSpan = res.selectedTimeSpan, startDate = res.startDate, endDate = res.endDate}}

eval (CalendarAction Calendar.HideCalendarPopup) state = continue state { props{ invoicePopupVisible = false, startDate = Mb.Nothing, endDate = Mb.Nothing, showError = false}}

eval (CalendarAction (Calendar.DecrementMonth res)) state = do
  continue state {props {weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan}}

eval (CalendarAction (Calendar.IncrementMonth res)) state = do
  continue state {props {weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan}}

eval (CalendarAction (Calendar.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  updateAndExit state{props{downloadInvoice = true, invoicePopupVisible = false}} $ DownloadingInvoice state{props{downloadInvoice = true,  invoicePopupVisible = false}}

eval (DownloadInvoice resp) state = do
  continueWithCmd state
    [ do
        let _ = unsafePerformEffect $ logEventWithMultipleParams empty "ny_driver_invoice_download" $ []
        _ <- pure $ generateInvoicePDF state {props {invoiceData = resp}} "NEW"
        pure InvoiceDownloaded
  ]

eval (CalendarAction (Calendar.PrimaryButtonCancelActionController (PrimaryButtonController.OnClick))) state = do
  continue state { props { invoicePopupVisible = false}}


eval (CalendarAction (Calendar.SelectDate res)) state = do
  let errorResponse = getErrorResponse res.startDate res.endDate
  continue state {props {startDate = res.startDate, endDate = res.endDate, weeks = res.weeks, showError = errorResponse.showError, errorMessage = errorResponse.errorMessage }}

eval InvoiceDownloaded state = continue state {props{downloadInvoice = false}}

eval NoInvoiceAvailable state = continue state {props{showError = true, errorMessage = getString NO_INVOICE_AVAILABLE}}

eval _ state = continue state

getAllTransactions :: SA.HistoryEntityV2Resp -> PaymentHistoryScreenState -> Eval Action ScreenOutput PaymentHistoryScreenState
getAllTransactions (SA.HistoryEntityV2Resp response) state = do
  let autoPayInvoices = response.autoPayInvoices
      manualPayInvoices = nubByEq (\(ManualInvoiceHistory a) (ManualInvoiceHistory b) -> a.invoiceId == b.invoiceId) response.manualPayInvoices
      responseLength = if state.props.autoPayHistory then length autoPayInvoices else length response.manualPayInvoices
  continue state {
    data {
      autoPayList = union state.data.autoPayList (map (\ invoice -> getAutoPayInvoice invoice) autoPayInvoices),
      manualPayList = union state.data.manualPayList (map (\ invoice -> getManualPayInvoice invoice) manualPayInvoices)
    },
    props {
      enableLoadMore = responseLength == 15
    }
  }
getAutoPayInvoice :: AutoPayInvoiceHistory -> PaymentListItem
getAutoPayInvoice (AutoPayInvoiceHistory autoPayInvoice) = {
  paymentStatus : getAutoPayPaymentStatus autoPayInvoice.autoPayStage,
  invoiceId : autoPayInvoice.invoiceId,
  amount : autoPayInvoice.amount,
  description : (getString RIDES_TAKEN_ON) <> " ",
  feeType : AUTOPAY_PAYMENT,
  transactionDate : autoPayInvoice.executionAt,
  ridesTakenDate : (convertUTCtoISC autoPayInvoice.rideTakenOn "Do MMM YYYY")
}

getManualPayInvoice :: ManualInvoiceHistory -> PaymentListItem
getManualPayInvoice (ManualInvoiceHistory manualPayInvoice) = do
  let manualInvoiceItemConfig = getManualDesc ""
  {
    invoiceId : manualPayInvoice.invoiceId,
    paymentStatus : getInvoiceStatus (Mb.Just manualPayInvoice.paymentStatus),
    amount : manualPayInvoice.amount,
    description : manualInvoiceItemConfig.description,
    feeType : manualPayInvoice.feeType,
    transactionDate : manualPayInvoice.createdAt,
    ridesTakenDate : manualInvoiceItemConfig.rideDays
  }
    where 
      getManualDesc :: String -> {description :: String, rideDays :: String}
      getManualDesc _ = do
        let rideDate = Mb.fromMaybe "" manualPayInvoice.rideTakenOn
            rideDays = if manualPayInvoice.rideDays == 1 && rideDate /= "" then convertUTCtoISC rideDate "Do MMM YYYY" else show manualPayInvoice.rideDays <> " " <> getString DAYS
        case manualPayInvoice.feeType, manualPayInvoice.rideDays - 1 of
          AUTOPAY_REGISTRATION, 0 -> {description : getString ONE_TIME_REGISTERATION, rideDays : ""}
          AUTOPAY_REGISTRATION, _ -> {description : getString CLEARANCE_AND_REGISTERATION, rideDays : ""}
          _, _ -> {description : (getString RIDES_TAKEN_ON) <> " ", rideDays : rideDays }

type CalendarErrorResponse = {
  showError :: Boolean,
  errorMessage :: String
 }

getErrorResponse :: Mb.Maybe CalendarModalDateObject -> Mb.Maybe CalendarModalDateObject -> CalendarErrorResponse
getErrorResponse mbStartDate mbEndDate = do
  case mbStartDate of
    Mb.Just startDate -> do
      case mbEndDate of
        Mb.Just endDate -> do
          let toShowError = if (differenceBetweenTwoUTC endDate.utcDate startDate.utcDate)/86400 > 7 then true else false
          {
            showError : toShowError,
            errorMessage : if toShowError then getString YOU_CAN_DOWNLOAD_INVOICE_FOR_UPTO_7_DAYS else ""
          }
        Mb.Nothing -> {showError : false, errorMessage : ""}
    Mb.Nothing -> {showError : false, errorMessage : ""}