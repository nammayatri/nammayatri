{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.Controller where

import Components.DueDetailsList.Controller (Action(..)) as DueDetailsListController
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array (concatMap, null, (!!), partition)
import Data.Maybe as Mb
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge (copyToClipboard, toast)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, map, not, pure, show, unit, ($), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.PaymentHistoryScreen.ScreenData (dummyDetails, dummyPaymentListItem)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem)
import Services.API (FeeType(..), GetPaymentHistoryResp(..), PaymentDetailsEntity(..)) as SA

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
            | ListItemClick Int
            | UpdatePaymentHistory --GetPaymentHistoryResp
            | DueDetailsListAction DueDetailsListController.Action
            | Copy String
            | PrimaryButtonActionController PrimaryButtonController.Action


data ScreenOutput = 
-- ViewPaymentDetails PaymentHistoryScreenState 
--                     | 
                    GoBack
                    | SetupAutoPay PaymentHistoryScreenState


eval :: Action -> PaymentHistoryScreenState -> Eval Action ScreenOutput PaymentHistoryScreenState

eval BackPressed state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails}}
                         else exit $ GoBack

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick )) state = if state.props.subView == TransactionDetails then continue state { props{ subView = PaymentHistory}}
                         else if state.props.subView == RideDetails then continue state { props{ subView = TransactionDetails}}
                         else exit $ GoBack

-- eval ItemClick state = exit $ ViewPaymentDetails state

eval (ChangeTab switchToAutoPay) state = continue state { props{ autoPayHistory = switchToAutoPay}}

eval (ListItemClick index) state = do
  let transactionSplit = partition (\item -> item.feeType == SA.MANUAL_PAYMENT) state.data.transactions
      details = Mb.fromMaybe dummyDetails ((if state.props.autoPayHistory then transactionSplit.no else transactionSplit.yes) !! index)
      transactionDetails = {
        notificationStatus : details.paymentStatus,
        statusTime : details.transactionDate,
        details : [
        {key : "TXN_ID",
        title : getString TXN_ID,
        val : details.invoiceId},
        {
          key : "AMOUNT_PAID",
          title : getString AMOUNT_PAID,
          val : show details.totalCharges
        },
        { key : "PAYMENT_MODE",
        title : getString PAYMENT_MODE,
        val : if details.feeType == SA.MANUAL_PAYMENT then "UPI" else "UPI AutoPay"}
        ,
        {
          key : "TRIP_DATE",
          title : getString TRIP_DATE,
          val : (convertUTCtoISC details.ridesTakenDate "Do MMM, YYYY")
        },
        {
          key : "PLAN",
          title : getString PLAN,
          val : "details.invoiceId"
        },
        {
          key : "NUMBER_OF_RIDES",
          title : getString NUMBER_OF_RIDES,
          val : show details.totalRides
        },
        {key : "YOUR_EARNINGS",
        title : getString YOUR_EARNINGS,
        val : show details.totalEarnings},
        {key : "FEE_BREAKUP",
        title : getString FEE_BREAKUP,
        val : "details.invoiceId"},
        {key : "OFFER",
        title : getString OFFER,
        val : details.invoiceId}
      ],
      manualSpecificDetails : []}
  continue state { props{ subView = TransactionDetails}, data{transactionDetails = transactionDetails}}

eval ViewRideDetails state = continue state { props{ subView = RideDetails}}

-- eval (UpdatePaymentHistory response) state = do
eval (UpdatePaymentHistory) state = do
  -- let transactions = concatMap getAllTransactions response
  continue state{data{ transactions = dummyPaymentListItem }}

eval (DueDetailsListAction (DueDetailsListController.SelectDue index)) state = continue state

eval (Copy val) state = continueWithCmd state [ do 
    _ <- pure $ copyToClipboard val
    _ <- pure $ toast (getString COPIED)
    pure NoAction
  ]

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = exit $ SetupAutoPay state

eval _ state = continue state

-- getAllTransactions :: PaymentDetailsEntity -> Array PaymentListItem
-- getAllTransactions transaction = do
--     let processedTransaction = {
--         invoiceId:transaction.invoiceId,
--         ridesTakenDate : transaction.ridesTakenDate,
--         totalRides: transaction.totalRides,
--         transactionDate : transaction.date,
--         driverFeeId: transaction.driverFeeId,
--         paymentStatus: transaction.status,
--         totalEarnings: transaction.totalEarnings,
--         chargesBreakup: transaction.chargesBreakup,
--         totalCharges: transaction.charges
--     }
--     if null transaction.txn then 
--       [processedTransaction]
--     else do
--       let txns = map (\txn -> processedTransaction{  paymentStatus = txn.status }) transaction.txn
--       txns
      