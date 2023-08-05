module Screens.PaymentHistoryScreen.ScreenData where

import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..))

initData :: PaymentHistoryScreenState
initData = {
    data: {

    },

    props: {
        subView : TransactionDetails,
        autoPayHistory : true
    }
}