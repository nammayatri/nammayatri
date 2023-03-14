module Screens.BankDetailScreen.ScreenData where

import Screens.Types
initData :: BankDetailScreenState
initData = {
    data: {
        beneficiaryNumber: "",
        ifsc: ""
    },
    props: {
        openRegistrationModal: false,
        inputData: "",
        isBeneficiaryMatching: false
    }
}
