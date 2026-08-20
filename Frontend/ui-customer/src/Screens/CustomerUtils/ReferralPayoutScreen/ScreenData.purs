module Screens.ReferralPayoutScreen.ScreenData where

import ConfigProvider
import Prelude
import MerchantConfig.Types (AppConfig)
import Screens.Types ( ReferralType(..))
import Data.Maybe(Maybe(..))
import Services.API


data UpiVerificationStatus = UpiVerified | UpiNotVerified | UpiFailed | UpiVerifying


derive instance eqUpiVerificationStatus :: Eq UpiVerificationStatus

type ReferralPayoutScreenState
  = { data ::
        ReferralPayoutScreenData
    , props ::
        ReferralPayoutScreenProps
    }

type ReferralPayoutScreenProps
  = { demoModePopup :: Boolean
    , isPayoutEnabled :: Boolean
    , showReferralFaq :: Boolean
    , upiEditTextFocused :: Boolean
    , showShareAppQr :: Boolean
    , showUPIPopUp :: Boolean
    , isEarnings :: Boolean
    , showUpiSuccess :: Boolean
    , isUpiUpdated :: Boolean
    }

type ReferralPayoutScreenData
  = { appConfig :: AppConfig
    , verificationStatus :: UpiVerificationStatus
    , vpa :: String
    , referralCode :: String
    , referreeCode :: String
    , existingVpa :: Maybe String
    , referralType :: ReferralType
    , referralEarnings :: Number
    , referredByEarnings :: Number
    , history :: Array PayoutItem
    , referralAmountPaid :: Number
    }

initData :: ReferralPayoutScreenState
initData =
  { data:
      { appConfig: getAppConfig appConfig
      , verificationStatus : UpiNotVerified
      , vpa : ""
      , existingVpa : Nothing
      , referralCode : ""
      , referralType : GIVE_REFERRAL
      , referralEarnings : 0.0
      , referredByEarnings : 0.0
      , referralAmountPaid : 0.0
      , history : []
      , referreeCode : ""
      }
  , props:
      { demoModePopup: false
      , showReferralFaq: false
      , upiEditTextFocused: false
      , isPayoutEnabled: false
      , showShareAppQr: false
      , showUPIPopUp: false
      , isEarnings: false
      , showUpiSuccess : false
      , isUpiUpdated : false
      }
  }
