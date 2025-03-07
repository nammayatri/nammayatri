module Screens.ReferralPayoutScreen.ScreenData where

import ConfigProvider
import Prelude
import MerchantConfig.Types (AppConfig)
import Screens.Types ( ReferralType(..))


data UpiVerificationStatus = UpiVerified | UpiNotVerified | UpiFailed | UpiVerifying


derive instance eqUpiVerificationStatus :: Eq UpiVerificationStatus

type ReferralPayoutScreenState
  = { data ::
        ReferralPayoutScreenData
    , props ::
        ReferralPayoutScreenProps
    }

type ReferralPayoutScreenProps
  = { enableDemoModeCount :: Int
    , demoModePopup :: Boolean
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
    , referralType :: ReferralType
    , referralEarnings :: Number
    , referredByEarnings :: Number
    , referralAmountPaid :: Number
    }

initData :: ReferralPayoutScreenState
initData =
  { data:
      { appConfig: getAppConfig appConfig
      , verificationStatus : UpiNotVerified
      , vpa : ""
      , referralCode : ""
      , referralType : GIVE_REFERRAL
      , referralEarnings : 0.0
      , referredByEarnings : 0.0
      , referralAmountPaid : 0.0
      }
  , props:
      { enableDemoModeCount: 0
      , demoModePopup: false
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
