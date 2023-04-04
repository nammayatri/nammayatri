module Merchant.Utils where

import Common.Types.App (LazyCheck(..))
import Helpers.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Screens.Types (FareTypes(..), Language)

foreign import getString' :: String -> String

foreign import getValueFromConfig :: String -> String

type LanguageData
  = { languages :: Array Language
    }

getLanguagesList :: LazyCheck -> Array Language
getLanguagesList lazy = case (getMerchant FunctionCall) of
  NAMMAYATRI ->
    [ { name: "English"
      , value: "EN_US"
      , subTitle: ""
      }
    , { name: "ಕನ್ನಡ"
      , value: "KN_IN"
      , subTitle: "Kannada"
      }
    , { name: "हिंदी"
      , value: "HI_IN"
      , subTitle: "Hindi"
      }
    ]
  JATRISAATHI ->
    [ { name: "English"
      , value: "EN_US"
      , subTitle: ""
      }
    , { name: "বাংলা"
      , value: "BN_IN"
      , subTitle: "Bengali"
      }
    , { name: "हिंदी"
      , value: "HI_IN"
      , subTitle: "Hindi"
      }
    ]
  YATRI ->
    [ { name: "English"
      , value: "EN_US"
      , subTitle: ""
      }
    , { name: "മലയാളം"
      , value: "ML_IN"
      , subTitle: "Malayalam"
      }
    ]
