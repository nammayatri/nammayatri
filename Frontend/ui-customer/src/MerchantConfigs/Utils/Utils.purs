module Merchant.Utils where

import Prelude
import Common.Types.App (LazyCheck(..))
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign.Generic (class Decode, class Encode, decode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Screens.Types (Language)
import Debug

foreign import getStringFromConfig :: String -> String

foreign import getValueFromConfig :: String -> String

foreign import getENStrings :: String -> String

type LanguageData
  = { languages :: Array Language
    }

getLanguagesList :: LazyCheck -> Array Language
getLanguagesList lazy = case (getMerchant FunctionCall) of
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
  _ ->
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

foreign import getMerchantId :: String -> Foreign

data Merchant
  = NAMMAYATRI
  | JATRISAATHI
  | YATRI
  | UNKNOWN

derive instance genericMerchant :: Generic Merchant _

instance eqMerchant :: Eq Merchant where
  eq = genericEq

instance encodeMerchant :: Encode Merchant where
  encode = defaultEnumEncode

instance decodeMerchant :: Decode Merchant where
  decode = defaultEnumDecode

getMerchant :: LazyCheck -> Merchant
getMerchant lazy = case decodeMerchantId (getMerchantId "") of
  Just merchant -> merchant
  Nothing -> UNKNOWN

decodeMerchantId :: Foreign -> Maybe Merchant
decodeMerchantId = hush <<< runExcept <<< decode
