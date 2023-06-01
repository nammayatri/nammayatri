module Merchant.Utils where

import Common.Types.App (LazyCheck(..))
import Prelude ((<>), (==), (&&), class Show, class Ord, class Eq, (<<<))
import Screens.Types ( Language)
import Engineering.Helpers.Commons (os)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (Foreign, decodeJSON, encodeJSON, decode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Maybe(Maybe(..))
import Data.Either(hush)
import Control.Monad.Except (runExcept)

foreign import getStringFromConfig :: String -> String

foreign import getValueFromConfig :: String -> String

foreign import getENStrings :: String -> String

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
  YATRISATHI ->
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

getMerchant :: LazyCheck -> Merchant
getMerchant lazy = case (decodeMerchantId (getMerchantId "")) of
  Just merchant -> merchant
  Nothing -> NAMMAYATRI

data Merchant = NAMMAYATRI | YATRISATHI | YATRI

derive instance genericMerchant :: Generic Merchant _
instance eqMerchant :: Eq Merchant where eq = genericEq
instance encodeMerchant :: Encode Merchant where encode = defaultEnumEncode
instance decodeMerchant:: Decode Merchant where decode = defaultEnumDecode

decodeMerchantId :: Foreign -> Maybe Merchant
decodeMerchantId = hush <<< runExcept <<< decode

foreign import getMerchantId :: String -> Foreign

showCarouselScreen :: LazyCheck -> Boolean
showCarouselScreen a = os == "ANDROID" && (getMerchant FunctionCall) == NAMMAYATRI