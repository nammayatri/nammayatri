module  Merchant.Utils where

import Prelude(unit, (/=))
import Common.Types.App (LazyCheck)
import Data.Maybe (Maybe(..))
-- import Helpers.Utils (getMerchant, Merchant(..))
import Screens.Types (Language)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Prelude (class Eq, class Show, (<<<), Unit)
import Foreign.Class (class Decode, class Encode, decode)
import Data.Eq.Generic (genericEq)
import Foreign (Foreign)
import Data.Either (hush)
import Control.Monad.Except (runExcept)

foreign import getStringFromConfig :: String -> String
foreign import getValueFromConfig :: String -> String

foreign import getValueFromConfig :: String -> String

foreign import getENStrings :: String -> String

getLanguage :: LazyCheck -> Array Language
getLanguage language = do
  case getMerchant unit of
    NAMMAYATRIPARTNER -> 
        [
        {name:"English",value:"EN_US", subtitle: ""}, 
        {name:"ಕನ್ನಡ",value:"KN_IN", subtitle: "Kannada"},
        {name:"हिंदी",value:"HI_IN", subtitle: "Hindi"},
        {name:"தமிழ்",value:"TA_IN", subtitle: "Tamil"}
        ]
    JATRISAATHIDRIVER -> 
        [
        {name:"English",value:"EN_US", subtitle: ""}, 
        {name:"বাংলা",value:"BN_IN", subtitle: "Bengali"},
        {name:"हिंदी",value:"HI_IN", subtitle: "Hindi"}
        ]
    YATRIPARTNER  ->
        [
        {name:"English",value:"EN_US", subtitle: ""}, 
        {name:"മലയാളം",value:"ML_IN", subtitle: "Malayalam"}
        ]

data Merchant = NAMMAYATRIPARTNER | JATRISAATHIDRIVER | YATRIPARTNER

derive instance genericMerchant :: Generic Merchant _
instance eqMerchant :: Eq Merchant where eq = genericEq
instance showMerchant :: Show Merchant where show = genericShow
instance encodeMerchant :: Encode Merchant where encode = defaultEnumEncode
instance decodeMerchant:: Decode Merchant where decode = defaultEnumDecode


getMerchant :: Unit -> Merchant
getMerchant unit = case (decodeMerchantId (getMerchantId "")) of
  Just merchant -> merchant
  Nothing -> NAMMAYATRIPARTNER

decodeMerchantId :: Foreign -> Maybe Merchant
decodeMerchantId = hush <<< runExcept <<< decode

foreign import getMerchantId :: String -> Foreign