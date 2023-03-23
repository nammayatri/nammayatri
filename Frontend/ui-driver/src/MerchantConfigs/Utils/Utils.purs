module  MerchantConfigs.Utils where

import Prelude(unit)
import Common.Types.App (LazyCheck)
import Data.Maybe (Maybe(..))
import Helpers.Utils (getMerchant, Merchant(..))
import Screens.Types (Language)

foreign import getString' :: String -> String

foreign import getValueFromMerchant :: String -> String

getLanguage :: LazyCheck -> Array Language
getLanguage language = do
  case getMerchant unit of
    Nothing -> []
    Just merchant -> case merchant of
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
                []