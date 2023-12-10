module Services.Config where

import Debug
import Prelude (class Eq, (==))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import ConfigJBridge (getKeyInSharedPrefKeysConfig, getValueToLocalNativeStoreConfig)
foreign import environment :: String -> String

foreign import getMerchant :: String -> String

data Env = LOCAL | DEV | UAT | PROD
derive instance genericEnv :: Generic Env _
instance eqEnv :: Eq Env where eq = genericEq

newtype Config = Config
  { baseUrl :: String
  , fingerprint :: String
  }
getEnv :: Env
getEnv = case spy "Selected Environment :- " (environment "") of
  "local"       -> LOCAL
  "master"      -> DEV
  "sandbox"     -> UAT
  "prod"        -> PROD
  _             -> PROD

getConfig :: Config
getConfig = do
  case getEnv of
    LOCAL -> Config
        { baseUrl: "http://localhost:8013/v2"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    DEV  -> Config
        { baseUrl: "https://api.beckn.juspay.in/pilot/app/v2"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    UAT  -> Config
        { baseUrl: "https://api.beckn.juspay.in/pilot/app/v2"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    PROD -> Config
        { baseUrl: "https://api.beckn.juspay.in/pilot/app/v2"
        , fingerprint : "[\"6oc0n8dAm42JFWP2ClTuC0JqnHJ2IIszzTE98r5Om/I=\"]"
        }
getMerchantId :: String -> String
getMerchantId dummy = case (getMerchant "") of 
  "NAMMAYATRI" -> "NAMMA_YATRI"
  "YATRISATHI" -> "JATRI_SAATHI"
  "YATRI" -> "YATRI"
  "MOBILITY_PM" -> "MOBILITY_PAYTM"
  "PASSCULTURE" -> "MOBILITY_PASSCULTURE"
  "MOBILITY_RS" -> "MOBILITY_REDBUS"
  _ -> "NAMMA_YATRI"

getEndpoint :: String -> String
getEndpoint dummy = do
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER") == "5000500050") then
    "api.sandbox.beckn.juspay.in/dev/app/v2"
    else 
      let Config config = getConfig
      in config.baseUrl
      
getBaseUrl :: String -> String
getBaseUrl dummy = do
  let a = spy "dummy" dummy
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER") == "5000500050") then
    spy "getBaseUrl inside if" "https://api.sandbox.beckn.juspay.in/dev/app/v2"
    else
      let Config config = getConfig
      in spy "getBaseUrl inside else" (config.baseUrl)

getFingerPrint :: String -> String
getFingerPrint dummy = do
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER") == "5000500050") then
    "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
    else 
      let Config config = getConfig
      in config.fingerprint

getDriverNumber :: String -> String
getDriverNumber _ = case getEnv of 
                        DEV  -> (getDevNumber "").driverNumber
                        UAT  -> (getDevNumber "").driverNumber
                        PROD -> (getProdNumber "").driverNumber
                        _    -> (getDevNumber "").driverNumber

getSupportNumber :: String -> String
getSupportNumber _ = case getEnv of 
                        DEV  -> (getDevNumber "").supportNumber
                        UAT  -> (getDevNumber "").supportNumber
                        PROD -> (getProdNumber "").supportNumber
                        _    -> (getDevNumber "").supportNumber


getDevNumber :: String -> { supportNumber :: String , driverNumber :: String }
getDevNumber _ = case (getMerchant "") of 
    "YATRISATHI" -> {supportNumber : "08068501060", driverNumber : "03340585169"}
    "YATRI"       -> {supportNumber : "08068501060", driverNumber : "08047108594"}
    "UNKNOWN"     -> {supportNumber : "08069490360", driverNumber : "08069456526"}
    _             -> {supportNumber : "08068501060", driverNumber : "08069456526"}


getProdNumber :: String -> { supportNumber :: String , driverNumber :: String}
getProdNumber _ = case (getMerchant "") of
    "YATRISATHI" -> {supportNumber : "08069724888", driverNumber : "03340585514"}
    "YATRI"       -> {supportNumber : "07411782309", driverNumber : "08047108594"}
    "UNKNOWN"     -> {supportNumber : "08069490360", driverNumber : "08069457995"}
    _             -> {supportNumber : "08068501060", driverNumber : "08069457995"}