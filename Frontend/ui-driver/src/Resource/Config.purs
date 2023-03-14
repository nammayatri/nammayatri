module Services.Config where

import Prelude (class Eq, (==), (||))
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)
import ConfigJBridge (getKeyInSharedPrefKeysConfig)
import Log (printLog)
import Debug.Trace (spy)

foreign import environment :: String -> String

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
        { baseUrl: "http://localhost:8014/v2"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        } 
    DEV  -> Config
        { baseUrl: "https://api.sandbox.beckn.juspay.in/dev/dobpp/ui"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    UAT  -> Config
        { baseUrl: "https://api.sandbox.beckn.juspay.in/dobpp/ui"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    PROD -> Config
        { baseUrl: "https://api.beckn.juspay.in/dobpp/ui"
        , fingerprint : "[\"6oc0n8dAm42JFWP2ClTuC0JqnHJ2IIszzTE98r5Om/I=\"]"
        }

getEndpoint :: String -> String
getEndpoint dummy = do
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "4000400040" || (getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "3000300030") then
    "api.sandbox.beckn.juspay.in/transport/v2"
    else 
      let Config config = getConfig
      in config.baseUrl

getBaseUrl :: String -> String
getBaseUrl dummy = do
  let a = printLog "dummy" dummy
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "4000400040" || (getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "3000300030") then
    "https://api.sandbox.beckn.juspay.in/dobpp/ui"
    else
      let Config config = getConfig
      in (config.baseUrl)

getFingerPrint :: String -> String
getFingerPrint dummy = do
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "4000400040" || (getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "3000300030") then
    "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
    else 
      let Config config = getConfig
      in config.fingerprint

getCustomerNumber :: String -> String
getCustomerNumber _ = case getEnv of 
                        DEV  -> "08069456455"
                        UAT  -> "08069457952"
                        PROD -> "08069457986"
                        _    -> ""

getSupportNumber :: String -> String
getSupportNumber _ = case getEnv of 
                        DEV  -> "08068501081"
                        UAT  -> "08068501081"
                        PROD -> "08068501081"
                        _ -> ""
