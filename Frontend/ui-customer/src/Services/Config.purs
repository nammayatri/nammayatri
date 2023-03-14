module Services.Config where

import Debug.Trace (spy)
import Prelude (class Eq, (==))
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)
import ConfigJBridge (getKeyInSharedPrefKeysConfig)

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
        { baseUrl: "http://localhost:8013/v2"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    DEV  -> Config
        { baseUrl: "https://api.sandbox.beckn.juspay.in/dev/app/v2"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    UAT  -> Config
        { baseUrl: "https://api.sandbox.beckn.juspay.in/pilot/app/v2"
        , fingerprint : "[\"GwTg/ASRViI4veIkOMZTXPAKc6ct/ocbQHPdMzPhdn0=\"]"
        }
    PROD -> Config
        { baseUrl: "https://api.beckn.juspay.in/pilot/app/v2"
        , fingerprint : "[\"6oc0n8dAm42JFWP2ClTuC0JqnHJ2IIszzTE98r5Om/I=\"]"
        }

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
                        DEV  -> "08069456526"
                        UAT  -> "08069457934"
                        PROD -> "08069457995"
                        _    -> ""

getSupportNumber :: String -> String
getSupportNumber _ = case getEnv of 
                        DEV  -> "08068501060"
                        UAT  -> "08068501060"
                        PROD -> "08068501060"
                        _    -> "08068501060"