module Services.Config where

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Debug (spy)
import Helpers.Commons (getKeyInSharedPrefKeys)
import Prelude (class Eq, (==), (||))

foreign import environment :: String -> String

foreign import getMerchant :: String -> String

data Env
  = LOCAL
  | DEV
  | UAT
  | PROD

derive instance genericEnv :: Generic Env _

instance eqEnv :: Eq Env where
  eq = genericEq

newtype Config
  = Config
  { baseUrl :: String
  , fingerprint :: String
  }

getEnv :: Env
getEnv = case spy "Selected Environment :- " (environment "") of
  "local" -> LOCAL
  "master" -> DEV
  "sandbox" -> UAT
  "prod" -> PROD
  _ -> PROD

getMerchantId :: String -> String
getMerchantId dummy = "NA"

getConfig :: Config
getConfig = do
  case getEnv of
    LOCAL ->
      Config
        { baseUrl: ""
        , fingerprint: ""
        }
    DEV ->
      Config
        { baseUrl: getKeyInSharedPrefKeys "BASE_URL"
        , fingerprint: ""
        }
    UAT ->
      Config
        { baseUrl: getKeyInSharedPrefKeys "BASE_URL"
        , fingerprint: ""
        }
    PROD ->
      Config
        { baseUrl: getKeyInSharedPrefKeys "BASE_URL"
        , fingerprint: ""
        }

getEndpoint :: String -> String
getEndpoint dummy = do
  if ((getKeyInSharedPrefKeys "MOBILE_NUMBER_KEY") == "" || (getKeyInSharedPrefKeys "MOBILE_NUMBER_KEY") == "") then
    ""
  else
    let
      Config config = getConfig
    in
      config.baseUrl

getBaseUrl :: String -> String
getBaseUrl dummy = do
  if ((getKeyInSharedPrefKeys "MOBILE_NUMBER_KEY") == "" || (getKeyInSharedPrefKeys "MOBILE_NUMBER_KEY") == "") then
    ""
  else
    let
      Config config = getConfig
    in
      (config.baseUrl)

getFingerPrint :: String -> String
getFingerPrint dummy = do
  if ((getKeyInSharedPrefKeys "MOBILE_NUMBER_KEY") == "" || (getKeyInSharedPrefKeys "MOBILE_NUMBER_KEY") == "") then
    ""
  else
    let
      Config config = getConfig
    in
      config.fingerprint

getCustomerNumber :: String -> String
getCustomerNumber _ = case getEnv of
  DEV -> ""
  UAT -> ""
  PROD -> ""
  _ -> ""

getSupportNumber :: String -> String
getSupportNumber _ = case getEnv of
  DEV -> ""
  UAT -> ""
  PROD -> ""
  _ -> ""

getWhatsAppSupportNo :: String -> String
getWhatsAppSupportNo _ = case getEnv of
  DEV -> ""
  UAT -> ""
  PROD -> ""
  _ -> ""
