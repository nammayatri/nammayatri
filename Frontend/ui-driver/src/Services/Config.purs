{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.Config where

import Prelude (class Eq, (==), (||))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import ConfigJBridge (getKeyInSharedPrefKeysConfig, getValueToLocalNativeStoreConfig)
import Log (printLog)
import Debug (spy)

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

getMerchantId :: String ->  String
getMerchantId dummy = "NA"

getConfig :: Config
getConfig = do
  case getEnv of
    LOCAL -> Config
        { baseUrl: ""
        , fingerprint : ""
        }
    DEV  -> Config
        { baseUrl: getValueToLocalNativeStoreConfig "BASE_URL"
        , fingerprint : ""
        }
    UAT  -> Config
        { baseUrl: getValueToLocalNativeStoreConfig "BASE_URL"
        , fingerprint : ""
        }
    PROD -> Config
        { baseUrl: getValueToLocalNativeStoreConfig "BASE_URL"
        , fingerprint : ""
        }

getEndpoint :: String -> String
getEndpoint dummy = do
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "" || (getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "") then
    ""
    else
      let Config config = getConfig
      in config.baseUrl

getBaseUrl :: String -> String
getBaseUrl dummy = do
  let a = printLog "dummy" dummy
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "" || (getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "") then
    ""
    else
      let Config config = getConfig
      in (config.baseUrl)

getFingerPrint :: String -> String
getFingerPrint dummy = do
  if ((getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "" || (getKeyInSharedPrefKeysConfig "MOBILE_NUMBER_KEY") == "") then
    ""
    else
      let Config config = getConfig
      in config.fingerprint

getCustomerNumber :: String -> String
getCustomerNumber _ = case getEnv of
                        DEV  -> ""
                        UAT  -> ""
                        PROD -> ""
                        _    -> ""

getSupportNumber :: String -> String
getSupportNumber _ = case getEnv of
                        DEV  -> ""
                        UAT  -> ""
                        PROD -> ""
                        _ -> ""

getWhatsAppSupportNo :: String -> String
getWhatsAppSupportNo _ = case getEnv of
                        DEV  -> ""
                        UAT  -> ""
                        PROD -> ""
                        _ -> ""
                  