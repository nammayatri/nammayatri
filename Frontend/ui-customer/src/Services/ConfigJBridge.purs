module ConfigJBridge where

import Effect (Effect)

foreign import getKeyInSharedPrefKeysConfig :: String -> String

foreign import getKeyInSharedPrefKeysConfigEff :: String -> Effect String
