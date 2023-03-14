module Language.Strings where

import Prelude (($))
import Language.Types (STR)
import JBridge (getKeyInSharedPrefKeys)
import EN (getEN) 
import KN (getKN)
import HI (getHI)

data LANGUAGE_KEY = EN_US | KN_IN | HI_IN

getKey :: String -> LANGUAGE_KEY
getKey key = do
    case key of 
        "EN_US" -> EN_US
        "KN_IN" -> KN_IN
        "HI_IN" -> HI_IN
        _ -> EN_US

--TODO: Translate in OR AS
getString :: STR -> String
getString key = do
    let language = getKey $ getKeyInSharedPrefKeys "LANGUAGE_KEY"
    case language of
        EN_US -> getEN key
        KN_IN -> getKN key
        HI_IN -> getHI key