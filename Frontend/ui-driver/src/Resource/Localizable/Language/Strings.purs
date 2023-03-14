module Language.Strings where

import Prelude (($))
import Language.Types (STR)
import JBridge (getKeyInSharedPrefKeys)
import EN (getEN) 
import HI (getHI)
import KN (getKN)
import TA (getTA)

data Language = EN_US | KN_IN | HI_IN | TA_IN 

getKey :: String -> Language
getKey key = do
    case key of 
        "EN_US" -> EN_US
        "KN_IN" -> KN_IN
        "HI_IN" -> HI_IN
        "TA_IN" -> TA_IN
        _ -> EN_US

--TODO: Translate in OR AS
getString :: STR -> String
getString key = do
    let language = getKey $ getKeyInSharedPrefKeys "LANGUAGE_KEY"
    case language of
        EN_US -> getEN key
        KN_IN -> getKN key
        HI_IN -> getHI key
        TA_IN -> getTA key

