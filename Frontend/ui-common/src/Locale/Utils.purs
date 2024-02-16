module Locale.Utils (module Locale.Utils, module ReExport) where

import Prelude
import DecodeUtil
import Data.Maybe
import JBridge
import Data.Function.Uncurried
import Constants as ReExport
import Data.Array

getLanguageLocale :: String -> String
getLanguageLocale key = do
  let mbKey = runFn3 getFromWindowString key Nothing Just
  case mbKey of
    Nothing ->
      let language = getKeyInSharedPrefKeys "LANGUAGE_KEY"
          setLanguage = if any (_ == language) ["__failed", "(null)", "", ""] then "EN_US" else language 
      in runFn2 setInWindow key setLanguage
    Just key -> key

setLanguageLocale :: String -> String
setLanguageLocale locale = do
  let _ =  runFn2 setKeyInSharedPref "LANGUAGE_KEY" locale
  runFn2 setInWindow ReExport.languageKey locale
