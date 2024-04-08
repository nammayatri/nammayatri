module Locale.Utils (module Locale.Utils) where

import Prelude
import Data.Maybe
import Data.Function.Uncurried
import Data.Array
import DecodeUtil
import Helpers.Commons

getLanguageLocale :: String -> String
getLanguageLocale key = do
  let
    mbKey = runFn3 getFromWindowString key Nothing Just
  case mbKey of
    Nothing ->
      let
        language = getKeyInSharedPrefKeys "LANGUAGE_KEY"

        setLanguage = if any (_ == language) [ "__failed", "(null)", "", "" ] then "EN_US" else language
      in
        runFn2 setInWindow key setLanguage
    Just key -> key

setLanguageLocale :: String -> String
setLanguageLocale locale = do
  let
    _ = runFn2 setKeyInSharedPref "LANGUAGE_KEY" locale
  runFn2 setInWindow "languageKey" locale
