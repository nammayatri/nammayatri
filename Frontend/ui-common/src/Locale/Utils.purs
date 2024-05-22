module Locale.Utils (module Locale.Utils, module ReExport) where

import Prelude
import DecodeUtil
import Data.Maybe
import JBridge
import Data.Function.Uncurried
import Constants as ReExport
import Data.Array
import Data.Int as INT
import Data.String as DS
import Data.Tuple


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

compareVersions :: String -> String -> Boolean
compareVersions version1 version2 =
  let components1 = DS.split (DS.Pattern ".") version1
      components2 = DS.split (DS.Pattern ".") version2
      padded1 = padVersion components1 components2
      padded2 = padVersion components2 components1
  in foldl (\acc (Tuple x y) -> if (x > y) then true else acc) false (zip padded1 padded2)
  where
    -- Parse each component to an integer and pad with remaining elements if necessary
    padVersion :: Array String -> Array String -> Array Int
    padVersion version longerVersion =
      let padded = map (\s -> fromMaybe 0 (INT.fromString s)) version
          remaining = drop (length version) longerVersion
          paddedRight = map (\s -> fromMaybe 0 (INT.fromString s)) remaining
      in padded <> paddedRight