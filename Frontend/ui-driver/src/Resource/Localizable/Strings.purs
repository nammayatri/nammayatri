{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Language.Strings where

import Data.String.Common (trim)
import Language.Types (STR)
import MerchantConfig.Utils (getStringFromConfig, getStringWithVar, getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))
import Prelude (($))
import ConfigJBridge (getKeyInSharedPrefKeysConfig)
import Data.Maybe (Maybe(..))
import Resources.Localizable.BN (getBN, getBnFromJS)
import Resources.Localizable.EN (getEN, getEnFromJS)
import Resources.Localizable.HI (getHI, getHiFromJS)
import Resources.Localizable.KN (getKN, getKnFromJS)
import Resources.Localizable.ML (getML, getMlFromJS)
import Resources.Localizable.TA (getTA, getTaFromJs)
import Resources.Localizable.FR (getFR)
import Resources.Localizable.TE (getTE, getTeFromJS)
import Locale.Utils
import Prelude
import Data.String as DS
import Language.Types2 
import Debug 
getString :: STR -> String
getString key = 
  let language = getLanguageLocale languageKey
  in getStringFromConfigOrLocal language key

getStringEnToHi :: STR -> String
getStringEnToHi key = 
  let language = getLanguageLocale "languageKey"
  in case language of
    "EN_US" -> 
      if getMerchant FunctionCall == YATRISATHI 
        then getStringFromLocal "HI_IN" key 
        else getStringFromConfigOrLocal language key
    _ -> getStringFromConfigOrLocal language key
    
getStringFromConfigOrLocal :: String -> STR -> String
getStringFromConfigOrLocal language key = 
  case (getStringFromConfig key Just Nothing) of
    Just value -> value
    Nothing    -> getStringFromLocal language key

getStringFromLocal :: String -> STR -> String
getStringFromLocal language key = do
  case language of
    "BN_IN" -> getBnFromJS $ getJSKey key
    "HI_IN" -> getHiFromJS $ getJSKey key
    "KN_IN" -> getKnFromJS $ getJSKey key
    "ML_IN" -> getMlFromJS $ getJSKey key
    "TA_IN" -> getTaFromJs $ getJSKey key
    "TE_IN" -> getTeFromJS $ getJSKey key
    _       -> getEnFromJS $ getJSKey key

getVarString :: STR -> Array String -> String
getVarString key vals = getStringWithVar (getString key) vals

getStringWithoutNewLine :: STR -> String
getStringWithoutNewLine str = DS.replace (DS.Pattern "\n") (DS.Replacement " ") $ getString str