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
import MerchantConfig.Utils (getStringFromConfig, getStringWithVar)
import Prelude (($))
import Data.Maybe (Maybe(..))
import Debug (spy)
import ConfigJBridge (getKeyInSharedPrefKeysConfig)
import Resources.Localizable.BN (getBN)
import Resources.Localizable.EN (getEN)
import Resources.Localizable.HI (getHI)
import Resources.Localizable.KN (getKN)
import Resources.Localizable.ML (getML)
import Resources.Localizable.FR (getFR)

getString :: STR -> String
getString key = 
  let language = getKeyInSharedPrefKeysConfig "LANGUAGE_KEY"
  in getStringFromConfigOrLocal language key

getStringFromConfigOrLocal :: String -> STR -> String
getStringFromConfigOrLocal language key = 
  case (getStringFromConfig key Just Nothing) of
    Just value -> value
    Nothing    -> getStringFromLocal language key
  
getVarString :: STR -> Array String -> String
getVarString key vals = getStringWithVar (getString key) vals

getStringFromLocal :: String -> STR -> String
getStringFromLocal language key = 
  case language of
    "BN_IN" -> getBN key
    "HI_IN" -> getHI key
    "KN_IN" -> getKN key
    "ML_IN" -> getML key
    "FR_FR" -> getFR key
    _       -> getEN key