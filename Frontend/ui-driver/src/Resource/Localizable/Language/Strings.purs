{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

