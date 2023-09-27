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
import Language.Types (STR, getStringFromEnum)
import MerchantConfig.Utils (getENStrings, getStringFromConfig, getStringWithVar)
import Prelude (($))

data Language = EN_US | KN_IN | HI_IN | TA_IN 

getString :: STR -> String
getString key = getStringFromConfig $ trim $ getStringFromEnum key

getEN :: STR -> String
getEN key = getENStrings $ trim $ getStringFromEnum key

getVarString :: STR -> Array String -> String
getVarString key vals = getStringWithVar (getString key) vals