{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.IncrementDecrement.Controller where

import Prelude 
import Common.Types.App
import Effect (Effect)
import Styles.Colors as Color
import Data.Maybe (Maybe(..))

type Config = {
    initialCount :: Int
  , plus :: PlusMinusObj
  , minus :: PlusMinusObj
  , stroke :: String
  , backgroundColor :: String
  , fontSize :: String
  , cornerRadius :: String
  , padding :: String
  , margin :: String
  , onChange :: Maybe (Int -> Effect Unit)
}

type PlusMinusObj = {
    backgroundColor :: String
  , textColor :: String
  , height :: String
  , width :: String
  , fontSize :: String
  , fontWeight :: String
  , rippleColor :: String
}

config :: Config
config = 
  { initialCount: 0
  , plus: {
      backgroundColor: Color.black900
    , textColor: Color.yellow900
    , height: "wrap_content"
    , width: "wrap_content"
    , fontSize: "16"
    , fontWeight: "bold"
    , rippleColor: Color.black500
    }
  , minus: {
      backgroundColor: Color.grey700
    , textColor: Color.black900
    , height: "wrap_content"
    , width: "wrap_content"
    , fontSize: "16"
    , fontWeight: "bold"
    , rippleColor: Color.black200
    }
  , stroke: "1," <> Color.grey700
  , backgroundColor: Color.white900
  , fontSize: "16"
  , cornerRadius: "4.0"
  , padding: "28, 7, 28, 7"
  , margin: "0, 24, 0, 24"
  , onChange: Nothing
  }