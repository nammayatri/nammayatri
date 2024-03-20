{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.TipsView.Controller where

import Prelude (negate)
import PrestoDOM ( Margin(..))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Prelude (class Eq, class Show)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)

data Action
  = NoAction
  | TipBtnClick Int Int
  | AddTip
  | ChangeTip

type Config 
  = { activeIndex :: Int
    , isVisible :: Boolean
    , customerTipArray :: Array String
    , customerTipArrayWithValues :: Array Int
    , enableTips :: Boolean
    , tipLayoutMargin :: Margin
    , fareEstimate :: String
    , tipSelected :: String
    , fareEstimateText :: String
    , tipSelectedText :: String
    , showTipInfo :: Boolean
    }

config :: Config
config = 
  { customerTipArray : []
  , customerTipArrayWithValues : []
  , activeIndex : 0
  , isVisible : true
  , enableTips : true
  , tipLayoutMargin : (Margin 0 0 0 0)
  , fareEstimate : ""
  , tipSelected : ""
  , fareEstimateText : ""
  , tipSelectedText : ""
  , showTipInfo : false
  }