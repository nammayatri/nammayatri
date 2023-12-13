{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.StatsModel.Controller where

import PrestoDOM (Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Prelude
import Effect (Effect)

data Action
  = NoAction
  | OnIconClick

type Config
  = { countTextConfig :: TextConfig
    , earningsTextConfig :: TextConfig
    , bonusTextConfig :: TextConfig
    , textConfig :: TextConfig
    , totalRidesOfDay :: Int
    , totalEarningsOfDay :: Int
    , bonusEarned :: Int
    , visibility :: Visibility
    }

type TextConfig
  = { width :: Length
    , height :: Length
    , text :: String
    , color :: String
    , visibility :: Visibility
    , gravity :: Gravity
    , weight :: Number
    }

config :: Config
config =
  { countTextConfig:
      { width: MATCH_PARENT
      , height: WRAP_CONTENT
      , text: ""
      , color: Color.black800
      , visibility: VISIBLE
      , gravity: CENTER_HORIZONTAL
      , weight: 1.0
      }
  , earningsTextConfig:
      { width: MATCH_PARENT
      , height: WRAP_CONTENT
      , text: ""
      , color: Color.black800
      , visibility: VISIBLE
      , gravity: CENTER_HORIZONTAL
      , weight: 1.0
      }
  , bonusTextConfig:
      { width: MATCH_PARENT
      , height: WRAP_CONTENT
      , text: ""
      , color: Color.green900
      , visibility: VISIBLE
      , gravity: CENTER_HORIZONTAL
      , weight: 1.0
      }
  , textConfig:
      { width: MATCH_PARENT
      , height: WRAP_CONTENT
      , text: ""
      , color: Color.black800
      , visibility: VISIBLE
      , gravity: CENTER_HORIZONTAL
      , weight: 1.0
      }
  , totalRidesOfDay: 2
  , totalEarningsOfDay: 2
  , bonusEarned: 0
  , visibility: VISIBLE
  }
