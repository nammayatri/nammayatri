module Components.StatsModel.Controller where

import PrestoDOM ( Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Prelude
import Effect (Effect)

data Action = NoAction

type Config =
  { countTextConfig :: CountTextConfig,
    earningsTextConfig :: EarningsTextConfig,
    textConfig :: TextConfig,
    totalRidesOfDay :: Int,
    totalEarningsOfDay :: Int,
    visibility :: Visibility
  }

type CountTextConfig =
  { width :: Length
  , height :: Length
  , text :: String
  , color :: String 
  , visibility :: Visibility
  , gravity :: Gravity
  , weight :: Number
  }

type EarningsTextConfig =
  { width :: Length
  , height :: Length
  , text :: String
  , color :: String 
  , visibility :: Visibility
  , gravity :: Gravity
  , weight :: Number
  }

type TextConfig =
  { width :: Length
  , height :: Length
  , text :: String
  , color :: String 
  , visibility :: Visibility
  , gravity :: Gravity
  , weight :: Number
  }

config :: Config 
config = 
  { countTextConfig :
    { width : MATCH_PARENT
    , height : WRAP_CONTENT
    , text : ""
    , color : Color.black800
    , visibility : VISIBLE
    , gravity : CENTER_HORIZONTAL
    , weight : 1.0
    },
    earningsTextConfig :
    { width : MATCH_PARENT
    , height : WRAP_CONTENT
    , text : ""
    , color : Color.black800
    , visibility : VISIBLE
    , gravity : CENTER_HORIZONTAL
    , weight : 1.0
    },
    textConfig :
    { width : MATCH_PARENT
    , height : WRAP_CONTENT
    , text : ""
    , color : Color.black800
    , visibility : VISIBLE
    , gravity : CENTER_HORIZONTAL
    , weight : 1.0
    },
    totalRidesOfDay : 2,
    totalEarningsOfDay : 2,
    visibility : VISIBLE
  }