module Components.GenericRadioButton.Controller where

import Font.Style (Style(..))
import PrestoDOM (Padding(..))

data Action
  = OnSelect Int
  | TextChanged String String

type Config
  = { isSelected :: Boolean
    , buttonTextConfig :: TextConfig
    , padding :: Padding
    , activeButtonConfig :: ButtonConfig
    , inActiveButtonConfig :: ButtonConfig
    , id :: Int
    , showEditText :: Boolean
    , isLimitExceeded :: Boolean
    , editTextConfig ::
        { hint :: String
        , textStyle :: Style
        , id :: String
        , defaultText :: String
        }
    }

type ButtonConfig
  = { height :: Int
    , width :: Int
    , buttonHeight :: Int
    , buttonWidth :: Int
    , buttonColor :: String
    , textStyle :: Style
    , stroke :: String
    , background :: String
    }

type TextConfig
  = { text :: String
    , color :: String
    }

config :: Config
config =
  { activeButtonConfig:
      { height: 20
      , width: 20
      , buttonHeight: 10
      , buttonWidth: 10
      , buttonColor: ""
      , textStyle: Body4
      , stroke: ""
      , background: ""
      }
  , inActiveButtonConfig:
      { height: 20
      , width: 20
      , buttonHeight: 10
      , buttonWidth: 10
      , buttonColor: ""
      , textStyle: ParagraphText
      , stroke: ""
      , background: ""
      }
  , isSelected: true
  , padding: Padding 16 16 16 16
  , buttonTextConfig:
      { text: ""
      , color: ""
      }
  , id: 0
  , showEditText: false
  , isLimitExceeded: false
  , editTextConfig:
      { hint: ""
      , textStyle: Body1
      , id : ""
      , defaultText : ""
      }
  }
