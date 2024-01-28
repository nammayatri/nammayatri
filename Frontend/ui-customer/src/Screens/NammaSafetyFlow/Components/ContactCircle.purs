module Screens.NammaSafetyFlow.Components.ContactCircle where

import Common.Types.App
import Data.Array
import Data.Maybe
import Prelude
import PrestoDOM
import Screens.Types
import Styles.Types

import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils as HU
import Mobility.Prelude
import Styles.Colors as Color

view :: forall w. Config -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
view config push =
  let
    size = if config.enableCheckmark then 42 else 32
    contact = config.contact
  in
    relativeLayout
      [ height $ V size
      , width $ V size
      , margin $ MarginHorizontal 5 5
      ]
      [ linearLayout
          ( [ height $ V 32
            , width $ V 32
            , background backgroundColor
            , cornerRadius if EHC.os == "IOS" then 12.0 else 20.0
            , gravity CENTER
            , onClick push $ const $ OnClick config.index
            ]
              <> if config.enableCheckmark then [ margin $ MarginTop 10 ] else []
          )
          [ textView
              $ [ text text'
                , color textColor
                ]
              <> FontStyle.tags TypoGraphy
          ]
      , imageView
          [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_tick_black_white"
          , width $ V 24
          , height $ V 24
          , alignParentRight "true,-1"
          , visibility $ boolToVisibility $ contact.priority == 0 && config.enableCheckmark
          ]
      ]
  where
  backgroundColor = fromMaybe "" (fromMaybe [] (contactColorsList !! config.index) !! 0)

  textColor = fromMaybe "" (fromMaybe [] (contactColorsList !! config.index) !! 1)

  text' = (DS.toUpper ((<>) (getFirstChar config.contact.name) (getLastChar config.contact.name)))


getFirstChar :: String -> String
getFirstChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 0))

getLastChar :: String -> String
getLastChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 1))

getNameInitials :: String -> (Array String)
getNameInitials fullName = (take 2 (DS.split (DS.Pattern " ") (fullName)))

type Config = 
    { contact :: NewContacts
    , index :: Int
    , enableCheckmark :: Boolean
    }

config :: Config
config = 
    { contact : 
        { name : ""
        , priority : 0
        , number : ""
        , isSelected : false
        , enableForFollowing : false
        }
    , index : 0
    , enableCheckmark : true
    }

getContactConfig :: NewContacts -> Int -> Boolean -> Config
getContactConfig contact index enableCheckmark =
    { contact: contact
    , index: index
    , enableCheckmark: enableCheckmark
    }

contactColorsList :: Array (Array Color)
contactColorsList =
  [ [ Color.yellow900, Color.black800 ]
  , [ Color.blue800, Color.white900 ]
  , [ Color.orange800, Color.black800 ]
  ]

data Action = OnClick Int