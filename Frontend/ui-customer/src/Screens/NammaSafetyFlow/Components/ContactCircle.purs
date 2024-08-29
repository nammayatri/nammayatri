module Screens.NammaSafetyFlow.Components.ContactCircle where

import Common.Types.App (LazyCheck(..))
import Data.Array (take, (!!))
import Data.Maybe (fromMaybe, Maybe(..))
import Prelude (Unit, const, ($), (&&), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), PrestoDOM, alignParentRight, background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, relativeLayout, text, textView, visibility, width, clickable)
import Screens.Types (NewContacts)
import Styles.Types (Color)
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Font.Style as FontStyle
import Helpers.Utils as HU
import Mobility.Prelude (boolToVisibility)
import Styles.Colors as Color
import Services.API as API
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption)
import Data.Maybe (Maybe(..))

view :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
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
            , background viewDetails.backgroundColor
            , cornerRadius if EHC.os == "IOS" then 16.0 else 20.0
            , gravity CENTER
            , onClick push $ const $ OnClick config.index
            , clickable config.isClickable
            ]
              <> if config.enableCheckmark then [ margin $ MarginTop 10 ] else []
          )
          [ textView
              $ [ text viewDetails.text'
                , color viewDetails.textColor
                ]
              <> FontStyle.tags TypoGraphy
          ]
      , linearLayout
          [ height $ V 24
          , width MATCH_PARENT
          ]
          [ HV.layoutWithWeight
          , imageView
              [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_tick_black_white"
              , width $ V 24
              , height $ V 24
              , alignParentRight "true,-1"
              , gravity RIGHT
              , visibility $ boolToVisibility $ contact.priority == 0 && config.enableCheckmark
              ]
          ]
      ]
  where
  viewDetails = getContactViewDetails config.index config.contact.name

getContactViewDetails :: Int -> String -> ContactViewDetails 
getContactViewDetails  index name = 
  {
    backgroundColor : fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0),
    textColor : fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1),
    text' : (DS.toUpper ((<>) (getFirstChar name) (getLastChar name)))
  }


getFirstChar :: String -> String
getFirstChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 0))

getLastChar :: String -> String
getLastChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 1))

getNameInitials :: String -> (Array String)
getNameInitials fullName = (take 2 (DS.split (DS.Pattern " ") (fullName)))

type Config
  = { contact :: NewContacts
    , index :: Int
    , enableCheckmark :: Boolean
    , isClickable :: Boolean
    }

type ContactViewDetails  
  = { backgroundColor :: String
    , textColor :: String
    , text' :: String
  }

config :: Config
config =
  { contact:
      { name: ""
      , priority: 1
      , number: ""
      , isSelected: false
      , enableForFollowing: false
      , enableForShareRide: false
      , shareTripWithEmergencyContactOption: neverShareRideOption
      , onRide : false
      , contactPersonId : Nothing
      , isFollowing: Nothing
      , notifiedViaFCM : Nothing
      }
  , index: 0
  , enableCheckmark: true
  , isClickable: true
  }

getContactConfig :: NewContacts -> Int -> Boolean -> Boolean -> Config
getContactConfig contact index enableCheckmark clickable =
  { contact: contact
  , index: index
  , enableCheckmark: enableCheckmark
  , isClickable: clickable
  }

contactColorsList :: Array (Array Color)
contactColorsList =
  [ [ Color.yellow900, Color.black800 ]
  , [ Color.blue800, Color.white900 ]
  , [ Color.orange800, Color.black800 ]
  ]

data Action
  = OnClick Int
