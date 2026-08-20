{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectContactsFlow.SelectContactsScreen.View where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, os, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, discard, pure, unit, void, map, not, bind, (<$>), ($), (&&), (-), (<), (<<<), (<>), (==), (>))
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), ScopedScreen, accessibility, accessibilityHint, afterRender, alignParentBottom, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollBarY, stroke, text, textSize, textView, visibility, weight, width, adjustViewWithKeyboard, scrollView, clickable)
import Screens.SelectContactsFlow.SelectContactsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (NewContacts, CheckBoxSelectionConfig, Stage(..))
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (FetchImageFrom(FF_COMMON_ASSET), fetchImage, storeCallBackContacts)
import Data.Array (difference, length, null, take, (!!))
import Data.String (split, Pattern(..))
import Components.PopUpModal as PopUpModal
import Screens.SelectContactsFlow.SelectContactsScreen.ComponentConfig
import PrestoDOM.List as PrestoList
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Engineering.Helpers.Utils (terminateLoader)
import JBridge as JB
import Effect.Uncurried (runEffectFn2)
import Data.Maybe (Maybe(..))
import Screens.SelectContactsFlow.SelectContactsScreen.ScreenData

screen :: SelectContactsScreenState -> PrestoList.ListItem -> ScopedScreen Action SelectContactsScreenState ScreenOutput
screen initialState listItem =
  { initialState
  , view: view listItem
  , name: "SelectContactsScreen"
  , parent : Nothing
  , globalEvents : [ 
        ( \push -> do
                    void $ storeCallBackContacts push ContactsCallback
                    void $ runEffectFn2 JB.storeKeyBoardCallback push KeyboardCallback
                    pure $ pure unit
        )
  ]
  , eval:
      \action state -> do
        let
          _ = spy "SelectContactScreen action " action
        let
          _ = spy "SelectContactScreen state " state
        eval action state
  }

view :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> SelectContactsScreenState ->  PrestoDOM (Effect Unit) w
view listItem push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , onBackPressed push (const BackPressAction)
  , background Color.white900
  , padding if os == "IOS" then (Padding 0 safeMarginTop 0 0) else (Padding 0 0 0 0)
  , afterRender push $ const AfterRender
  ][
      if state.props.showContacts then (contactListView listItem push state) else emptyTextView state
    ]

contactListView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> SelectContactsScreenState -> PrestoDOM (Effect Unit) w
contactListView listItemm push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        ]
        [ GenericHeader.view (push <<< ContactListGenericHeaderActionController) (genericHeaderConfig state)
        , horizontalLine
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 44
        , orientation HORIZONTAL
        , cornerRadius 8.0
        , padding (Padding 2 2 2 2)
        , margin (Margin 16 16 16 16)
        , gravity LEFT
        , stroke ("1," <> Color.borderColorLight)
        ]
        [ editText
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , weight 1.0
            , textSize FontSize.a_16
            , padding (Padding 14 10 0 10)
            , color Color.black800
            , gravity LEFT
            , id (getNewIDWithTag "contactEditText")
            , background Color.white900
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , text ""
            , hint $ getString SEARCH_CONTACTS
            , pattern "[^\n]*,255"
            , onChange push $ ContactTextChanged
            ]
        , imageView
            [ height $ V 17
            , width $ V 17
            , accessibilityHint "Cancel Search : Button"
            , accessibility ENABLE
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_cancel"
            , gravity CENTER
            , margin (Margin 10 10 10 10)
            , onClick push $ const ContactListClearText
            ]
        ]
    , showContact listItemm push state
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding (Padding 16 16 16 0)
        , stroke $ "1," <> Color.grey900
        , alignParentBottom "true,-1"
        , margin (Margin 0 0 0 0)
        , adjustViewWithKeyboard "true"
        , alignParentBottom "true,-1"
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height if os == "IOS" then (V 68) else WRAP_CONTENT
            , gravity BOTTOM
            , padding $ PaddingBottom 16
            ]
            [ PrimaryButton.view getPushFn $ contactListPrimaryButtonConfig state
            ]
        ]
    ]
  where
  getPushFn =
    ( \action -> do
        void $ terminateLoader ""
        push $ PrimaryButtonAC action
    )

showContact :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> SelectContactsScreenState -> PrestoDOM (Effect Unit) w
showContact listitem push state =
  linearLayout
    [ width MATCH_PARENT
    , background Color.blue600
    , weight 1.0
    ]
    [ showContactData listitem push state
    ]

showContactData :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> SelectContactsScreenState -> PrestoDOM (Effect Unit) w
showContactData listItem push state =
  Keyed.linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ Tuple "contacts"
        $ PrestoList.list
            [ height MATCH_PARENT
            , scrollBarY false
            , width MATCH_PARENT
            , PrestoList.listItem listItem
            , background Color.white900
            , PrestoList.listDataV2 $ state.data.prestoListContacts
            ]
    ]

emptyTextView :: forall w. SelectContactsScreenState -> PrestoDOM (Effect Unit) w
emptyTextView state = textView []

horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ]
    []