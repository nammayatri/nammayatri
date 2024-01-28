module Screens.EmergencyContactsScreen.View where

import Animation (screenAnimation)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os, screenWidth, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (openUrlInApp)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (==), (<>), map, (/=), discard, (||), (&&),(-), (>), show)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..),scrollView, swipeRefreshLayout, hint, onScroll, scrollBarY, onScrollStateChange, alignParentBottom, pattern, onChange, id, editText, background, color, fontStyle, gravity, height, lineHeight, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, weight, width, imageView, imageUrl, cornerRadius, onClick, afterRender, visibility, stroke, relativeLayout, clickable, imageWithFallback, onRefresh, accessibility, accessibilityHint)
import Screens.EmergencyContactsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.Types (EmergencyContactsScreenState, ContactDetail, NewContacts)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App
import Helpers.Utils (storeCallBackContacts, contactPermission)
import Data.Array (take, (!!), mapWithIndex, null, length)
import Data.String as DS
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, Pattern(..))
import Components.PopUpModal as PopUpModal
import Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig
import PrestoDOM.List as PrestoList
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Data.Array as DA
import PrestoDOM.Types.Core (toPropValue)
import Data.String.Regex (match)
import Halogen.VDom.DOM.Prop (PropValue)
import Helpers.Utils (fetchImage, FetchImageFrom(..), setRefreshing)
import Screens.NammaSafetyFlow.Components.ContactsList as ContactsList
import Screens.NammaSafetyFlow.Components.HelperViews as HelperViews

screen :: EmergencyContactsScreenState -> PrestoList.ListItem -> Screen Action EmergencyContactsScreenState ScreenOutput
screen initialState listItemm =
  { initialState
  , view: view listItemm
  , name: "EmergencyContactsScreen"
  , globalEvents: [ globalOnScroll "EmergencyContactsScreen",
                    ( \push -> do
                        _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag") false
                        pure (pure unit)
                    )
                  ]
  , eval
  }

view :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
view listItemm push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , background Color.white900
        , padding if os == "IOS" then (Padding 0 safeMarginTop 0 (if safeMarginBottom == 0 && os == "IOS" then 16 else safeMarginBottom)) else (Padding 0 0 0 0)
        , gravity CENTER
        , afterRender
            ( \action -> do
                _ <- push action
                _ <- storeCallBackContacts push ContactsCallback
                _ <- push FetchContacts
                pure unit
            )
            (const NoAction)
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , accessibility if state.props.showInfoPopUp then DISABLE_DESCENDANT else DISABLE
              ]
              [ GenericHeader.view (push <<< ContactListGenericHeaderActionController) (genericHeaderConfig state)
              , linearLayout
                  [ height $ V 1
                  , width $ V (screenWidth unit)
                  , background Color.greySmoke
                  ]
                  []
              , linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , gravity CENTER
                  , padding $ PaddingHorizontal 16 16
                  , visibility if state.props.showContactList then GONE else VISIBLE
                  ]
                  [ emergencyContactsView push state
                  , HelperViews.recommendContactsToInstallView Language
                  , PrimaryButton.view (push <<< PrimaryButtonActionControll) (primaryButtonConfig state)
                  ]
              ]
          , if state.props.showContactList then (contactListView listItemm push state) else emptyTextView state
          ]
            <> if state.props.showInfoPopUp then [ removeContactPopUpView push state ] else [ emptyTextView state ]
        )

------------------------ EmptyTextView ---------------------------
emptyTextView :: forall w. EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emptyTextView state = textView []

------------------------ ContactsListView ---------------------------
contactListView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
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
    , showEmergencyContact listItemm push state
    , linearLayout
        [ height if os == "IOS" then (V 84) else WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding (Padding 16 16 16 24)
        , stroke $ "1," <> Color.grey900
        , alignParentBottom "true,-1"
        , margin (Margin 0 0 0 0)
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height if os == "IOS" then (V 52) else WRAP_CONTENT
            , gravity BOTTOM
            , alignParentBottom "true,-1"
            ]
            [ PrimaryButton.view (push <<< ContactListPrimaryButtonActionController) (contactListPrimaryButtonConfig state.data.contactsCount)
            ]
        ]
    ]

showEmergencyContact :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContact listitemm push config =
  swipeRefreshLayout
    ([ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blue600
    , weight 1.0
    , onRefresh push (const RefreshScreen)
    ] <> if os == "IOS" then [] else [id $ getNewIDWithTag "EmergencyContactTag"] )
    [ showEmergencyContactData listitemm push config
    ]

showEmergencyContactData :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContactData listItemm push state =
  Keyed.linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ]
  [ Tuple "contacts"
    $ PrestoList.list
    [ height MATCH_PARENT
    -- , scrollBarY false
    , width MATCH_PARENT
    , onScroll "contacts" "EmergencyContactsScreen" push (ContactListScroll)
    , onScrollStateChange push (ContactListScrollStateChanged)
    , PrestoList.listItem listItemm
    , background Color.white900
    , PrestoList.listDataV2  $ state.data.prestoListArrayItems
    ]
  ]

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) str == prefix

contactListPrimaryButtonConfig :: Int -> PrimaryButtonConfig.Config
contactListPrimaryButtonConfig count =
  let
    config' = PrimaryButtonConfig.config

    primaryButtonConfig' =
      config'
        { textConfig
          { text = if (count > 0) then (getString CONFIRM_EMERGENCY_CONTACTS) else (getString SELECT_CONTACTS)
          , accessibilityHint = (if (count > 0) then (getString CONFIRM_EMERGENCY_CONTACTS) else (getString SELECT_CONTACTS)) <> " : Button"
          , color = if (count > 0) then Color.yellow900 else Color.yellow800
          }
        , background = if (count > 0) then Color.black900 else Color.black600
        , isClickable = if (count > 0) then true else false
        }
  in
    primaryButtonConfig'


horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]

--------------------------------------------------- emergencyContactsView -----------------------------------------------------
emergencyContactsView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsView push state =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ emptyContactsView push state
    , emergencyContactsListView push state
    ]

--------------------------------------------------- emptyContactsView -----------------------------------------------------
emptyContactsView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emptyContactsView push state =
  linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , visibility if (null state.data.contactsList) then VISIBLE else GONE
    , weight 1.0
    ]
    [ imageView
        [ height $ V 150
        , width $ V 150
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_emergency_contact_empty"
        ]
    , textView
        $ [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , gravity CENTER
          , text (getString NO_EMERGENCY_CONTACTS_SET)
          , color Color.black900
          ]
        <> FontStyle.h2 LanguageStyle
    , textView
        $ [ height $ WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER
          , text (getString SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC)
          , color Color.black700
          , padding (Padding 16 10 16 10)
          ]
        <> FontStyle.paragraphText LanguageStyle
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
emergencyContactsListView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsListView push state =
  linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingVertical 12 10
    , visibility if null state.data.contactsList then GONE else VISIBLE
    , weight 1.0
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width if os == "IOS" then V (screenWidth unit - 20) else WRAP_CONTENT
          , text (getString SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC)
          , color Color.black700
          , padding $ PaddingVertical 10 10
          ]
        <> FontStyle.paragraphText LanguageStyle
    , ContactsList.view (push <<< ContactListAction) state.data.contactsList
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------


getNameInitials :: String -> (Array String)
getNameInitials fullName = (take 2 (split (Pattern " ") (fullName)))

getFirstChar :: String -> String
getFirstChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 0))

getLastChar :: String -> String
getLastChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 1))

removeContactPopUpView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
removeContactPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (removeContactPopUpModelConfig state) ]
