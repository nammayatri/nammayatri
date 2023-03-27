module Screens.EmergencyContactsScreen.View where

import Animation (screenAnimation)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (openUrlInApp, loaderText)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (==), (<>), map, (/=), discard, (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, lineHeight, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, weight, width, imageView, imageUrl, cornerRadius, onClick, afterRender, visibility, stroke, relativeLayout, clickable, imageWithFallback)
import Screens.EmergencyContactsScreen.Controller (Action(..), ScreenOutput, eval, contactColorsList)
import Screens.Types (EmergencyContactsScreenState, ContactDetail, NewContacts)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Styles.Colors as Color
import Debug.Trace (spy)
import Common.Types.App
import Helpers.Utils (storeCallBackContacts, contactPermission)
import Components.ContactList as ContactList
import Data.Array (take, (!!), mapWithIndex, null, length)
import Data.String as DS
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, Pattern(..))
import Components.PopUpModal as PopUpModal
import Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig

screen :: EmergencyContactsScreenState -> Screen Action EmergencyContactsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "EmergencyContactsScreen"
  , globalEvents: []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , background Color.white900
        , padding if os == "IOS" then (Padding 0 safeMarginTop 0 safeMarginBottom) else (Padding 0 0 0 0)
        , gravity CENTER
        , afterRender
            ( \action -> do
                _ <- push action
                _ <- storeCallBackContacts push ContactsCallback
                if ((getValueToLocalStore CONTACTS == "__failed") || (getValueToLocalStore CONTACTS == "(null)")) then do
                  _ <- push FetchContacts
                  pure unit
                else do
                  pure unit
                _ <- push CheckingContactList
                pure unit
            )
            (const NoAction)
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
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
                , padding (Padding 16 0 16 0)
                , visibility if state.props.showContactList then GONE else VISIBLE
                ]
                [ emergencyContactsView push state
                , PrimaryButton.view (push <<< PrimaryButtonActionControll) (primaryButtonConfig state)
                ]
            ]
        , if state.props.showContactList then (contactListView push state) else emptyTextView state
        , if (state.props.showInfoPopUp == true) then removeContactPopUpView push state else emptyTextView state
        ]

------------------------ EmptyTextView ---------------------------
emptyTextView :: forall w. EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emptyTextView state = textView []

------------------------ ContactsListView ---------------------------
contactListView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
contactListView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black900
    ]
    [ ContactList.view (push <<< ContactListAction)
        { contactsData: state.data.contactsNewList
        , count: state.data.contactsCount
        , contactList: state.data.contactsList
        , editedText: state.data.editedText
        }
    ]

--------------------------------------------------- emergencyContactsView -----------------------------------------------------
emergencyContactsView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsView push state =
  linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
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
        , imageWithFallback "ny_ic_emergency_contact_empty,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contact_empty.png"
        ]
    , textView
        [ height $ WRAP_CONTENT
        , width $ WRAP_CONTENT
        , gravity CENTER
        , text (getString NO_EMERGENCY_CONTACTS_SET)
        , color Color.black900
        , textSize 18
        , fontStyle $ FontStyle.bold LanguageStyle
        ]
    , textView
        [ height $ WRAP_CONTENT
        , width if os == "IOS" then (V 360) else (WRAP_CONTENT)
        , gravity CENTER
        , text (getString EMERGENCY_CONTACTS_SCREEN_DESCRIPTION)
        , color Color.black700
        , textSize 14
        , fontStyle $ FontStyle.regular LanguageStyle
        , padding (Padding 0 10 0 10)
        ]
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
emergencyContactsListView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsListView push state =
  linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 10 0 10)
    , visibility if (null state.data.contactsList) then GONE else VISIBLE
    , weight 1.0
    ]
    [ textView
        [ height $ WRAP_CONTENT
        , width if os == "IOS" then (V 360) else (WRAP_CONTENT)
        , text (getString EMERGENCY_CONTACTS_SCREEN_DESCRIPTION)
        , color Color.black700
        , textSize 14
        , fontStyle $ FontStyle.regular LanguageStyle
        , padding (Padding 0 10 0 10)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        (mapWithIndex (\index item -> contactCardView push state item index) state.data.contactsList)
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
contactCardView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactCardView push state contact index =
  linearLayout
    [ height $ WRAP_CONTENT
    , width if os == "IOS" then (V 360) else (MATCH_PARENT)
    , padding $ Padding 18 18 18 18
    , margin $ Margin 0 5 0 5
    , cornerRadius 8.0
    , stroke ("1," <> Color.grey900)
    ]
    [ linearLayout
        [ height $ V 24
        , width $ V 24
        , background (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0))
        , cornerRadius if os == "IOS" then 12.0 else 20.0
        , gravity CENTER
        , margin (MarginRight 10)
        ]
        [ textView
            [ text (DS.toUpper ((<>) (getFirstChar contact.name) (getLastChar contact.name)))
            , color (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1))
            , textSize FontSize.a_12
            ]
        ]
    , textView
        [ height $ WRAP_CONTENT
        , width $ WRAP_CONTENT
        , weight 1.0
        , text contact.name
        , color Color.black800
        , textSize 16
        , fontStyle $ FontStyle.semiBold LanguageStyle
        ]
    , textView
        [ height $ WRAP_CONTENT
        , width $ WRAP_CONTENT
        , text (getString REMOVE)
        , color Color.blue900
        , textSize 14
        , onClick push (const (RemoveButtonClicked contact))
        ]
    ]

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
