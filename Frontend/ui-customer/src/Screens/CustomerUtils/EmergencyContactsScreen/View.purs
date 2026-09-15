module Screens.EmergencyContactsScreen.View where

import Animation (screenAnimation)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os, screenWidth, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, discard, pure, unit, void, map, not, (<$>), ($), (&&), (-), (<), (<<<), (<>), (==), (>))
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollBarY, stroke, text, textSize, textView, visibility, weight, width, adjustViewWithKeyboard, scrollView, clickable)
import Screens.EmergencyContactsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (EmergencyContactsScreenState, NewContacts, CheckBoxSelectionConfig, Stage(..))
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (FetchImageFrom(FF_COMMON_ASSET), fetchImage, storeCallBackContacts)
import Data.Array (difference, length, null, take, (!!), mapWithIndex, concat, any)
import Data.String as DS
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Data.Foldable (foldl)
import Data.String.CodeUnits (toCharArray)
import Components.PopUpModal as PopUpModal
import Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig (genericHeaderConfig, primaryButtonConfig, removeContactPopUpModelConfig)
import Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig as CC
import PrestoDOM.List as PrestoList
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.Tuple (Tuple(..))
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Helpers.Utils (FetchImageFrom(..), fetchImage, storeCallBackContacts)
import Screens.NammaSafetyFlow.Components.ContactsList as ContactsList
import Screens.NammaSafetyFlow.Components.HelperViews as HelperViews
import Components.DropDownWithHeader as DropDownWithHeader
import Debug (spy)
import Storage (isLocalStageOn)
import Engineering.Helpers.Utils (terminateLoader)
import Mobility.Prelude (boolToInvisibility, boolToVisibility)
import JBridge as JB
import Effect.Uncurried (runEffectFn2)

screen :: EmergencyContactsScreenState -> PrestoList.ListItem -> Screen Action EmergencyContactsScreenState ScreenOutput
screen initialState listItemm =
  { initialState
  , view: view listItemm
  , name: "EmergencyContactsScreen"
  , globalEvents:
      [ globalOnScroll "EmergencyContactsScreen"
      , ( \push -> do
            pure (pure unit)
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "EmergencyContactsScreen action " action
        let
          _ = spy "EmergencyContactsScreen state " state
        eval action state
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
        , padding if os == "IOS" then (Padding 0 safeMarginTop 0 paddingBottom) else (Padding 0 0 0 0)
        , afterRender push $ const NoAction
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
                  , visibility if state.props.showContactList then GONE else VISIBLE
                  ]
                  [ emergencyContactsView push state
                  , HelperViews.recommendContactsToInstallView state.props.appName $ state.props.saveEmergencyContacts && not state.props.getDefaultContacts && length state.data.selectedContacts > 0
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , padding $ Padding 16 0 16 16
                      ]
                      [ PrimaryButton.view (push <<< PrimaryButtonActionControll) (primaryButtonConfig state) ]
                  ]
              ]
          , if state.props.showAddContactOptions then (addContactsOptionView push state) else emptyTextView state
          ]
            <> if state.props.showInfoPopUp then [ removeContactPopUpView push state ] else [ emptyTextView state ]
        )
  where
  marginBottom =
    if state.props.showContactList then
      0
    else if safeMarginBottom == 0 && os == "IOS" then
      16
    else
      safeMarginBottom

  paddingBottom = if state.props.isKeyBoardOpen then 0 else marginBottom

addContactsOptionView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
addContactsOptionView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black9000
    , onClick push $ const HideAddContactsOptions
    , gravity BOTTOM
    , adjustViewWithKeyboard "true"
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadii $ Corners 16.0 true true false false
        , orientation VERTICAL
        , clickable true
        , onClick push $ const NoAction
        , background Color.white900
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , padding $ Padding 16 16 0 0
              , text $ getString $ if state.props.addContactsManually then ADD_CONTACTS_MANUALLY else ADD_CONTACTS
              , color Color.black700
              ]
            <> FontStyle.subHeading2 TypoGraphy
        , addContactOptions push state
        , manualContactView push state
        ]
    ]

manualContactView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
manualContactView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 16.0
    , visibility $ boolToVisibility state.props.addContactsManually
    , orientation VERTICAL
    , background Color.white900
    ]
    [
      PrimaryEditText.view (push <<< TrustedNumberPET) (CC.primaryEditTextConfig state),
      PrimaryEditText.view (push <<< TrustedNamePET) (CC.primaryEditTextConfigName state),
      separator,
      relativeLayout[
        height WRAP_CONTENT,
        width MATCH_PARENT
      ][
      PrimaryButton.view (push <<< PrimaryButtonActionControll) (CC.primaryButtonConfigManualContact state)
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.transparentHigh
        , clickable true
        , visibility $ boolToVisibility $ not $ state.props.validManualContact && state.props.validManualName
        ][
          PrimaryButton.view (push <<< PrimaryButtonActionControll) (CC.primaryButtonConfigManualContactDummy state)
        ]
      ]
    ]

separator :: forall w. PrestoDOM (Effect Unit) w
separator =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.grey900
  , margin $ MarginTop 5
  ][]

addContactOptions :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
addContactOptions push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility $ not state.props.addContactsManually
    , padding $ Padding 16 16 16 16
    ]
    [ optionView AddContacts "ny_ic_user_info_details" CHOOSE_FROM_CONTACTS,
     separator,
     optionView AddContactsManually "ny_ic_new_add_contact" ADD_MANUALLY]
  where
  optionView action imageAsset componentText =
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      , padding $ Padding 16 16 16 16
      , onClick push $ const action
      ]
      [ imageView
          [ height $ V 24
          , width $ V 24
          , margin $ MarginRight 12
          , imageWithFallback $ fetchImage FF_COMMON_ASSET imageAsset
          ]
      , textView
          $ [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ getString componentText
            , weight 1.0
            , color Color.black800
            ]
          <> FontStyle.subHeading3 TypoGraphy
      , imageView
          [ height $ V 24
          , width $ V 24
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
          ]
      ]

------------------------ EmptyTextView ---------------------------
emptyTextView :: forall w. EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emptyTextView state = textView []

--------------------------------------------------- emergencyContactsView -----------------------------------------------------
emergencyContactsView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsView push state =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ emptyContactsView push state
    -- , emergencyContactsListView push state
    ]

--------------------------------------------------- emptyContactsView -----------------------------------------------------
emptyContactsView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emptyContactsView push state =
  linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ scrollView
        [ height MATCH_PARENT 
        , width MATCH_PARENT
        , scrollBarY false
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ imageView
                [ height $ V 200
                , width MATCH_PARENT
                , visibility $ boolToVisibility $ not $ state.props.saveEmergencyContacts && state.props.getDefaultContacts
                , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_share_explain"
                ]
            , explanationContentView push state
            , HelperViews.recommendContactsToInstallView state.props.appName $ state.props.saveEmergencyContacts && state.props.getDefaultContacts
            , checkBoxSelectionView push state
            ]
        ]
    ]

explanationContentView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
explanationContentView push state =
  let
    contactsLength = length state.data.selectedContacts
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , visibility $ boolToVisibility state.props.saveEmergencyContacts
      , orientation VERTICAL
      , gravity CENTER_HORIZONTAL
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ Padding 16 16 16 0
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text $ getString LIVE_RIDE_TRACKING
                , color Color.black900
                ]
              <> FontStyle.h3 TypoGraphy
          ]
      , imageViewComponent state "ny_ic_share_ride_rounded" true
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ Padding 16 16 16 0
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text $ getString LIVE_RIDE_TRACKING_DESC
                , color Color.black700
                ]
              <> FontStyle.body5 TypoGraphy
          ]
      , emergencyContactsListView push state
      , if contactsLength < 3 && contactsLength > 0 then addContactsButtonView push state else emptyTextView state
      ]

imageViewComponent :: forall w. EmergencyContactsScreenState -> String -> Boolean -> PrestoDOM (Effect Unit) w
imageViewComponent state imageUrl enableMargin =
  imageView
    [ height $ V 200
    , width MATCH_PARENT
    , margin $ if enableMargin then Margin 16 16 16 0 else MarginTop 16
    , visibility $ boolToVisibility $ state.props.saveEmergencyContacts && state.props.getDefaultContacts
    , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET imageUrl
    ]

addContactsButtonView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
addContactsButtonView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 16
    , margin $ Margin 16 16 16 16
    , cornerRadius 16.0
    , background Color.blue600
    , onClick push $ const ShowAddContactsOptions
    , gravity CENTER
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString ADD_CONTACTS
          , color Color.blue800
          ]
        <> FontStyle.body20 TypoGraphy
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
emergencyContactsListView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsListView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    $ mapWithIndex
        ( \index contact ->
            linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , padding $ Padding 16 16 16 16
              , margin $ Margin 16 16 16 0
              , cornerRadius 16.0
              , background Color.blue600
              , orientation VERTICAL
              ]
              [ emergencyContactListItem push state contact index
              , dropDownWithHeaderView push state contact
              ]
        )
        state.data.selectedContacts

dropDownWithHeaderView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> NewContacts -> PrestoDOM (Effect Unit) w
dropDownWithHeaderView push state contact =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , padding (Padding 0 0 0 0)
    ]
    [ DropDownWithHeader.view (push <<< DropDownWithHeaderAC) $ CC.dropDownWithHeaderConfig state contact --(CC.dropDownWithHeaderConfig state)
    ]

emergencyContactListItem :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
emergencyContactListItem push state contact index =
  let
    userColor = case index of
      0 -> Color.yellow900
      1 -> Color.blue800
      2 -> Color.yellow800
      _ -> Color.grey700
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ]
      [ linearLayout
          [ height $ V 36
          , width $ V 36
          , background userColor
          , cornerRadius 50.0
          , gravity CENTER
          , margin $ MarginRight 12
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getInitials $ getNameInitials contact.name
                , color Color.black
                ]
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation VERTICAL
          , weight 1.0
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text contact.name
                , color Color.black900
                ]
              <> FontStyle.subHeading3 TypoGraphy
          , textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text contact.number
                , color Color.black700
                ]
              <> FontStyle.body32 TypoGraphy
          ]
      , imageView
          [ height $ V 40
          , width $ V 40
          , visibility $ boolToVisibility $ not $ any isLocalStageOn [RideAccepted, RideStarted, ChatWithDriver]
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_delete_bin"
          , onClick push $ const $ RemoveButtonClicked contact
          ]
      ]

getInitials :: Array String -> String
getInitials wordsArray =
  let
    getInitial word = DS.take 1 word
    initialsArray = map (\word -> getInitial word) wordsArray
    uppercaseInitials = foldl (\acc w -> acc <> (DS.toUpper w) ) "" initialsArray
  in
    uppercaseInitials

--------------------------------------------------- checkBoxSelectionView -----------------------------------------------------
checkBoxSelectionView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
checkBoxSelectionView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $ boolToVisibility state.props.getDefaultContacts
    , orientation VERTICAL
    , padding (Padding 0 0 0 0)
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ Padding 16 16 16 0
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text $ getString APP_CALL_CHAT
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        , imageViewComponent state "ny_ic_chat_rounded" false
        , textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text $ getString TRUSTED_CONTACT_DESC
              , color Color.black700
              , margin $ MarginTop 6
              ]
            <> FontStyle.body5 TypoGraphy
        , textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text $ getString DEFAULT_CONTACT
              , margin $ MarginTop 16
              , color Color.black700
              ]
            <> FontStyle.body1 TypoGraphy
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 16 16 16
        , margin $ Margin 16 12 16 16
        , cornerRadius 16.0
        , background Color.blue600
        , orientation VERTICAL
        ]
        $ mapWithIndex (\index contact -> contactListViewCheckBox push state contact index) state.data.selectedContacts
    ]

contactListViewCheckBox :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactListViewCheckBox push state contact index =
  let
    isDefault = contact.priority == 0 --contact.number == state.data.defaultSelectedContact.number

    strokeColor = if isDefault then Color.black800 else Color.black500

    userColor = case index of
      0 -> Color.yellow900
      1 -> Color.blue800
      2 -> Color.yellow800
      _ -> Color.grey700
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginVertical 12 12
      , orientation HORIZONTAL
      , onClick push $ const $ DefaultContactSelected contact
      , gravity CENTER_VERTICAL
      ]
      [ linearLayout
          [ height $ V 36
          , width $ V 36
          , background userColor
          , cornerRadius 50.0
          , gravity CENTER
          , margin $ MarginRight 12
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getInitials $ getNameInitials contact.name
                , color Color.black
                ]
          ]
      , textView
          $ [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text contact.name
            , color Color.black900
            , weight 1.0
            ]
          <> FontStyle.body32 TypoGraphy
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , padding $ Padding 4 4 4 4
          , stroke $ "1," <> Color.black800
          , cornerRadius 50.0
          ]
          [ linearLayout
              [ height $ V 12
              , width $ V 12
              , cornerRadius 50.0
              , visibility $ boolToInvisibility isDefault
              , background Color.black800
              ]
              []
          ]
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
