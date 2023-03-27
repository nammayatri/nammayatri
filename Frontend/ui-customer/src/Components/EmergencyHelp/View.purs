{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.EmergencyHelp.View where
import Components.EmergencyHelp.Controller (Action(..), EmergencyHelpModelState, contactColorsList)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, ($), (/=), (<>), (==), pure, (<<<), (-), discard, unit, bind)
import Data.Array (take, (!!), drop, head, mapWithIndex, null)
import Data.String as DS
import Data.Array as DA
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, relativeLayout, frameLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, textFromHtml, onBackPressed, scrollView, imageWithFallback, stroke, afterRender)
import Styles.Colors as Color
import Data.String (split, Pattern(..), indexOf, length)
import Components.PopUpModal.Controller as PopUpModalConfig
import Components.PopUpModal.View as PopUpModal
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.GenericHeader.View as GenericHeader
import Common.Types.App
import PrestoDOM.Types.DomAttributes (Corners(..))
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os)
import Helpers.Utils (isPreviousVersion, getPreviousVersion)
import Storage (getValueToLocalStore, KeyStore(..))

view :: forall w .  (Action  -> Effect Unit) -> EmergencyHelpModelState  -> PrestoDOM (Effect Unit) w
view push state = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , afterRender (\action -> do
                          _ <- push action
                          push StoreContacts 
                          pure unit
                        ) (const NoAction)
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , padding if os == "IOS" then (PaddingVertical safeMarginTop safeMarginBottom) else (PaddingBottom 10)
      ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
         , scrollView 
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         ][ linearLayout 
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , orientation VERTICAL
             ][ emergencyHelpLogoContainer state
               , showEmergencyContact state push
               ,  linearLayout 
                   [ height $ V 1
                   , width MATCH_PARENT
                   , background Color.grey800
                   , margin $ Margin 16 8 16 0
                   ][]
               , supportButtonView state push 
               , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , margin $ MarginBottom 100
                  ][]
              ]
            ]
          ]
          , popUpViewCustomerSupport state push
          , popUpViewCallPolice state push
          , popUpViewCallEmergencyContact state push
          , popUpViewCallSuccessful state push
       ]
    
supportButtonView :: forall w . EmergencyHelpModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
supportButtonView state push =  
  linearLayout
   [ height WRAP_CONTENT
   , width MATCH_PARENT
   , orientation VERTICAL
   , clickable true
   ](mapWithIndex (\index item ->
      supportButtonViewContent state push item index
      ) (supportList state))

supportButtonViewContent :: forall w . EmergencyHelpModelState -> (Action  -> Effect Unit) -> CardData -> Int -> PrestoDOM (Effect Unit) w
supportButtonViewContent state push item index =  linearLayout
     [height WRAP_CONTENT
     , width MATCH_PARENT
     , orientation VERTICAL
     ][ linearLayout
        [ height WRAP_CONTENT
         , width MATCH_PARENT
         ,  margin $ Margin 16 12 16 0
         , clickable true
         , onClick push $ const item.action
        ][ linearLayout 
           [ height WRAP_CONTENT
           , width WRAP_CONTENT
           , orientation VERTICAL
           ][ textView
              [ text item.title
              , color Color.black800
              , textSize FontSize.a_14
              , fontStyle $ FontStyle.medium LanguageStyle
              , lineHeight "20"
              , gravity CENTER
              ]
            , textView
              [ text item.secondaryTitle
              , margin $ MarginTop 4
              , color Color.black700 
              , textSize FontSize.a_12
              , lineHeight "16"
              , fontStyle $ FontStyle.regular LanguageStyle
              ]
            ]
          , linearLayout
            [ width MATCH_PARENT
            , gravity RIGHT
            ][  imageView
                [ height $ V 12
                , width $ V 12
                , imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
                , margin $ MarginTop 4
                , color Color.black900
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
              ]
         ] 
         ,  linearLayout 
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.grey800
            , visibility if index == (DA.length (supportList state)) - 1 then GONE else VISIBLE
            , margin (Margin 16 20 16 0)
            ][]
     ]

emergencyHelpLogoContainer :: forall w . EmergencyHelpModelState -> PrestoDOM (Effect Unit) w
emergencyHelpLogoContainer state = 
   linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingVertical 28 32
    , background Color.blue600
    ][  imageView
        [ height $ V 128
        , width MATCH_PARENT
        , imageWithFallback "ny_ic_emergency_shield,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_shield.png"
        , margin (MarginBottom 24)
        ]
      , textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text $ getString DO_YOU_NEED_EMERGENCY_HELP
        , gravity CENTER
        , color Color.black800
        , fontStyle $ FontStyle.bold LanguageStyle
        , textSize FontSize.a_18
        , lineHeight "20"
        ]
      ]

callPoliceConfig :: EmergencyHelpModelState -> PopUpModalConfig.Config
callPoliceConfig state  = 
  let 
  config' = PopUpModalConfig.config 
  popUpConfig' = config' {
    primaryText { 
      text = getString DAIL_100
    , margin = (Margin 40 23 40 12)
    , fontStyle = FontStyle.semiBold LanguageStyle }
    , option1 {
      text = getString CANCEL_
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 16 16) }
    , option2 {
      text = getString CALL_POLICE
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 12 0)  }
    , backgroundClickable = true
    , secondaryText {
      text = getString YOU_ARE_ABOUT_TO_CALL_POLICE
    , margin = (Margin 40 0 40 32) }
    , gravity = CENTER
    , margin = (Margin 16 0 16 0)
    , cornerRadius = (Corners 15.0 true true true true)
  }
  in popUpConfig'

contactSupportConfig :: EmergencyHelpModelState -> PopUpModalConfig.Config
contactSupportConfig state  = 
  let 
  config' = PopUpModalConfig.config 
  popUpConfig' = config' {
    primaryText { 
      text = (<>) (getString CONTACT_SUPPORT) "?"
    , margin = (Margin 40 23 40 12)
    , fontStyle = FontStyle.semiBold LanguageStyle }
    , option1 {
      text = getString CANCEL_
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 16 16) }
    , option2 {
      text = getString CALL_SUPPORT
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 12 0) }
    , backgroundClickable = true
    , secondaryText {
      text = getString YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT
    , margin = (Margin 40 0 40 32) }
    , gravity = CENTER
    , margin = (MarginHorizontal 16 16)
    , cornerRadius = (Corners 20.0 true true true true)
  }
  in popUpConfig'

callEmergencyContactConfig :: EmergencyHelpModelState -> PopUpModalConfig.Config
callEmergencyContactConfig state  = 
  let 
  config' = PopUpModalConfig.config 
  popUpConfig' = config' {
    primaryText { 
      text = (<>) (getString CALL_EMERGENCY_CONTACTS) "?"
    , margin = (Margin 40 23 40 12)
    , fontStyle = FontStyle.semiBold LanguageStyle }
    , option1 {
      text = getString CANCEL_
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 16 16) }
    , option2 {
      text = "Place Call"
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 12 0) }
    , backgroundClickable = true
    , secondaryText {
      visibility = GONE
    }
    , contactViewConfig {
       visibility = VISIBLE,
       fullName = state.currentlySelectedContact.name,
       nameInitials = (DS.toUpper((<>) (getFirstChar  state.currentlySelectedContact.name) (getLastChar  state.currentlySelectedContact.name) ))
    }
    , gravity = CENTER
    , margin = (MarginHorizontal 16 16)
    , cornerRadius = (Corners 20.0 true true true true)
  }
  in popUpConfig'

callSuccessfulConfig :: EmergencyHelpModelState -> PopUpModalConfig.Config
callSuccessfulConfig state  = 
  let 
  config' = PopUpModalConfig.config 
  popUpConfig' = config' {
    primaryText { 
      text = (<>) "Was Your Call Successful" "?"
    , margin = (Margin 40 23 40 46)
    , fontStyle = FontStyle.semiBold LanguageStyle }
    , option1 {
      text = "No"
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 16 16) }
    , option2 {
      text = "Yes"
    , fontSize = FontSize.a_16
    , margin = (MarginHorizontal 12 0) }
    , backgroundClickable = true
    , secondaryText {
      visibility = GONE }
    , gravity = CENTER
    , margin = (MarginHorizontal 16 16)
    , cornerRadius = (Corners 20.0 true true true true)
  }
  in popUpConfig'

popUpViewCustomerSupport :: forall w. EmergencyHelpModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
popUpViewCustomerSupport state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.showContactSupportPopUp then VISIBLE else GONE
  ][PopUpModal.view (push <<< ContactSupport) (contactSupportConfig state)]

popUpViewCallPolice :: forall w. EmergencyHelpModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
popUpViewCallPolice state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.showCallPolicePopUp then VISIBLE else GONE
  ][PopUpModal.view (push <<< CallPolice) (callPoliceConfig state)]

popUpViewCallEmergencyContact :: forall w. EmergencyHelpModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
popUpViewCallEmergencyContact state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.showCallContactPopUp then VISIBLE else GONE
  ][PopUpModal.view (push <<< CallEmergencyContact) (callEmergencyContactConfig state)]


popUpViewCallSuccessful :: forall w. EmergencyHelpModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
popUpViewCallSuccessful state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.showCallSuccessfulPopUp then VISIBLE else GONE
  ][PopUpModal.view (push <<< CallSuccessful) (callSuccessfulConfig state)]

showEmergencyContact :: forall w . EmergencyHelpModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
showEmergencyContact state push = 
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 20 16 12
    , visibility if(isPreviousVersion (getValueToLocalStore VERSION_NAME) (if os == "IOS" then "1.2.5" else "1.2.1")) then GONE else VISIBLE
    ][  linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ][  textView
            [ text $ getString CALL_EMERGENCY_CONTACTS
            , fontStyle $ FontStyle.medium LanguageStyle
            , color Color.black800
            , textSize FontSize.a_14
            , lineHeight "20"
            ]
          , linearLayout
            [ width MATCH_PARENT
            , gravity RIGHT
            , visibility if (DA.null state.emergencyContactData) then VISIBLE else GONE
            ][  imageView
                [ height $ V 12
                , width $ V 12
                , imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
                , margin $ MarginTop 6
                , color Color.black900
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
             ] 
          ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]( if (DA.null state.emergencyContactData) then [noContactsAvailableView push]
              else allContactsView state push)
    ]

noContactsAvailableView :: forall w .(Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
noContactsAvailableView push = 
  textView
  [ text $ getString YOU_WILL_BE_ASKED_TO_SELECT_CONTACTS
  , margin $ MarginTop 4 
  , color Color.black700 
  , textSize FontSize.a_12
  , lineHeight "16"
  , fontStyle $ FontStyle.regular LanguageStyle
  , onClick push $ const AddedEmergencyContacts
  ]
 

allContactsView :: forall w . EmergencyHelpModelState -> (Action  -> Effect Unit)  -> Array(PrestoDOM (Effect Unit) w)
allContactsView state push = 
  (mapWithIndex (\ index item -> 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 0 12 8 0
    , stroke ("1," <> Color.borderColorLight)
    , padding $ Padding 23 16 23 16
    , cornerRadius 8.0
    ][  linearLayout
        [ height WRAP_CONTENT
        , width  MATCH_PARENT
        , gravity CENTER
        ][  linearLayout
            [ height $ V 24
            , width $ V 24
            , background (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0))
            , cornerRadius 12.0
            , gravity CENTER
            ][  textView
                [text (DS.toUpper((<>) (getFirstChar item.name) (getLastChar item.name) ))
                , color (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1))
                , textSize FontSize.a_12
                ]
              ]
              ,  linearLayout 
                 [ height  WRAP_CONTENT
                 , width  WRAP_CONTENT
                 , padding (PaddingLeft 8)
                 ][  textView
                   [text (item.name)
                   , color Color.black800
                   , textSize FontSize.a_16
                   , lineHeight "20"
                   , fontStyle $ FontStyle.semiBold LanguageStyle
                   ]
                  ]
              , linearLayout
                [ height  WRAP_CONTENT
                 , width  MATCH_PARENT
                 , gravity RIGHT
                 , onClick push $ const $ CallContactPopUp item
                ][ textView
                   [ text $ "Call"
                   , color Color.green900
                   , textSize FontSize.a_14
                   , lineHeight "18"
                   , fontStyle $ FontStyle.regular LanguageStyle
                   , margin $ MarginLeft 5
                   ]
                 ]
          ]
       ]) state.emergencyContactData)

genericHeaderConfig :: EmergencyHelpModelState -> GenericHeaderConfig.Config
genericHeaderConfig state = let 
  config = GenericHeaderConfig.config
  genericHeaderConfig' = config 
    { height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 12 12 12) } 
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = getString EMERGENCY_HELP
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.semiBold LanguageStyle }
    , suffixImageConfig {
        visibility = GONE }
    }
  in genericHeaderConfig'

getNameInitials :: String -> (Array String)
getNameInitials fullName =  (take 2 (split (Pattern " ") (fullName)))

getFirstChar :: String ->  String
getFirstChar name =  DS.take 1 (fromMaybe "" ( (getNameInitials name) !! 0))

getLastChar :: String -> String
getLastChar name =  DS.take 1 ( fromMaybe "" ( (getNameInitials name) !! 1) )

type CardData =  { action :: Action
  , title :: String
  , secondaryTitle :: String
 }

supportList :: EmergencyHelpModelState -> Array (CardData)
supportList state = [
  { action :  ContactSupportPopup
  , title : getString CONTACT_SUPPORT
  , secondaryTitle : getString ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION },
  { action : CallPolicePopup
  , title : getString CALL_POLICE
  , secondaryTitle : getString ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION }
]