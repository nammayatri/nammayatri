module Components.ContactList.View where

import Components.ContactList.Controller (Action(..), ContactsState, aToz, sortedContactData, searchContacts)
import Components.MenuButton.Controller as MenuButtonConfig
import Components.MenuButton.View as MenuButton
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.GenericHeader.View as GenericHeader
import Helpers.Utils (storeCallBackContacts, contactPermission)
import Prelude
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Gradient(..), background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, relativeLayout, frameLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, textFromHtml, onBackPressed, scrollView, afterRender, stroke, alignParentBottom, gradient, editText, id, hint, pattern, onChange, imageWithFallback)
import Styles.Colors as Color
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PrimaryButton.View as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextConfig
import Components.PrimaryEditText.View as PrimaryEditText
import PrestoDOM.Animation as PrestoAnim
import Animation (fadeIn, fadeOut)
import Font.Size as FontSize
import Font.Style as FontStyle
import Data.Show (show)
import Data.String as DS
import Data.Array (sortBy, mapWithIndex, filter, (!!), length)
import Data.Ord (compare)
import Common.Types.App
import Data.Map (Map, empty, insert)
import Data.NonEmpty (head)
import JBridge (getBtnLoader)
import Screens.Types (NewContacts, Contacts)
import Data.Maybe (Maybe(..), fromMaybe)
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os, getNewIDWithTag)

view :: forall w. (Action -> Effect Unit) -> ContactsState -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        ]
        [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig config)
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
            , imageUrl "ic_cancel"
            , gravity RIGHT
            , margin (Margin 0 10 18 10)
            , onClick push $ const ClearText
            ]
        ]
    , showEmergencyContact push config
    , linearLayout
        [ height if os == "IOS" then (V 84) else WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding (Padding 16 16 16 16)
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
            [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig config.count)
            ]
        ]
    ]

genericHeaderConfig :: ContactsState -> GenericHeaderConfig.Config
genericHeaderConfig state =
  let
    config = GenericHeaderConfig.config

    genericHeaderConfig' =
      config
        { height = WRAP_CONTENT
        , prefixImageConfig
          { height = V 25
          , width = V 25
          , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
          , margin = (Margin 12 12 12 12)
          }
        , padding = (Padding 0 5 0 5)
        , textConfig
          { text = ((show (state.count)) <> "/" <> show (3 - (length state.contactList)) <> " " <> (getString CONTACTS_SELECTED))
          , textSize = FontSize.a_18
          , color = Color.darkDescriptionText
          , fontStyle = FontStyle.semiBold LanguageStyle
          }
        , suffixImageConfig
          { visibility = GONE
          }
        }
  in
    genericHeaderConfig'

horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]

showEmergencyContact :: forall w. (Action -> Effect Unit) -> ContactsState -> PrestoDOM (Effect Unit) w
showEmergencyContact push config =
  scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blue600
    , weight 1.0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.blue600
        , margin (Margin 0 0 0 0)
        -- , visibility if (config.contactsData /= []) then VISIBLE else GONE
        ]
        (
          
          -- if (config.contactsData == []) then 
          --   []
          -- else showEmergencyContactData push config
          showEmergencyContactData push config
          
        )
    ]

showEmergencyContactData :: forall w. (Action -> Effect Unit) -> ContactsState -> Array (PrestoDOM (Effect Unit) w)
showEmergencyContactData push config =
  ( mapWithIndex
      ( \ind1 letter ->
          ( linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              [ linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation HORIZONTAL
                  , padding (Padding 16 12 0 12)
                  , background Color.blue600
                  , visibility if (((filter (\contact -> DS.take 1 contact.name == letter) config.contactsData) /= []) && (config.editedText == "")) then VISIBLE else GONE
                  ]
                  [ textView
                      [ text (letter)
                      , textSize FontSize.a_12
                      , color Color.black700
                      ]
                  ]
              , linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ]
                  ( mapWithIndex
                      ( \ind2 item ->
                          linearLayout
                            [ height MATCH_PARENT
                            , width MATCH_PARENT
                            , orientation VERTICAL
                            , weight 1.0
                            , if item.isSelected then background Color.grey900 else background Color.white900
                            , onClick push $ (const (ContactSelected item))
                            ]
                            [ linearLayout
                                [ height WRAP_CONTENT
                                , width MATCH_PARENT
                                , orientation HORIZONTAL
                                ]
                                [ linearLayout
                                    [ height $ V 32
                                    , width $ V 32
                                    , margin (Margin 16 19 0 19)
                                    , gradient (Linear 90.0 (fromMaybe [] (Color.linearGradient !! ((ind1 + ind2) `mod` 6))))
                                    , cornerRadius if os == "IOS" then 16.0 else 20.0
                                    , gravity CENTER
                                    ]
                                    []
                                , linearLayout
                                    [ height WRAP_CONTENT
                                    , width MATCH_PARENT
                                    , orientation VERTICAL
                                    , margin (Margin 12 16 0 16)
                                    , gravity CENTER_VERTICAL
                                    , weight 1.0
                                    ]
                                    [ textView
                                        [ text (item.name)
                                        , color Color.black900
                                        , textSize FontSize.a_14
                                        , margin (Margin 0 0 4 0)
                                        , lineHeight "16"
                                        , fontStyle $ FontStyle.medium LanguageStyle
                                        ]
                                    , textView
                                        [ text ("+91 " <> (item.number))
                                        , color Color.black700
                                        , textSize FontSize.a_14
                                        , margin (Margin 0 0 0 0)
                                        , fontStyle $ FontStyle.regular LanguageStyle
                                        ]
                                    ]
                                , linearLayout
                                    [ height $ V 24
                                    , width $ V 24
                                    , margin (Margin 0 23 16 23)
                                    , padding (Padding 0 0 0 0)
                                    , gravity CENTER
                                    ]
                                    [ imageView
                                        [ height if item.isSelected then V 24 else V 17
                                        , width if item.isSelected then V 24 else V 17
                                        , imageWithFallback if item.isSelected then "ny_ic_selected_icon,https://assets.juspay.in/nammayatri/images/user/ny_ic_selected_icon" else "ny_ic_outer_circle,https://assets.juspay.in/nammayatri/images/user/ny_ic_outer_circle"
                                        ]
                                    ]
                                ]
                            , horizontalLine
                            ]
                      )
                      (searchContacts config.editedText (sortedContactData letter config.contactsData))
                  )
              ]
          )
      )
      aToz
  )


primaryButtonConfig :: Int -> PrimaryButtonConfig.Config
primaryButtonConfig count =
  let
    config' = PrimaryButtonConfig.config

    primaryButtonConfig' =
      config'
        { textConfig
          { text = if (count > 0) then (getString CONFIRM_EMERGENCY_CONTACTS) else (getString SELECT_CONTACTS)
          , color = if (count > 0) then Color.yellow900 else Color.yellow800
          }
        , background = if (count > 0) then Color.black900 else Color.black600
        , isClickable = if (count > 0) then true else false
        }
  in
    primaryButtonConfig'
