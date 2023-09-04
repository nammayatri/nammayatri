module Components.NewContact.View where

import Prelude
import Components.NewContact.Controller (Action(..))
import Effect (Effect)
import Styles.Colors as Color
import Data.Array (length, (!!))
import Font.Style as FontStyle
import Font.Size as FontSize
import Language.Types (STR(..))
import Language.Strings (getString)
import Helpers.Utils (parseFloat)
import Data.Int (toNumber)
import Storage (getValueToLocalStore, KeyStore(..))
import PrestoDOM (Length(..) , Margin(..), Orientation(..), Padding(..) , Visibility(..), Gravity(..), PrestoDOM,Gradient(..), Accessiblity(..), gradient, weight, cornerRadius, height, width, margin, padding, linearLayout, gravity, orientation, fontStyle, textSize, textView, text, background, clickable, color, imageView, imageUrl, ellipsize, maxLines, lineHeight, visibility, textFromHtml, layoutGravity, imageWithFallback, relativeLayout, accessibilityHint, accessibility, singleLine)
import Common.Types.App
import Data.Maybe
import Screens.Types(NewContacts)
import Engineering.Helpers.Commons (os)
import PrestoDOM.List as PrestoList
import Screens.EmergencyContactsScreen.Controller (Action(..)) as Screen
import PrestoDOM.Types.Core (toPropValue)
import Data.Maybe (Maybe(..), fromMaybe)

view :: forall w . (Screen.Action  -> Effect Unit) -> NewContacts -> PrestoDOM (Effect Unit) w
view push item =
    linearLayout
    [ height  MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    , background Color.red
    , PrestoList.backgroundHolder "contactBackgroundColor"
    , PrestoList.onClickHolder push  $ Screen.NewContactActionController <<< ContactSelected
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ]
        [ imageView
            [ height $ V 40
            , width $ V 40
            , margin (Margin 16 16 0 16)
            , imageWithFallback "ny_ic_user,https://assets.juspay.in/nammayatri/images/user/ny_ic_user.png"
            , cornerRadius if os == "IOS" then 16.0 else 20.0
            , gravity CENTER
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin (Margin 12 16 0 16)
            , gravity CENTER_VERTICAL
            , weight 1.0
            ]
            [ textView
                [ PrestoList.textHolder "name"
                , color Color.black900
                , textSize FontSize.a_14
                , margin (Margin 0 0 4 0)
                , lineHeight "16"
                , maxLines 1 
                , singleLine true
                , ellipsize true
                , fontStyle $ FontStyle.medium LanguageStyle
                ]
            , textView
                [ PrestoList.textHolder "number"
                , color Color.black700
                , textSize FontSize.a_14
                , margin (Margin 0 0 0 0)
                , fontStyle $ FontStyle.regular LanguageStyle
                ]
            ]
        , relativeLayout
            [ height $ V 24
            , width $ V 24
            , margin (Margin 0 23 16 23)
            , padding (Padding 0 0 0 0)
            , gravity CENTER
            ][ imageView
                [ height $ V 24 
                , width $ V 24 
                , accessibility ENABLE
                , PrestoList.imageUrlHolder "isSelectImage"
                , accessibilityHint "Selected"
                , PrestoList.visibilityHolder "visibilitySelectedImage"
                ] 
            , imageView
                [ height $ V 17
                , width $ V 17
                , accessibility ENABLE
                , accessibilityHint "Un Selected"
                , PrestoList.imageUrlHolder "isSelectImage"
                , PrestoList.visibilityHolder "visibilityUnSelectedImage"
                ]
            ]
        ]
    , horizontalLine
    ]

horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]
