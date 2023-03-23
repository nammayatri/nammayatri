module Components.ChooseVehicle.View where

import Components.ChooseVehicle.Controller (Action(..), Config)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit,($),(<>),const,pure,unit)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius,  fontStyle,  gravity, height, imageView,  lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, clickable, imageWithFallback , imageUrl)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Font.Size as FontSize
import Common.Types.App

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM ( Effect Unit ) w
view push state = 
  linearLayout
  [ height state.height
  , width MATCH_PARENT
  , background if state.onselect then state.backgroundColour else Color.white900
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , cornerRadius  if state.onselect then 6.0 else 0.0
  , stroke $ if state.onselect then state.stroke else  "0,"<>Color.greySmoke
  , margin state.margin
  , clickable state.clickable
  , onClick push $ const OnClick
  ][
    imageView
      [ imageWithFallback state.primaryimage.imageUrl
      , height state.primaryimage.height
      , width  state.primaryimage.width
      , padding state.primaryimage.padding
      , margin state.primaryimage.margin
      ]  
    , textViewlayout push state 
    , linearLayout 
      [ height WRAP_CONTENT
      , weight 1.0
      ][]
    , priceDetails push state
    ]

textViewlayout :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM ( Effect Unit) w 
textViewlayout push state = 
 linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation VERTICAL
  ][ 
    textView 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text state.mainText.text
        , color state.mainText.colour
        , textSize state.mainText.textSize
        , fontStyle $ FontStyle.bold LanguageStyle
        , lineHeight "20"
        ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][
        textView 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , lineHeight "20"
        , text state.subText1.text
        , color state.subText1.colour
        , textSize state.subText1.textSize
        , fontStyle $ FontStyle.regular LanguageStyle
        ]
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , cornerRadius 2.0
        , background Color.black600
        , margin (Margin 6 6 6 0)
        , gravity CENTER_VERTICAL
        ]
        []
    , textView 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , lineHeight "20"
        , text state.subText2.text
        , color state.subText2.colour
        , textSize state.subText2.textSize
        , fontStyle $ FontStyle.regular LanguageStyle
        ]
      ]
  ]

priceDetails:: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM ( Effect Unit) w 
priceDetails push state = 
 linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  ][
    textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ "₹" <> state.cornerText.text
        , textSize state.cornerText.textSize
        , lineHeight "20"
        , color state.cornerText.colour
        , fontStyle $ FontStyle.bold LanguageStyle
        , visibility state.cornerText.visibility
        ]
    , linearLayout
        [ height state.imageLayoutHeight
        , width  state.imageLayoutWidth
        , stroke $ fromMaybe ( "0,"<>Color.greySmoke) state.imageStroke
        , clickable state.secondaryImage.clickable
        , onClick push (const $ OnImageClick)
        , padding state.secondaryImage.padding
        , margin state.secondaryImage.margin
        ][  imageView
          [ imageWithFallback state.secondaryImage.imageUrl
          , height state.secondaryImage.height
          , width state.secondaryImage.width
          , visibility state.secondaryImage.visibility
          ]  
        ]
  ]