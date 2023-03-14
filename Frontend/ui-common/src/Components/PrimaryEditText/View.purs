module Components.PrimaryEditText.View where

import Prelude (Unit, ($), (<>), (==), (&&), not)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons (os)
import Components.PrimaryEditText.Controller (Action(..), Config)
import PrestoDOM (InputType(..),Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), alpha, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, hintColor, imageUrl, imageView, lineHeight, letterSpacing, linearLayout, margin, onChange, orientation, padding, pattern, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, inputType, multiLineEditText, maxLines, inputTypeI)

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin config.margin
    ][  topLabelView config
      , editTextLayout push config
      , errorLabelLayout config
      ]

topLabelView :: forall w . Config -> PrestoDOM (Effect Unit) w
topLabelView config = 
  textView
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , textSize config.topLabel.textSize
    , text config.topLabel.text
    , color config.topLabel.color
    , fontStyle config.topLabel.fontStyle
    , gravity config.topLabel.gravity
    , lineHeight "28"
    , singleLine true
    , margin config.topLabel.margin
    , alpha config.topLabel.alpha
    ]  


editTextLayout :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
editTextLayout push config = 
  linearLayout
    [ height config.height
    , width config.width
    , background config.background
    , cornerRadius config.cornerRadius
    , gravity CENTER_VERTICAL
    , stroke if config.showErrorLabel then config.warningStroke else config.stroke
    ][editTextView push config]



editTextView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
editTextView push config = 
  (if( os == "IOS" && (not config.editText.singleLine )) then multiLineEditText else editText) 
  $ [ height config.height
  , width config.width
  , id config.id
  , weight 1.0
  , textSize config.editText.textSize
  , color config.editText.color
  , fontStyle config.editText.fontStyle
  , text config.editText.text
  , hint config.editText.placeholder
  , singleLine config.editText.singleLine
  , hintColor config.editText.placeholderColor
  , margin config.editText.margin
  , background config.background
  , padding config.editText.padding
  , onChange push $ TextChanged config.id
  , gravity config.editText.gravity
  , letterSpacing config.editText.letterSpacing
  , alpha config.editText.alpha
  ] 
  <> (case config.editText.pattern of 
        Just _pattern -> case config.type of 
                          "text" -> [pattern _pattern
                                    , inputType TypeText]
                          "number" -> [ pattern _pattern
                                      , inputType Numeric]
                          "password" -> [ pattern _pattern
                                      , inputType Password]
                          _    -> [pattern _pattern]
        Nothing -> case config.type of 
                          "text" -> [inputType TypeText]
                          "number" -> [inputType Numeric]
                          "password" -> [inputType Password]
                          _    -> []) 
  <> ( if config.editText.capsLock then [inputTypeI 4097] else [])

errorLabelLayout :: forall w . Config -> PrestoDOM (Effect Unit) w
errorLabelLayout config =
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    ][  imageView
        [ height config.errorImageConfig.height
        , width config.errorImageConfig.width
        , imageUrl config.errorImageConfig.imageUrl
        , margin config.errorImageConfig.margin
        , padding config.errorImageConfig.padding
        , visibility if config.showErrorImage then VISIBLE else GONE
        ]
      , textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , textSize config.errorLabel.textSize
        , text config.errorLabel.text
        , color config.errorLabel.color
        , fontStyle config.errorLabel.fontStyle
        , gravity config.errorLabel.gravity
        , margin config.errorLabel.margin
        , lineHeight "28"
        , singleLine true
        , visibility if config.showErrorLabel then VISIBLE else GONE
        , alpha config.errorLabel.alpha
        ]
    ]