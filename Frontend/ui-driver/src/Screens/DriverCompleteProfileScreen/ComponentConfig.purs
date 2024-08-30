module Screens.DriverCompleteProfileScreen.ComponentConfig where

import Screens.Types as ST
import Components.PrimaryButton.Controller as PrimaryButton
import Styles.Colors as Color
import Components.Calendar.Controller as CalendarConfig
import Language.Strings (getString, getVarString)
import Language.Types(STR(..))
import Data.Maybe(isJust)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, scrollView)
import Prelude((<>), (/), unit, ($), (-), (==))
import Engineering.Helpers.Utils as EHU
import Components.AddImagesModel as AddImagesModel
import Components.ViewImageModel as ViewImageModel
import Helpers.Utils (fetchImage, FetchImageFrom(..), getPastYears)
import Components.InputTextView as InputTextView
import Data.Array((:))
import Components.InputTextView.Controller
import JBridge as JB
import Styles.Colors as Color
import Engineering.Helpers.Commons (screenWidth)

donePrimaryButtonConfig :: ST.DriverCompleteProfileScreenState -> PrimaryButton.Config
donePrimaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig { 
          text = getString SUBMIT 
        , color = Color.primaryButtonColor
        }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 48
      , alpha = 1.0 
      , isClickable = true
      , margin = Margin 16 16 16 16
      , id = ""
      }
  in primaryButtonConfig'

addImageModelConfig :: ST.DriverCompleteProfileScreenState -> AddImagesModel.AddImagesModelState
addImageModelConfig state = let 
    addImageModelConfig' = AddImagesModel.config
      {
        doneButtonText  = getString DONE,
        addedImagesText = getString ADDED_IMAGES,
        noImagesAddedText = getString NO_IMAGES_ADDED,
        viewText = getString VIEW,
        deleteText = getString DELETE,
        addAnotherText = getString ADD_ANOTHER,
        addImageText = getString ADD_IMAGE,
        images = state.data.addImagesState.images,
        stateChanged = state.data.addImagesState.stateChanged,
        isLoading = state.data.addImagesState.isLoading,
        imageMediaIds = state.data.addImagesState.imageMediaIds,
        noOfImages = 4
      }
  in addImageModelConfig'

viewImageModelConfig :: ST.DriverCompleteProfileScreenState -> ViewImageModel.ViewImageModelState
viewImageModelConfig state = let 
    viewImageModelConfig' = ViewImageModel.config
      {
        imagePreviewText = getString IMAGE_PREVIEW,
        image = state.data.viewImageState.image,
        imageName = state.data.viewImageState.imageName
      }
  in viewImageModelConfig'

inputTextConfig :: ST.DriverCompleteProfileScreenState -> InputTextView.InputTextConfig
inputTextConfig state =
  { data:
    { title : if state.data.inputTextState.component == "pledge" then getString PLEDGE else getString WHY_NY
    , doneButtonConfig:
      PrimaryButton.config
        { textConfig
          { text = getString SAVE
          , color = Color.yellow900
          , accessibilityHint = "Submit Other" 
          }
        , background = Color.black900
        , margin = MarginHorizontal 16 8
        , isClickable = true
        , id = "Submit Other"
        , enableLoader = (JB.getBtnLoader "Submit Other")
        , enableRipple = true
        , rippleColor = Color.rippleShade
        , width = V $ (screenWidth unit / 2) - 25
        }
  , cancelButtonConfig:
      PrimaryButton.config
        { textConfig
          { text = getString CANCEL
          , color = Color.black700
          , accessibilityHint = "Cancel Other" 
          }
        , background = Color.white900
        , isClickable = true
        , id = "Cancel Other"
        , enableLoader = (JB.getBtnLoader "Cancel Other")
        , enableRipple = true
        , rippleColor = Color.rippleShade
        , stroke= ("1," <> Color.black500)
        , width = V $ (screenWidth unit / 2) - 25
        , margin = (Margin 10 0 5 0)
        }}
  }