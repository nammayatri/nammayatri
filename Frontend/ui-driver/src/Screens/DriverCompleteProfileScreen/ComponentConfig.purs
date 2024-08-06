module Screens.DriverCompleteProfileScreen.ComponentConfig where

import Screens.Types as ST
import Components.PrimaryButton.Controller as PrimaryButton
import Styles.Colors as Color
import Components.Calendar.Controller as CalendarConfig
import Language.Strings (getString, getVarString)
import Language.Types(STR(..))
import Data.Maybe(isJust)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, scrollView)
import Prelude((<>))
import Engineering.Helpers.Utils as EHU
import Components.AddImagesModel as AddImagesModel
import Components.ViewImageModel as ViewImageModel

donePrimaryButtonConfig :: ST.DriverCompleteProfileScreenState -> PrimaryButton.Config
donePrimaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig { 
          text = "Submit"
        , color = Color.primaryButtonColor
        }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 48
      , alpha = 1.0 -- if state.stateChanged then 1.0 else 0.5
      , isClickable = true --state.stateChanged
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
        imageMediaIds = state.data.addImagesState.imageMediaIds
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