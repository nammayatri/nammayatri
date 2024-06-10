module Screens.DeleteAccountScreen.View where

import Prelude
import PrestoDOM
import Effect
import Data.Maybe
import Screens.DeleteAccountScreen.ScreenData
import Screens.DeleteAccountScreen.Controller
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Engineering.Helpers.Commons as EHC
import Styles.Colors as Color
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.DeleteAccountScreen.ComponentConfig
import Components.PopUpModal as PopUpModal
import Common.Types.App
import Animation as Anim

screen :: State -> Screen Action State ScreenOutput
screen initialState =
    { initialState
    , view: view
    , name: "DelectAccountScreen"
    , globalEvents: []
    , eval
    }


view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
  [
    height MATCH_PARENT
  , width MATCH_PARENT
  ]$[
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingTop EHC.safeMarginTop
  , background Color.white900
  , clickable true
  ][
    GenericHeader.view (push <<< DeleteGenericHeaderAC) (genericHeaderConfig state) 
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (Padding 16 12 16 12)
    , background Color.blue600
    ][ 
      textView $
      [ text $ getString WE_WOULD_APPRECIATE_YOUR_REASONING      
      , color Color.black650
      ] <> FontStyle.tags LanguageStyle
    ]
  , relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , adjustViewWithKeyboard "true"
    ][  editTextView state push
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.white900
        , alignParentBottom "true,-1"
        , weight 1.0 
        ][PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)]
      ]
    ]
  ]   <> (if state.props.showConfirmPopUp then [PopUpModal.view (push <<<  PopUpModalAction) (requestDeletePopUp state )] else [])
      <> (if state.props.showRequestSubmitted then [PopUpModal.view (push <<<  RSPopUpModalAction) (requestSubmittedPopUp state )] else [])

editTextView :: State -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
editTextView state push =
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][
    scrollView[
      width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding (Padding 6 0 6 60)
      , orientation VERTICAL
      ][
          PrimaryEditText.view (push <<< EmailEditTextAC) (emailPrimaryEditTextConfig state)
        , PrimaryEditText.view (push <<< DescriptionEditTextAC) (descriptionPrimaryEditTextConfig state)
      ]
    ]
  ]