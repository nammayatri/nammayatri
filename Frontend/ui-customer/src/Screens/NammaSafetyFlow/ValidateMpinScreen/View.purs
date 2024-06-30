{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ValidateMpinScreen.View where

import Animation (screenAnimationFadeInOut)
import Prelude (Unit, const, discard, not, pure, unit, void, ($), (&&), (<<<), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, alignParentBottom, alpha, background, color, cornerRadius, gravity, height, id, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, onAnimationEnd, onBackPressed, onClick, orientation, padding, relativeLayout, rippleColor, scrollView, stroke, text, textView, visibility, weight, width, accessibilityHint)
import Screens.NammaSafetyFlow.ComponentConfig (primaryEditTextConfig)
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (mapWithIndex, null)
import Data.Maybe (Maybe)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, getLocationName, getAssetsBaseUrl)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (doAff)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.ValidateMpinScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Screens.NammaSafetyFlow.Components.SafetyUtils (getVehicleDetails)
import Components.PrimaryEditText as PrimaryEditText

screen :: ST.NammaSafetyScreenState -> Screen Action ST.NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "ValidateMpinScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let
          _ = spy "ValidateMpinScreen action " action
        let
          _ = spy "ValidateMpinScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  ]
  [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , padding padding'
      , onBackPressed push $ const $ BackPressed

      ]
      [ Header.view (push <<< SafetyHeaderAction) headerConfig
      , mpinView state push
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , alignParentBottom "true,-1"
      ] [ PrimaryButton.view (push <<< ContinueBtn) (continueMpinButtonConfig state) ]
  ]
  where
  padding' = if EHC.os == "IOS" then (PaddingVertical EHC.safeMarginTop (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

  headerConfig =
    (Header.config Language)
      { useLightColor = false
      , title = "MPIN Verification" -- getString $ if not state.props.showCallPolice then EMERGENCY_SOS else CALL_POLICE
      , learnMoreTitle = ""
      , showLearnMore = false
      }

mpinView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
mpinView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    ]
    [ textView
        $ [ text "MPIN for safety checks"
          , color Color.black900
          , margin $ MarginBottom 8
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , PrimaryEditText.view (push <<< PrimaryEditTextAC ) (primaryEditTextConfig state false)
    ]

continueMpinButtonConfig :: ST.NammaSafetyScreenState -> PrimaryButton.Config
continueMpinButtonConfig state =
  let
    config = PrimaryButton.config
    continueMpinButtonConfig' = config
      { textConfig
      { text = "Continue"
      }
      , margin = (Margin 16 16 16 16)
      , isClickable = true
      , alpha = 1.0
      , id = "ValidateMpinScreenPrimaryButton"
      }
  in continueMpinButtonConfig'