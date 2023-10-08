module Screens.RentalBookingFlow.View where

import Components.SearchLocationModel.Controller (Action(..))
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth, setText)
import Prelude ((<>))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not, discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Accessiblity(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), accessibilityHint ,adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, clickable, color, cornerRadius, cursorColor, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, inputTypeI, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility)
import PrestoDOM.Animation as PrestoAnim
import Screens.Types (HomeScreenState)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push (const $ GoBack)
    ] [ linearLayout
      [ height $ V ((screenHeight unit)/ 7)
      , width MATCH_PARENT
      , background state.data.config.searchLocationConfig.searchLocationTheme
      , accessibility DISABLE
      , clickable true
      , onClick push (const NoAction)
      , padding (Padding 0 safeMarginTop 0 0)
      ][]
    ]