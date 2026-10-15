module Components.Safety.SafetyActionTileView where

import PrestoDOM
import Prelude
import Data.Maybe as Mb
import Common.Types.App (LazyCheck(..), RateCardType(..))
import Data.String as DS
import Data.Int as DI
import Data.Maybe as DM
import Animation (translateInXForwardAnim, translateInXBackwardAnim)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (>),(==), (||), (&&), (/), (*), (/=), (+), (<<<), unit, map, (-), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, imageUrl, fontStyle, gravity, height, imageView, textFromHtml,imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, lineHeight,fontStyle, scrollView, maxLines, singleLine, stroke, horizontalScrollView, relativeLayout)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as AnimConfig
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (os, screenWidth, screenHeight)
import Mobility.Prelude (boolToVisibility)
import Timers (startTimer)
import Components.Safety.Utils as SU
import Mobility.Prelude as MP

data Action = OnClick

view :: forall w. String -> String -> (Action -> Effect Unit) -> String -> String -> Length -> Boolean -> Boolean -> String -> Length -> PrestoDOM (Effect Unit) w
view image title push backgroundColor strokeColor width' useMargin isDisabled color' height' =
  linearLayout
    ([ height height'
    , width width'
    , orientation VERTICAL
    , padding $ Padding 16 16 16 16
    , cornerRadius 12.0
    , stroke $ "1," <> strokeColor
    , background backgroundColor 
    , onClick push $ const OnClick
    , gravity CENTER
    , alpha $ if isDisabled then 0.6 else 1.0
    , clickable $ not isDisabled
    ] <> if useMargin then [ margin $ MarginHorizontal 6 6 ] else [])
    [ imageView
        [ imageWithFallback image
        , height $ V 24
        , width $ V 24
        ]
    , textView
        $ [ text title
          , color color'
          , margin $ MarginTop 12
          , gravity CENTER
          ]
        <> FontStyle.body1 TypoGraphy
    ]