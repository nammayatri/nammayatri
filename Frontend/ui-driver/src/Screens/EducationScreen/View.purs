module Screens.EducationScreen.View where

import Animation (screenAnimationFadeInOut, screenAnimation)
import Common.Types.App (LazyCheck(..), YoutubeData)
import Types.App (defaultGlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (mapWithIndex, (!!)) as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (length, trim)
import Data.String.CodeUnits (charAt)
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, screenWidth, getVideoID, getYoutubeData)
import Font.Size as FontSize
import Font.Style as FontStyle
import Data.Function.Uncurried (runFn5)
import Helpers.Utils (fetchImage, FetchImageFrom(..), parseNumber)
import JBridge (renderBase64Image, openUrlInApp, setScaleType, setYoutubePlayer, addMediaPlayer)
import Language.Strings (getString)
import Components.PrimaryButton (view) as PrimaryButton
import Language.Types (STR(..))
import Prelude (Unit, map, bind, const, pure, show, unit, void, discard, ($), (<<<), (<>), (==), (&&), (-), (*), (/))
import PrestoDOM (Gravity(..), Screen(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageUrl, imageView, linearLayout, margin, onAnimationEnd, onClick, orientation, padding, progressBar, relativeLayout, stroke, text, textSize, textView, visibility, weight, width, scrollBarY, scrollView, lineHeight, textFromHtml, imageWithFallback, rippleColor, fillViewport, onBackPressed)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Properties (alpha, visibility, background, color, cornerRadius, fontStyle, gravity, height, id, imageWithFallback, layoutGravity, margin, orientation, padding, stroke, textSize, weight, width)
import Components.GenericHeader.View as GenericHeader
import Services.API (MediaType(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Styles.Types (FontStyle)
import Screens.Types as ST
import Engineering.Helpers.Commons as EHC
import Resource.Localizable.TypesV2 as LT2
import Common.Types.App as CT
import Resource.Localizable.StringsV2 as StringsV2
import Screens.EducationScreen.Controller (Action(..), ScreenOutput, primaryButtonConfig, genericHeaderConfig, eval)
import Debug (spy)
import RemoteConfig as RC

educationScreen :: ST.EducationScreenState -> Screen Action ST.EducationScreenState ScreenOutput
educationScreen initialState = 
  { initialState
  , view
  , name: "educationScreen"
  , globalEvents: []
  , eval:
    \action state -> do
      let _ = spy "EducationScreen action " action
      let _ = spy "EducationScreen state " state
      eval action state 
}

view :: forall w. (Action -> Effect Unit) -> ST.EducationScreenState -> PrestoDOM (Effect Unit) w
view push state =
    screenAnimation $
    relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , onBackPressed push $ const GoBack
        , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
        , onClick push $ const NoAction
        ]
        [
            linearLayout 
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , background Color.white900
            , orientation VERTICAL
            ][
                GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
                , linearLayout
                [ height $ V 1 
                , width MATCH_PARENT
                , background Color.grey900
                ] []
                , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                ][ 
                    instructionView push state
                ]
            ]
            , separatorView push state
            , footerView push state
        ]

instructionView :: forall w . (Action -> Effect Unit) -> ST.EducationScreenState -> PrestoDOM (Effect Unit) w
instructionView push state =
    scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , fillViewport true
    ][ 
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 100
      ]
      [
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , background Color.blue600
        , padding $ Padding 16 16 16 16
        , cornerRadius 16.0
        ]
        [ 
        videoView push state
        , textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ state.instructionText
            , color Color.black800
            , margin $ MarginVertical 20 20
            ]
          <> FontStyle.subHeading3 CT.TypoGraphy
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          ( map (\item -> instructionItem item) state.descriptionList)
        ]
    ]
    ]

videoView :: forall w . (Action -> Effect Unit) -> ST.EducationScreenState -> PrestoDOM (Effect Unit) w
videoView push state =
    linearLayout
    [
      height MATCH_PARENT
    , width MATCH_PARENT
    ][
        screenAnimationFadeInOut
        $ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , id (getNewIDWithTag "EducationalVideoView")
            , cornerRadius 12.0
            , afterRender
                ( \action -> do
                    let
                        id = (getNewIDWithTag "EducationalVideoView")
                        url = state.videoUrl
                    pure $ runFn5 setYoutubePlayer (getYoutubeDataConfig "VIDEO" (getVideoID url)) id ("PLAY") push YoutubeVideoStatus
                )
                (const NoAction)
            ][]
    ]
    where
        getYoutubeDataConfig videoType videoId = getYoutubeData {
        videoType = videoType,
        videoId = videoId
      }


instructionItem :: forall w. RC.WMBEducationDescription -> PrestoDOM (Effect Unit) w
instructionItem item =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 16
  ]
  [ imageView
    [ width $ V 20
    , height $ V 20
    , imageWithFallback $ fetchImage COMMON_ASSET item.imageUrl
    , margin $ MarginRight 8
    ]
  , textView
    $ [ text item.description
      , color Color.black800
      ]
    <> FontStyle.body20 TypoGraphy
  ]

separatorView :: forall w. (Action -> Effect Unit) -> ST.EducationScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , margin $ MarginVertical 5 5
  , background Color.grey900
  ]
  []

footerView :: forall w. (Action -> Effect Unit) -> ST.EducationScreenState -> PrestoDOM (Effect Unit) w
footerView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , background Color.transparent
    ][ 
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingBottom 16
        , orientation VERTICAL
        , background Color.white900
        ]
        [ linearLayout  
          [ height $ V 1
          , width MATCH_PARENT
          , margin $ MarginBottom 16
          , background Color.grey900
          ][]
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
      ]
    ]