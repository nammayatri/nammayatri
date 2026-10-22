module Components.Safety.SosButtonAndDescription.View where

import Prelude
import PrestoDOM

import Animation (translateInXForwardAnim, translateInXBackwardAnim)
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..), RateCardType(..))
import Components.PrimaryButton as PrimaryButton
import Components.Safety.SosButtonAndDescription.Controller (Action(..), Config(..))
import Components.Safety.Utils as SU
import Data.Array as DA
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Maybe as Mb
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons (os, screenWidth, screenHeight)
import Font.Size as FontSize
import Font.Style as FontStyle
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Mobility.Prelude as MP
import Prelude (Unit, ($), const, (<>), (>), (==), (||), (&&), (/), (*), (/=), (+), (<<<), unit, map, (-), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, imageUrl, fontStyle, gravity, height, imageView, textFromHtml, imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, lineHeight, fontStyle, scrollView, maxLines, singleLine, stroke, horizontalScrollView, relativeLayout)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Timers (startTimer)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.black700
    , background Color.blackOpacity12
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    , padding $ Padding 16 16 16 16
    , cornerRadius 12.0
    , alpha $ if config.isDisabled then 0.6 else 1.0
    ]
    [ sosButtonView config push false
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        ]
        (map (\item -> SU.measureView item) config.sosDescription)
    , case config.primaryContactAndEdit of
        Just item -> primaryContactAndEditView item push config.isDisabled config.editContactText
        Nothing -> MP.noView
    ]


-- ---------------------------------- sosButtonView -----------------------------------
sosButtonView :: forall w. Config -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
sosButtonView config push useMargin =
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ]
      [ textView
          $ [ text config.descriptionText
            , color Color.white900
            , margin $ Margin 16 0 16 20
            , width MATCH_PARENT
            , gravity CENTER
            ]
          <> FontStyle.h3 TypoGraphy
      , relativeLayout
          ( [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , accessibilityHint if not config.triggeringSos then "Test SOS button" else ""
            , visibility $ boolToVisibility config.showSosButton
            ]
              <> ( if not (config.triggeringSos || config.isDisabled) then
                    [ onClick
                        ( \action -> do
                            void $ startTimer config.timerValue "triggerSos" "1" push CountDown
                            void $ push action
                        )
                        (const TriggerSosCountdown)
                    ]
                  else
                    []
                )
              <> ( if useMargin then
                    [ margin $ MarginVertical 40 40 ]
                  else
                    []
                )
          )
          [ imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_sos_button"
              , width $ V 188
              , height $ V 188
              ]
          , textView
              $ [ text config.buttonText
                , color Color.white900
                , gravity CENTER
                , width $ V 188
                , height $ V 188
                ]
              <> (if config.triggeringSos then FontStyle.title0 else FontStyle.priceFont) TypoGraphy
          ]
      ]

-- ---------------------------------- primaryContactAndEditView -----------------------------------
primaryContactAndEditView :: forall w. SU.MeasureViewConfig -> (Action -> Effect Unit) -> Boolean -> String -> PrestoDOM (Effect Unit) w
primaryContactAndEditView item push isDisabled editContactText =
  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , margin $ MarginTop 12
        ]
        [ SU.measureView item
        , textView
            $ [ text editContactText
              , color Color.blue800
              , gravity RIGHT
              , onClick push $ const AddContacts
              , clickable $ not isDisabled
              , weight 1.0
              ]
            <> FontStyle.tags TypoGraphy
        ]