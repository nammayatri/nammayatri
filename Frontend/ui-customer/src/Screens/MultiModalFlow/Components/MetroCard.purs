module Screens.MultiModalFlow.Components.MetroCard
  ( view
  , Action(..)
  , dummyRouteInfo
  ) where

import Common.Types.App (LazyCheck(..))
import Data.Array (take, (!!))
import Data.Maybe (fromMaybe, Maybe(..))
import Prelude
import PrestoDOM
import Screens.Types (NewContacts)
import Styles.Types (Color)
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Font.Style as FontStyle
import Helpers.Utils as HU
import Mobility.Prelude (boolToVisibility, layoutWithWeight)
import Styles.Colors as Color
import Services.API as API
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption)
import Data.Maybe (Maybe(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 12 12 12 12
    , orientation VERTICAL
    , cornerRadius 12.0
    , stroke $ "1," <> Color.grey900
    , background Color.white900
    ]
    [ routeDetails config push
    , separatorView Color.grey700 (MarginVertical 12 12)
    , frequencyAndPriceInfoView config push
    , separatorView Color.grey700 (MarginVertical 12 12)
    , stopsInfoView config push
    , stopsView config
    ]

routeDetails :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
routeDetails config push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , background config.routeTagColor
            , padding $ Padding 8 5 8 5
            , margin $ MarginRight 4
            , cornerRadius 6.0
            ]
            [ textView
                $ [ text config.routeName
                  , color Color.white900
                  ]
                <> FontStyle.tags TypoGraphy
            ]
        , textView
            $ [ text config.details
              , color Color.black800
              ]
            <> FontStyle.tags TypoGraphy
        , layoutWithWeight
        , textView
            $ [ text config.time
              , color Color.black800
              , gravity RIGHT
              ]
            <> FontStyle.tags TypoGraphy
        ]
    , textView
        $ [ text config.destinationDetails
          , color Color.black700
          , gravity RIGHT
          ]
        <> FontStyle.tags TypoGraphy
    ]

frequencyAndPriceInfoView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
frequencyAndPriceInfoView config push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    ]
    [ textView
        $ [ text config.transportFrequency
          , color Color.black700
          ]
        <> FontStyle.tags TypoGraphy
    , dotView
    , textView
        $ [ text config.timeInterval
          , color Color.black700
          , gravity RIGHT
          ]
        <> FontStyle.tags TypoGraphy
    , layoutWithWeight
    , textView
        $ [ text config.price
          , color Color.black900
          , gravity RIGHT
          ]
        <> FontStyle.tags TypoGraphy
    ]

stopsInfoView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
stopsInfoView config push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    ]
    [ textView
        $ [ text $ show config.numberOfStops
          , color Color.black900
          ]
        <> FontStyle.tags TypoGraphy
    , dotView
    , textView
        $ [ text config.duration
          , color Color.black900
          , gravity RIGHT
          ]
        <> FontStyle.tags TypoGraphy
    , layoutWithWeight
    , imageView
        [ imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET $ if config.expandStops then "ny_ic_chevron_up" else "ny_ic_chevron_down"
        , height $ V 20
        , width $ V 20
        , gravity RIGHT
        ]
    ]

stopsView :: forall w. Config -> PrestoDOM (Effect Unit) w
stopsView config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , visibility $ boolToVisibility $ config.expandStops
    ]
    ( map
        ( \stop ->
            textView
              [ text stop
              , margin $ MarginTop 8
              ]
        )
        config.stops
    )

separatorView :: forall w. String -> Margin -> PrestoDOM (Effect Unit) w
separatorView color' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    ]
    []

data Action
  = ExpandStops

type Config
  = { routeTagColor :: String
    , routeName :: String
    , details :: String
    , time :: String
    , destinationDetails :: String
    , expandStops :: Boolean
    , stops :: Array String
    , price :: String
    , transportFrequency :: String
    , timeInterval :: String
    , duration :: String
    , numberOfStops :: Int
    }

dummyRouteInfo :: Config
dummyRouteInfo =
  { routeTagColor: "red"
  , routeName: "Bus 42"
  , details: "Fastest route with minimum stops"
  , time: "14:30"
  , destinationDetails: "Central Station"
  , expandStops: true
  , stops: [ "Main St" ]
  , price: "$2.50"
  , transportFrequency: "Every 15 minutes"
  , timeInterval: "14:30 - 15:00"
  , duration: "30 minutes"
  , numberOfStops: 5
  }

dotView :: forall w. PrestoDOM (Effect Unit) w
dotView = imageView
        [ imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_circle"
        , height $ V 4
        , width $ V 4
        , margin $ MarginHorizontal 4 4
        ]