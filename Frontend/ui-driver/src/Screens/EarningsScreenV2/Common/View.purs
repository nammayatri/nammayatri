module Screens.EarningsScreen.Common.View where

import Prelude
import Screens.EarningsScreen.ScreenData
import Common.Types.App
import Data.Maybe
import Effect
import Prelude
import PrestoDOM
import Screens.EarningsScreen.Controller
import Screens.EarningsScreen.ScreenData
import Screens.EarningsScreen.Common.Types
import Prelude
import Common.Types.App
import Data.Maybe
import Effect
import Prelude
import PrestoDOM
import Screens.EarningsScreen.Controller
import Screens.EarningsScreen.ScreenData
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Screens.EarningsScreen.Common.Types
import Helpers.Utils

tabLayout :: forall action a. (action -> Effect Unit) -> action -> EarningsTab -> Layout a
tabLayout push action tabType =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ Padding 4 4 4 4
    , cornerRadius 16.0
    ]
    ( map
        ( \item ->
            let
              isSelected = item.type == tabType
            in
              linearLayout
                ( [ weight 1.0
                  , height WRAP_CONTENT
                  , background $ if isSelected then Color.black900 else Color.white900
                  , cornerRadius 14.0
                  , gravity CENTER
                  ]
                    <> if isSelected then [ onClick push $ const action ] else []
                )
                [ textView
                    $ [ text $ getString item.string
                      , color if isSelected then Color.white900 else Color.black700
                      , margin $ MarginVertical 6 6
                      ]
                    <> FontStyle.tags TypoGraphy
                ]
        )
        tabList
    )

rideComponent :: forall a. RideComponent -> Layout a
rideComponent item =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.grey900
    , padding $ Padding 12 12 12 12
    , gravity CENTER
    , margin $ MarginBottom 12
    , cornerRadius 8.0
    ]
    [ imageView
        [ height $ V 24
        , width $ V 25
        , imageWithFallback $ getVehicleVariantImage "SEDAN"
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginLeft 8
        ]
        [ textView
            $ [ text item.serviceTierType
              , color Color.black900
              ]
            <> FontStyle.paragraphText TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            ]
            [ textView
                $ [ text item.date
                  , color Color.black700
                  ]
                <> FontStyle.body3 TypoGraphy
            , linearLayout
                [ height $ V 3
                , width $ V 3
                , cornerRadius 1.5
                , background Color.black700
                , margin $ MarginHorizontal 6 6
                ]
                []
            , textView
                $ [ text item.time
                  , color Color.black700
                  ]
                <> FontStyle.body3 TypoGraphy
            ]
        ]
    , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , gravity RIGHT
        , orientation VERTICAL
        ]
        $ [ textView
              $ [ text item.price
                , color Color.black
                , margin $ MarginBottom 4
                ]
              <> FontStyle.subHeading1 TypoGraphy
          ]
        <> ( map
              ( \tagItem ->
                  linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , background tagItem.background
                    , padding $ Padding 10 3 10 3
                    , cornerRadius 10.0
                    , margin $ MarginBottom 4
                    ]
                    [ textView
                        $ [ text tagItem.text
                          , color tagItem.color
                          ]
                        <> FontStyle.body17 TypoGraphy
                    ]
              )
              item.tags
          )
    ]

tabList :: Array TabList
tabList =
  [ { string: DAILY
    , "type": TAB_DAILY
    }
  , { string: WEEKLY
    , "type": TAB_WEEKLY
    }
  ]
