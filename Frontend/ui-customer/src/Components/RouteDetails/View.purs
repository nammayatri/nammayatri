module Components.RouteDetails.View where

import Prelude
import Components.RouteDetails.Controller (Action(..), Config)
import Common.Types.App
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||), (<), (>), (/=), bind, when, void, (*))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, alpha, layoutGravity)
import Common.Styles.Colors as Color
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Engineering.Helpers.Commons as EHC
import Data.Array (concat)
import Debug

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
    linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.grey700
    , orientation VERTICAL
    ] [ linearLayout
        [ height $ V 59
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , background Color.white900
        , padding $ Padding 16 16 16 16
        , orientation HORIZONTAL
        ] [ imageView
            [ imageWithFallback $ "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
            , height $ V 27
            , width $ V 24
            , margin $ Margin 0 0 8 0
            , gravity CENTER_VERTICAL
            , clickable true
            , onClick push $ const CloseRouteDetailsView
            ]
          , textView $
            [ text "Route Details"
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
          ]
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ Margin 16 24 16 0
        ] [ textView $
            [ text "Route"
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.black800
            , margin $ MarginLeft 9
            ] <> FontStyle.body6 TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ MarginTop 4
            , orientation VERTICAL
            , padding $ Padding 16 16 16 16
            , background Color.yellow100
            ] [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ] [ linearLayout 
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation HORIZONTAL
                    , margin $ Margin 0 4 0 4
                    ] [ imageView
                          [ imageWithFallback $ "ny_ic_green_location_circle," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_green_location_circle.png"
                          , height $ V 16
                          , width $ V 16
                          , margin $ Margin 0 0 8 0
                          ]
                      , textView $
                          [ text "Current Location"
                          , width WRAP_CONTENT
                          , height WRAP_CONTENT
                          , color Color.black800
                          , weight 1.0
                          ] <> FontStyle.body6 TypoGraphy
                      , textView $ 
                          [ text "3:08 PM" --config.start.scheduledTime
                          , width WRAP_CONTENT
                          , height WRAP_CONTENT
                          , color Color.black600
                          , gravity RIGHT
                          ] <> FontStyle.body1 TypoGraphy
                      ]
                  , imageView
                    [ imageWithFallback $ "ny_ic_dotted_line_small," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_dotted_line_small.png"
                    , height $ V 32
                    , width $ V 16
                    ]
                  , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , padding $ Padding 0 4 0 4
                    , orientation HORIZONTAL
                    ] [ linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , orientation VERTICAL
                        ] [ imageView
                            [ imageWithFallback $ "ny_ic_walk," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_walk.png"
                            , height $ V 16
                            , width $ V 16
                            ]
                          , imageView
                            [ imageWithFallback $ "ny_ic_dotted_line_long," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_dotted_line_long.png"
                            , height $ V 66
                            , width $ V 16
                            ]  
                        ]
                      , linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , orientation VERTICAL
                        , margin $ Margin 8 0 0 0]
                        [ textView $
                          [ text "Walk to Bus Stop"
                          , width WRAP_CONTENT
                          , height WRAP_CONTENT
                          , color Color.black800
                          , gravity CENTER_VERTICAL
                          ] <> FontStyle.body6 TypoGraphy
                        , linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , orientation HORIZONTAL
                          , margin $ Margin 0 8 0 0
                          , padding $ Padding 8 6 8 6
                          , gravity CENTER
                          , background Color.grey700
                          ] [ textView
                              [ text "0.65km"
                              , width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , color Color.black800
                              ]
                            , linearLayout
                              [ height $ V 4
                              , width $ V 4
                              , cornerRadius 2.5
                              , background Color.black500
                              , gravity CENTER_VERTICAL
                              , margin $ Margin 8 0 8 0
                              ][]
                            , imageView 
                              [ imageWithFallback $ "ny_ic_clock_unfilled," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_clock_unfilled.png"
                              , width $ V 20
                              , height $ V 20
                              , margin $ Margin 0 0 3 0
                              ]
                            , textView
                              [ text "10 m"
                              , width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , color Color.black800
                              ]
                            ]
                        ]
                      ]
                    , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation HORIZONTAL
                      , padding $ Padding 0 4 0 4
                      ] [ linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , orientation VERTICAL
                          ] [ imageView
                              [ imageWithFallback $ "ny_ic_green_bus_vector," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_green_bus_vector.png"
                              , height $ V 16
                              , width $ V 16
                              ]
                              , imageView
                              [ imageWithFallback $ "ny_ic_purple_line_long," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_purple_line_long.png"
                              , height $ V 106
                              , width $ V 16
                              ]
                            ]
                        , linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , orientation VERTICAL
                          , margin $ Margin 8 0 0 0
                          ] [ linearLayout 
                              [ height WRAP_CONTENT
                              , width MATCH_PARENT
                              , orientation HORIZONTAL
                              ] [ textView $
                                  [ text "Maharaja Signal Stop"
                                  , width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , color Color.black800
                                  , gravity CENTER_VERTICAL
                                  , weight 1.0
                                  ] <> FontStyle.body6 TypoGraphy
                                  , textView $ 
                                  [ text "3:17 PM" --config.stop[0].scheduledTime
                                  , width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , color Color.black600
                                  , gravity RIGHT
                                  ] <> FontStyle.body1 TypoGraphy
                                ]
                            , linearLayout
                              [ height WRAP_CONTENT
                              , width WRAP_CONTENT
                              , orientation HORIZONTAL
                              , margin $ Margin 0 12 0 12
                              , padding $ Padding 8 6 8 6
                              , background Color.grey700
                              , gravity CENTER
                              ] [ textView
                                  [ text "12km"
                                  , width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , color Color.black800
                                  ]
                                , linearLayout
                                  [ height $ V 4
                                  , width $ V 4
                                  , cornerRadius 2.5
                                  , background Color.black500
                                  , gravity CENTER_VERTICAL
                                  , margin $ Margin 8 0 8 0
                                  ][]
                                , imageView 
                                  [ imageWithFallback $ "ny_ic_clock_unfilled," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_clock_unfilled.png"
                                  , width $ V 20
                                  , height $ V 20
                                  , margin $ Margin 0 0 3 0
                                  ]
                                , textView
                                  [ text "28 m"
                                  , width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , color Color.black800
                                  ]
                                , linearLayout
                                  [ height $ V 4
                                  , width $ V 4
                                  , cornerRadius 2.5
                                  , background Color.black500
                                  , gravity CENTER_VERTICAL
                                  , margin $ Margin 8 0 8 0
                                  ][]
                                , imageView 
                                  [ imageWithFallback $ "ny_ic_money_unfilled," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_money_unfilled.png"
                                  , width $ V 20
                                  , height $ V 20
                                  , margin $ Margin 0 0 4 0
                                  ]
                                , textView
                                  [ text "₹ 46"
                                  , width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , color Color.black800
                                  ]
                                ]
                            , linearLayout 
                              [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , padding $ Padding 8 0 8 0
                              , background Color.blueMagentaOpacity10
                              ] [ textView $ 
                                  [ width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , text config.busId 
                                  , color Color.blueMagenta
                                  ] <> FontStyle.body15 TypoGraphy
                              ]
                            ]
                        ]
                    , linearLayout 
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation HORIZONTAL
                      , padding $ Padding 0 4 0 4
                      ] [ imageView
                            [ imageWithFallback $ "ny_ic_red_bus_vector," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_red_bus_vector.png"
                            , height $ V 16
                            , width $ V 16
                            , margin $ Margin 0 0 8 0
                            ]
                        , textView $
                            [ text "Shanti Nagar Bus Depot"
                            , width WRAP_CONTENT
                            , height WRAP_CONTENT
                            , color Color.black800
                            , weight 1.0
                            ] <> FontStyle.body6 TypoGraphy
                        , textView $ 
                            [ text "3:45 PM" --config.stop[1].scheduledTime
                            , width WRAP_CONTENT
                            , height WRAP_CONTENT
                            , color Color.black600
                            , gravity RIGHT
                            ] <> FontStyle.body1 TypoGraphy
                        ]
                    , imageView
                      [ imageWithFallback $ "ny_ic_dotted_line_small," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_dotted_line_small.png"
                      , height $ V 32
                      , width $ V 16
                      ]
                    , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , padding $ Padding 0 4 0 4
                      , orientation HORIZONTAL
                      ] [ linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , orientation VERTICAL
                          ] [ imageView
                              [ imageWithFallback $ "ny_ic_walk," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_walk.png"
                              , height $ V 16
                              , width $ V 16
                              ]
                            , imageView
                              [ imageWithFallback $ "ny_ic_dotted_line_long," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_dotted_line_long.png"
                              , height $ V 66
                              , width $ V 16
                              ]  
                          ]
                        , linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , orientation VERTICAL
                          , margin $ Margin 8 0 0 0
                          ] [ textView $
                              [ text "Walk to Destination"
                              , width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , color Color.black800
                              , gravity CENTER_VERTICAL
                              ] <> FontStyle.body6 TypoGraphy
                            , linearLayout
                              [ height WRAP_CONTENT
                              , width WRAP_CONTENT
                              , orientation HORIZONTAL
                              , margin $ Margin 0 8 0 0
                              , padding $ Padding 8 6 8 6
                              , gravity CENTER
                              , background Color.grey700
                              ] [ textView
                                  [ text "0.21km"
                                  , width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , color Color.black800
                                  ]
                                , linearLayout
                                  [ height $ V 4
                                  , width $ V 4
                                  , cornerRadius 2.5
                                  , background Color.black500
                                  , gravity CENTER_VERTICAL
                                  , margin $ Margin 8 0 8 0
                                  ][]
                                , imageView 
                                  [ imageWithFallback $ "ny_ic_clock_unfilled," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_clock_unfilled.png"
                                  , width $ V 20
                                  , height $ V 20
                                  , margin $ Margin 0 0 3 0
                                  ]
                                , textView
                                  [ text "4 m"
                                  , width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , color Color.black800
                                  ]
                                ]
                            ]
                        ]
                    , linearLayout 
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation HORIZONTAL
                      , margin $ Margin 0 4 0 4
                      ] [ imageView
                            [ imageWithFallback $ "ny_ic_red_location_circle," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_red_location_circle.png"
                            , height $ V 16
                            , width $ V 16
                            , margin $ Margin 0 0 8 0
                            ]
                        , textView $
                            [ text "Golden Nest" -- config.end.location.place
                            , width WRAP_CONTENT
                            , height WRAP_CONTENT
                            , color Color.black800
                            , weight 1.0
                            ] <> FontStyle.body6 TypoGraphy
                        , textView $ 
                            [ text "3:51 PM" --config.end.scheduledTime
                            , width WRAP_CONTENT
                            , height WRAP_CONTENT
                            , color Color.black600
                            , gravity RIGHT
                            ] <> FontStyle.body1 TypoGraphy
                        ]
                    , textView $
                      [ text "Hide details"
                      , width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , color Color.blue900
                      , margin $ Margin 0 0 16 0
                      , clickable true
                      , onClick push $ const CloseRouteDetailsView
                      ] <> FontStyle.body1 TypoGraphy
                  ]
              ]
          ]
      ]