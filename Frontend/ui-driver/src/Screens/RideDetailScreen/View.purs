module Screens.RideDetailScreen.View where

import Prelude (Unit, bind, discard, const, pure, unit, ($), (+), (-), (/), (<>), show)
import PrestoDOM --(Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, scrollView, frameLayout, alignParentBottom)
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.RideDetailScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Services.APITypes(Route(..))
import Services.Backend as Remote
import Services.Backend (walkCoordinate)
import Common.Types.App

screen :: ST.RideDetailScreenState -> Screen Action ST.RideDetailScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "RideDetailScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT 
  , background Color.white900
  , afterRender (\action -> do 
                _ <- push action
                launchAff_ $ EHC.flowRunner $ runExceptT $ runBackT $ do
                  let coor = (walkCoordinate state.data.sourceAddress.lon state.data.sourceAddress.lat state.data.destAddress.lon state.data.destAddress.lat)
                  _ <- lift $ lift $ doAff do liftEffect $ JB.mapSnapShot (EHC.getNewIDWithTag "RideDetailScreenMap") coor "DOT" false push MapSnapShot
                  pure unit 
                pure unit 
                ) (const AfterRender)
  ][  linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          ][  linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , weight 1.0
              , orientation VERTICAL
              ][  scrollView
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ][  linearLayout 
                      [ width MATCH_PARENT
                      , height MATCH_PARENT
                      , orientation VERTICAL
                      , gravity CENTER_HORIZONTAL
                      , margin (Margin 20 ((EHC.screenHeight unit)/18) 20 0)
                      ][  textView (
                          [ width WRAP_CONTENT
                          , height WRAP_CONTENT
                          , text $ (getString RIDE_COMPLETED_WITH) <> " " <> state.data.customerName
                          , color Color.black800
                          ] <> FontStyle.h3 TypoGraphy
                          )
                        , textView (
                          [ width WRAP_CONTENT
                          , height WRAP_CONTENT
                          , text (getString COLLECT_AMOUNT_IN_CASH)
                          , color Color.black700
                          , margin (MarginTop 10)
                          ] <> FontStyle.paragraphText TypoGraphy
                          )
                        , totalAmount state
                        , linearLayout
                          [ width MATCH_PARENT
                          , height (V 1)
                          , background Color.grey900
                          ][]
                        , linearLayout 
                          [ width MATCH_PARENT
                          , height WRAP_CONTENT
                          , orientation HORIZONTAL
                          , margin (Margin 0 20 0 20)
                          ][  textView (
                              [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , text state.data.bookingDateAndTime
                              ] <> FontStyle.subHeading1 TypoGraphy
                              )
                          ]
                        , address state
                        , routeMap state
                        , linearLayout
                          [ width MATCH_PARENT
                          , height (V 50)
                          ][]
                      ]
                  ]
              ]
            , cashCollected state push
          ]
        
      ]
  ]

totalAmount :: forall w . ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
totalAmount state = 
 linearLayout
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 , orientation HORIZONTAL
 , margin (MarginVertical ((EHC.screenHeight unit)/30) ((EHC.screenHeight unit)/30))
 ][ textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text "₹"
  , color Color.black
  , margin (MarginRight 2) 
  , textSize FontSize.a_44
  ],
  textView (
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text $ show state.data.totalAmount
  , color Color.black
  ] <> FontStyle.priceFont_big TypoGraphy
  )
 ]

address :: forall w . ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
address state = 
  frameLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ][ imageView
         [ width (V 19)
         , height (V 20)
         , imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
         ]
       , textView (
         [ width WRAP_CONTENT
         , height WRAP_CONTENT 
         , margin (MarginLeft 10)
         , ellipsize true
         , singleLine true
         , text state.data.sourceAddress.place  -- function for long text
         , color Color.black800
         ] <> FontStyle.body1 TypoGraphy
         )
      ] 
    , textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text state.data.rideStartTime
      , margin (Margin 25 22 20 5)
      ] <> FontStyle.body3 TypoGraphy
      )
    , imageView 
      [ width (V 5)
      , height (V ((EHC.screenHeight unit)/13))
      , imageUrl "ic_line"
      , margin (Margin 6 20 0 0)
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin (MarginTop ((EHC.screenHeight unit)/12))
      , gravity CENTER_VERTICAL
      ][ imageView
         [ width (V 15)
         , height (V 15)
         , imageWithFallback "ny_ic_destination,https://assets.juspay.in/nammayatri/images/common/ny_ic_destination.png"
         ]
       , textView (
         [ width WRAP_CONTENT
         , height WRAP_CONTENT 
         , margin (MarginLeft 10)
         , ellipsize true
         , singleLine true
         , text state.data.destAddress.place  -- function for long text
         , color Color.black800
         ] <> FontStyle.body1 TypoGraphy
         )
      ]
    , textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text state.data.rideEndTime
      , margin (Margin 25 (((EHC.screenHeight unit)/12) + 22) 20 5)
      ] <> FontStyle.body3 TypoGraphy
      )
  ]

routeMap :: forall w . ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
routeMap state = 
  linearLayout 
  [ width MATCH_PARENT
  , height $ V ((EHC.screenWidth unit) - 140)
  , margin (Margin 0 20 0 20)
  , background Color.grey900
  ][  linearLayout 
      [ width MATCH_PARENT
      , height WRAP_CONTENT 
      , background Color.black
      , id (EHC.getNewIDWithTag "RideDetailScreenMap")
      ][]
  ]

cashCollected :: forall w . ST.RideDetailScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cashCollected state push = 
  linearLayout
  [ width MATCH_PARENT
  , height (V 60)
  , background Color.black900
  , alignParentBottom "true,-1"
  , gravity CENTER
  -- , clickable state.props.cashCollectedButton  // require this to enable button only when our map snap shot (base64) is ready 
  , onClick push (const GoToHome)
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString CASH_COLLECTED)
      , fontStyle $ FontStyle.bold LanguageStyle
      , color Color.yellowText
      , textSize FontSize.a_16 
      ] <> FontStyle.subHeading1 TypoGraphy
  )
  ]