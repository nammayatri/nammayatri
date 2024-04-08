module Screens.RideRequestPopUp.View where

import Data.Maybe
import Debug
import Effect
import Prelude
import Prelude
import PrestoDOM
import Screens.RideRequestPopUp.Controller
import Api.Types (NearBySearchRequestRes(..))
import Components.SeparatorView.View as SeparatorView
import Data.Array (mapWithIndex)
import Data.Either (Either(..))
import Data.Time (Millisecond)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (liftAff)
import Font.Style as FontStyle
import Helpers.Colors as Color
import Helpers.Commons (flowRunner, getHeightFromPercent, safeMarginBottom, screenHeight)
import Helpers.Commons (safeMarginBottom, safeMarginTop, screenWidth)
import Helpers.Commons (screenWidth)
import Helpers.Pooling (delayViaTimer)
import Presto.Core.Types.Language.Flow (Flow)
import PrestoDOM.List (listDataV2, listItem, onClickHolder, textHolder, viewPager2)
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData)
import Services.Backend (nearBySearchRequest)
import Types (LazyCheck(..), OverlayData(..), defaultOverlayData)

type Layout w
  = PrestoDOM (Effect Unit) w

screen :: RideRequestPopUpScreenData -> ScopedScreen Action RideRequestPopUpScreenData ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "RideRequestPopUp"
  , globalEvents:
      [ ( \push -> do
            fiber <- launchAff $ flowRunner defaultOverlayData $ getRideRequest push
            pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber
        )
      ]
  , parent: Just "RideRequestPopUp"
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.blackLessTrans
    , padding $ PaddingTop safeMarginTop
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ rideRequestTab push state
        , sheetView push state
        , linearLayout [ weight 1.0 ] []
        , bottomLogoView state
        ]
    ]

rideRequestTab :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
rideRequestTab push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER
    , padding $ PaddingHorizontal 16 16
    , background Color.white900
    , cornerRadius 8.0
    , margin $ MarginHorizontal 16 16
    ]
    $ []
    <> (mapWithIndex (\idx item -> singleTabView push state idx) [ 1, 2, 3 ])

singleTabView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Int -> Layout w
singleTabView push state idx =
  linearLayout
    [ width $ V $ getSingleTab
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ Padding 14 20 14 20
    , gravity CENTER
    , cornerRadius 8.0
    ]
    [ textView
        $ [ text "$10"
          , color Color.black900
          ]
        <> FontStyle.body10 TypoGraphy
    ]

getSingleTab ∷ Int
getSingleTab = ((screenWidth unit) - 32) / 3

sheetView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
sheetView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 16
    ]
    [ viewPager2
        [ width $ MATCH_PARENT
        , height $ V $ (getHeightFromPercent 85) - safeMarginTop - safeMarginBottom
        , listItem $ spy "state.holder" state.holderView
        , listDataV2 $ getListData state
        , scrollDirection HORIZONTAL
        ]
    ]

sheetViewHolder :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
sheetViewHolder push state =
  linearLayout
    [ width $ MATCH_PARENT
    , height $ WRAP_CONTENT
    , padding $ Padding 16 16 16 16
    , gravity CENTER
    ]
    [ linearLayout
        [ width $ MATCH_PARENT
        , height $ WRAP_CONTENT
        , background Color.white900
        , cornerRadius 12.0
        , orientation VERTICAL
        , padding $ Padding 16 16 16 16
        ]
        [ textView
            $ [ text "Pickup"
              , color Color.black500
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , textView
            $ [ textHolder "pickupDistance"
              , color Color.black800
              , margin $ MarginTop 5
              ]
            <> FontStyle.h1 TypoGraphy
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 1
            , background Color.grey900
            , margin $ MarginTop 16
            ]
            []
        , sourceAndDestinationView push state
        , priceView push state
        , textView
            $ [ text "Includes $10 Pickup "
              , margin $ MarginTop 12
              , color Color.black700
              ]
            <> FontStyle.paragraphText TypoGraphy
        , buttonsView push state
        ]
    ]

sourceAndDestinationView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
sourceAndDestinationView push state =
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 16 24
    ]
    [ sourceDestinationImageView VISIBLE
    , sourceDestinationTextView push state
    , destinantionDistanceView push state
    ]

sourceDestinationImageView :: forall w. Visibility -> Layout w
sourceDestinationImageView vis =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , visibility vis
    ]
    [ imageView
        [ height $ V 14
        , width $ V 14
        , margin $ MarginTop 4
        , imageUrl "ny_ic_source_dot"
        ]
    , SeparatorView.view separatorConfig
    , imageView
        [ height $ V 14
        , width $ V 14
        , imageUrl "ny_ic_destination"
        ]
    ]

addressView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> String -> Visibility -> Margin -> Layout w
addressView push state holder vis newMargin =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ newMargin
    , visibility vis
    ]
    [ textView
        $ [ textHolder $ holder <> "Area"
          , color Color.black800
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , textView
        $ [ textHolder $ holder <> "FullAddress"
          , color Color.black500
          , margin $ MarginTop 5
          ]
        <> FontStyle.body1 TypoGraphy
    , textView
        $ [ textHolder $ holder <> "Pincode"
          , color Color.black800
          , margin $ MarginTop 5
          ]
        <> FontStyle.body4 TypoGraphy
    ]

destinantionDistanceView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
destinantionDistanceView push state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ addressView push state "source" INVISIBLE (MarginBottom 16)
    , distanceView VISIBLE
    ]

sourceDestinationTextView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
sourceDestinationTextView push state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , margin $ MarginLeft 12
    ]
    [ sourceDestinationImageView INVISIBLE
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ addressView push state "source" VISIBLE (MarginBottom 16)
        , distanceView INVISIBLE
        , addressView push state "destination" VISIBLE (MarginTop 16)
        ]
    ]

distanceView :: forall w. Visibility -> Layout w
distanceView vis =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ Padding 8 6 8 6
    , background Color.grey700
    , stroke $ "1," <> Color.grey900
    , visibility vis
    , cornerRadius 8.0
    , gravity CENTER
    ]
    [ textView
        $ [ textHolder $ "tripDistance"
          , color Color.black900
          ]
        <> FontStyle.subHeading1 TypoGraphy
    ]

priceView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
priceView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 8 6 8 6
    , background Color.grey700
    , cornerRadius 8.0
    , gravity CENTER
    ]
    [ textView
        $ [ textHolder $ "tripPrice"
          , color Color.black900
          ]
        <> FontStyle.h1 TypoGraphy
    ]

buttonsView :: forall w. (Action -> Effect Unit) -> RideRequestPopUpScreenData -> Layout w
buttonsView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 20
    ]
    [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , stroke $ "1," <> Color.black700
        , padding $ Padding 16 20 16 20
        , cornerRadius 8.0
        , onClickHolder push Decline
        ]
        [ textView
            $ [ text "Decline"
              , color Color.black900
              ]
            <> FontStyle.h1 TypoGraphy
        ]
    , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , padding $ Padding 16 20 16 20
        , background Color.green900
        , cornerRadius 8.0
        , gravity CENTER
        , margin $ MarginLeft 12
        ]
        [ textView
            $ [ text "Accept"
              , color Color.white900
              ]
            <> FontStyle.h1 TypoGraphy
        ]
    ]

bottomLogoView :: forall w. RideRequestPopUpScreenData -> Layout w
bottomLogoView state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , background Color.purple
    , padding $ PaddingBottom safeMarginBottom
    ]
    [ imageView
        [ imageUrl "ny_ic_logo_name"
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , margin $ MarginTop 12
        ]
    ]

separatorConfig :: SeparatorView.Config
separatorConfig =
  { orientation: VERTICAL
  , count: 9
  , height: V 7
  , width: V 2
  , layoutWidth: V 14
  , layoutHeight: WRAP_CONTENT
  , color: Color.black500
  }

getRideRequest :: (Action -> Effect Unit) -> Flow OverlayData Unit
getRideRequest push = do
  eiResp <- nearBySearchRequest
  case eiResp of
    Right (NearBySearchRequestRes resp) -> do
      let
        _ = spy "Response ->" resp
      -- void $ modifyState \(OverlayData oState) -> OverlayData oState{rideRequestPopUpScreen{ holderData = toPopupProp resp.searchRequestsForDriver}}
      -- void $ rideRequestPopUp
      restart
    Left _ -> restart
  where
  restart = do
    delayViaTimer $ Milliseconds 1000.0
    getRideRequest push

getListData state =
  [ { tripPrice: toPropValue "hello"
    , tripDistance: toPropValue "hello"
    , pickupDistance: toPropValue "hello"
    , sourceArea: toPropValue "hello"
    , sourceFullAddress: toPropValue "hello"
    , sourcePincode: toPropValue "hello"
    , destinationArea: toPropValue "hello"
    , destinationFullAddress: toPropValue "hello"
    , destinationPincode: toPropValue "hello"
    }
  , { tripPrice: toPropValue "hello"
    , tripDistance: toPropValue "hello"
    , pickupDistance: toPropValue "hello"
    , sourceArea: toPropValue "hello"
    , sourceFullAddress: toPropValue "hello"
    , sourcePincode: toPropValue "hello"
    , destinationArea: toPropValue "hello"
    , destinationFullAddress: toPropValue "hello"
    , destinationPincode: toPropValue "hello"
    }
  , { tripPrice: toPropValue "hello"
    , tripDistance: toPropValue "hello"
    , pickupDistance: toPropValue "hello"
    , sourceArea: toPropValue "hello"
    , sourceFullAddress: toPropValue "hello"
    , sourcePincode: toPropValue "hello"
    , destinationArea: toPropValue "hello"
    , destinationFullAddress: toPropValue "hello"
    , destinationPincode: toPropValue "hello"
    }
  ]
