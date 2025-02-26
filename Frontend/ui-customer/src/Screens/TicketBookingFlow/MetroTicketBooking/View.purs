{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketBooking.View where

import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..), City(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.View as PrimaryEditText
import Helpers.Utils (fetchImage, FetchImageFrom(..), getMetroConfigFromAppConfig, CityMetroConfig(..), getMetroConfigFromCity, getDefaultPixelSize)
import Prelude
import Screens.TicketBookingFlow.MetroTicketBooking.Controller
import Screens.TicketBookingFlow.MetroTicketBooking.ComponentConfig
import Font.Style as FontStyle
import PrestoDOM.Animation as PrestoAnim
import Debug (spy)
import Engineering.Helpers.Commons as EHC
import Animation.Config
import JBridge as JB
import Data.Array
import Font.Size as FontSize
import Data.Maybe (maybe, fromMaybe, isJust)
import Effect.Aff (launchAff)
import Services.API
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import Types.App (GlobalState, defaultGlobalState, FlowBT)
import Screens.Types
import Services.Backend as Remote
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Data.Time.Duration (Milliseconds(..))
import Components.RequestInfoCard as InfoCard
import Language.Strings
import Language.Types
import Data.String as DS
import Data.Function.Uncurried (runFn1, runFn3)
import Data.Maybe(Maybe(..))
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.BackTrack (liftFlowBT, getState)
import Engineering.Helpers.Commons (liftFlow)
import Control.Monad.Trans.Class (lift)
import Storage 
import Mobility.Prelude (boolToVisibility, layoutWithWeight)
import Components.Banner as Banner
import ConfigProvider (getAppConfig)
import Constants
import MerchantConfig.Types (MetroConfig)
import Helpers.API (callApi)
import Data.Array as DA
import LocalStorage.Cache (getFromCache)
import Common.Animation.Config
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Engineering.Helpers.Utils (getCityFromString)

screen :: ST.MetroTicketBookingScreenState -> Screen Action ST.MetroTicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketBookingScreen"
  , globalEvents : [getQuotes]
  , eval : \action state -> do
        let _ = spy "MetroTicketBookingScreenState action " action
        let _ = spy "MetroTicketBookingScreenState state " state 
        eval action state
  }
  where
    getQuotes push = do
      let city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
        (FRFSConfigAPIRes fcResponse) <- Remote.getFRFSBookingConfigBT (show city)
        liftFlowBT $ push $ MetroBookingConfigAction (FRFSConfigAPIRes fcResponse)
          

        let config = getAppConfig appConfig
            bookingEndTime  = if elem (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "DD/MM/YYYY") fcResponse.customDates then fcResponse.customEndTime else (EHC.convertUTCtoISC fcResponse.bookingEndTime "HH:mm:ss")
            withinTimeRange = JB.withinTimeRange (EHC.convertUTCtoISC fcResponse.bookingStartTime "HH:mm:ss") bookingEndTime (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "HH:mm:ss")
        liftFlowBT $ push $ ShowMetroBookingTimeError withinTimeRange
        pure unit

      void $ launchAff $ EHC.flowRunner defaultGlobalState $ getQuotesPolling initialState.data.searchId 5 3000.0 initialState push GetMetroQuotesAction
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ getSDKPolling initialState.data.bookingId 3000.0 initialState push GetSDKPollingAC
      pure $ pure unit

getQuotesPolling :: forall action. String-> Int -> Number -> ST.MetroTicketBookingScreenState -> (action -> Effect Unit) -> (Array FrfsQuote -> action) -> Flow GlobalState Unit
getQuotesPolling searchId count delayDuration state push action = do
  if state.props.currentStage == GetMetroQuote && searchId /= "" then do
    if count > 0 then do
      (getMetroQuotesResp) <- Remote.frfsQuotes searchId
      case getMetroQuotesResp of
          Right (FrfsQuotesRes resp) -> do
            if (not (null resp)) then do
                doAff do liftEffect $ push $ action resp
            else do
              if (count == 1) then do
                doAff do liftEffect $ push $ action resp
              else do
                void $ delay $ Milliseconds delayDuration
                getQuotesPolling searchId (count - 1) delayDuration state push action
          Left _ -> pure unit
      else 
        pure unit
  else pure unit

getSDKPolling :: forall action. String -> Number -> ST.MetroTicketBookingScreenState -> (action -> Effect Unit) -> (CreateOrderRes -> action) -> FlowBT String Unit
getSDKPolling bookingId delayDuration state push action = do
  let localPoolingStatus = runFn3 getFromCache (show METRO_PAYMENT_SDK_POLLING) Nothing Just
      
  if state.props.currentStage == PaymentSDKPooling && localPoolingStatus == Just true then do
      (GetMetroBookingStatusResp (FRFSTicketBookingStatusAPIRes metroTicketStatusResp)) <- Remote.getMetroStatusBT bookingId 
      let orderResp = metroTicketStatusResp.payment >>= \(FRFSBookingPaymentAPI paymentInfo) -> paymentInfo.paymentOrder 
      case orderResp of
        Just (CreateOrderRes createOrderResp) -> do
          liftFlowBT $ push $ action (CreateOrderRes createOrderResp)
        Nothing -> do
          void $ lift $ lift $ delay $ Milliseconds delayDuration
          getSDKPolling bookingId delayDuration state push action
  else pure unit

view :: forall w . (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let
    city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
    resp@(FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
    cityMetroConfig = getMetroConfigFromCity city (Just resp) ""
    config = getAppConfig appConfig
    metroConfig = getMetroConfigFromAppConfig config (show city)
  in
    PrestoAnim.animationSet [Anim.fadeIn true]  $ frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , onBackPressed push $ const BackPressed
    ] $ 
    [ linearLayout 
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.grey700
        , orientation VERTICAL
        , visibility $ boolToVisibility (not state.props.showShimmer)
        ]
        [ headerView state push
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.greySmoke
          , visibility $ boolToVisibility (not state.props.showShimmer)
          ][]
        , scrollView[
            height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.white900
          ][
            linearLayout[
              height WRAP_CONTENT
            , width MATCH_PARENT
            ][
              infoSelectioView state push city cityMetroConfig metroConfig
            , offerSelectionView push state
            ]
          ]
        ]
        , updateButtonView state push
    ] <> if state.props.showMetroBookingTimeError && not state.props.showShimmer then [InfoCard.view (push <<< InfoCardAC) (metroTimeErrorPopupConfig state cityMetroConfig)] else [linearLayout [visibility GONE] []]
      <> if state.props.showShimmer
            then [shimmerView state]
            else [linearLayout [visibility GONE] []]
      -- <> if state.props.currentStage == OfferSelection then [offerSelectionView push state] else [linearLayout [visibility GONE] []]

routeListView :: forall w. ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
routeListView state push = 
  PrestoAnim.animationSet ([] <>
    if EHC.os == "IOS" then
      [ Anim.fadeIn state.props.routeList 
      , Anim.fadeOut $ not state.props.routeList ]
    else
      [Anim.listExpandingAnimation $ listExpandingAnimationConfig state.props.routeList])
   $ 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation VERTICAL
    , margin $ Margin 0 5 0 5
    , visibility $ boolToVisibility $ (state.props.routeList && not (DA.null state.data.routeList))
    , stroke ("1," <> Color.borderColorLight)
    , cornerRadius 5.0
    , onAnimationEnd push $ const ListExpandAinmationEnd
    ](DA.mapWithIndex (\index (FRFSRouteAPI route) ->
        let
          routeCode = route.code
          routeName = route.shortName
          routeTotalStops = fromMaybe 0 route.totalStops
        in
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 15 16 10
        , onClick push $ const $ SelectRoutes routeCode routeName 
        , orientation VERTICAL
        ][  
          linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            ][  
              textView $
                [ accessibilityHint $ routeName <> " : Button"
                , textFromHtml routeName 
                , color Color.darkCharcoal
                , weight 1.0
                ] <> FontStyle.paragraphText LanguageStyle
          ,   textView $ 
                [
                  text $ (show routeTotalStops)<> " stops"
                ,  gravity RIGHT
                , color Color.darkCharcoal
                , visibility GONE
                , weight 0.0
                ]
            ]
          , linearLayout
              [ height $ V 1
              , width MATCH_PARENT
              , background Color.grey900
              , visibility if index == DA.length (state.data.routeList) - 1  then GONE else VISIBLE
              , margin $ Margin 0 20 0 0
              ][]
          ]) state.data.routeList)




infoSelectioView :: forall w . ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> City -> CityMetroConfig -> MetroConfig -> PrestoDOM (Effect Unit) w
infoSelectioView state push city cityMetroConfig metroConfig =
  let
    isBusTicketService = state.props.ticketServiceType == BUS
    showRouteList = DA.null state.data.routeList
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT    
      , padding $ PaddingBottom 75
      , scrollBarY false
      , background Color.white900
      , visibility $ boolToVisibility $ (not state.props.showShimmer) && (state.props.currentStage /= ST.OfferSelection)
      ] [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          [ bannerView push cityMetroConfig state
          , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , padding (Padding 20 20 20 20)
                    , orientation VERTICAL
                    ][ 
                      textView $ [
                        text $ getString PLAN_YOUR_JOURNEY
                      , color Color.black800  
                      , margin $ MarginBottom 9 
                      , visibility $ boolToVisibility (not isBusTicketService)
                      ] <> FontStyle.subHeading1 TypoGraphy
                    , linearLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , orientation VERTICAL
                      , visibility $ boolToVisibility $ isBusTicketService
                      ]
                      [ 
                         linearLayout
                          [ width MATCH_PARENT
                          , height WRAP_CONTENT
                          , orientation HORIZONTAL
                          , gravity CENTER_VERTICAL
                          --  , margin (MarginTop 8)
                          ][  textView
                              ([ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , text $ getString ROUTE_BUS_NO
                              , color Color.greyTextColor
                              , margin (MarginVertical 10 10)
                              ] <> FontStyle.body2 TypoGraphy)
                            ] 
                        , linearLayout
                              [ width MATCH_PARENT
                              , height WRAP_CONTENT
                              , orientation HORIZONTAL
                              , stroke ("1," <> Color.borderColorLight) 
                              , cornerRadius 4.0
                              , gravity CENTER_VERTICAL
                              , onClick push $ const $ SelectRouteslistView
                              
                              ][  textView $
                                  [ width MATCH_PARENT
                                  , height WRAP_CONTENT
                                  , padding (Padding 19 17 0 17)
                                  , color Color.greyTextColor
                                  , textFromHtml $ if state.props.routeName == "" then getString SELECT_ROUTE_NUMBER else  state.props.routeName                                            
                                  , weight 4.0
                                  , cornerRadius 6.0
                                  , stroke ("3," <> Color.white900)
                                  , alpha if state.props.routeName == "" then 0.2 else 1.0
                                  ] <> FontStyle.subHeading2 TypoGraphy
                                  , linearLayout
                                    [ width WRAP_CONTENT
                                    , height WRAP_CONTENT
                                    , gravity CENTER_VERTICAL
                                    , visibility $ boolToVisibility $ DA.length state.data.routeList > 1 || (state.props.routeName == "")
                                    ][ imageView
                                        [ width (V 20)
                                        , height (V 20)
                                        , margin $ MarginRight 10
                                        , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.props.routeList then "ny_ic_chevron_up_dark" else "ny_ic_chevron_down_light"
                                        ]
                                    ]
                                  ]
                            , routeListView state push
                      ]
                    , textView
                            ([ width WRAP_CONTENT
                            , height WRAP_CONTENT
                            , text $ getString PICKUP_AND_DESTINATION_STOP
                            , color Color.greyTextColor
                            -- , margin (MarginVertical 10 10)
                            , margin $ if state.props.ticketServiceType == BUS then  MarginTop 35 else MarginTop 5
                            ] <> FontStyle.body2 TypoGraphy)
                    , locationSelectionView push state
                    , incrementDecrementView push state metroConfig isBusTicketService
                    , limitReachedView push state
                    , offerInfoView push state
                    , roundTripCheckBox push state metroConfig
                    , linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , margin $ MarginTop 10
                        , visibility $ boolToVisibility $ city /= Delhi
                            ][ textView $ 
                                [ text $ getString UNCERTAIN_ABOUT_METRO_ROUTES
                                , color Color.black800
                                , visibility $ boolToVisibility (state.props.ticketServiceType /= BUS)
                                ] <> FontStyle.body1 TypoGraphy
                              , textView $ 
                                [ text $ " " <> (getString SEE_MAP) 
                                , color Color.blue900
                                , rippleColor Color.rippleShade
                                , visibility $ boolToVisibility (state.props.ticketServiceType /= BUS)
                                , onClick push $ const MetroRouteMapAction
                                ] <> FontStyle.subHeading1 TypoGraphy
                            ]
                    , termsAndConditionsView push cityMetroConfig true
                    -- , paymentSummaryView push state -- TODO : make payment summary view - @ashishsingh101
                    ]
    ]
  ]

bannerView :: forall w . (Action -> Effect Unit) ->  CityMetroConfig -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
bannerView push cityMetroConfig state = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , cornerRadius 8.0  
  , margin $ (Margin 16 0 16 0)
  , visibility $ boolToVisibility $ (state.props.ticketServiceType /= BUS) && state.props.currentStage /= ST.OfferSelection
  ][ Banner.view (push <<< const NoAction) (metroBannerConfig cityMetroConfig state)
  ]

roundTripCheckBox :: (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> MetroConfig -> forall w. PrestoDOM (Effect Unit) w
roundTripCheckBox push state metroConfig =
  let
    isRoundTripSelected = ST.ROUND_TRIP_TICKET == state.data.ticketType
    ticketType = if isRoundTripSelected then ST.ONE_WAY_TICKET else ST.ROUND_TRIP_TICKET
    city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
  in
    if city == Delhi
    then linearLayout [] []
    else 
      linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , padding $ Padding 4 4 4 4
      , margin $ MarginTop 20
      , visibility $ boolToVisibility (state.props.ticketServiceType /= BUS)
      ][linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT 
        , onClick (\action -> push action) (const $ ChangeTicketTab ticketType metroConfig)
      ][imageView
        [ height $ V 16
        , width $ V 16
        , layoutGravity "center_vertical"
        , margin $ MarginRight 4
        , imageWithFallback $ fetchImage FF_COMMON_ASSET (if isRoundTripSelected then "ny_ic_checked" else "ny_ic_unchecked")
        ]
        , textView $ 
         [ text $ " " <> (getString BOOK_ROUND_TRIP)
         , color Color.black800
         ] <> FontStyle.body2 TypoGraphy
       ]
      ]

selectionTab :: forall w . String  -> ST.TicketType -> (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> MetroConfig -> PrestoDOM (Effect Unit) w
selectionTab _text ticketType push state metoConfig = 
  let ticketEnabled = ticketType == state.data.ticketType
  in
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text _text
  , background if ticketEnabled then Color.black900 else Color.white900
  , padding (Padding 8 8 8 8)
  , weight 1.0
  , gravity CENTER
  , color if ticketEnabled then Color.white900 else Color.black900
  , cornerRadius if EHC.os == "IOS" then 16.0 else 20.0
  , textSize FontSize.a_14
  , onClick (\action ->
              if state.data.ticketType /= ticketType then do
                _ <- push action
                pure unit
              else pure unit
            ) (const $ ChangeTicketTab ticketType metoConfig)
  ]

paymentSummaryView :: (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> forall w . PrestoDOM (Effect Unit) w
paymentSummaryView push state = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , background Color.grey700
  , cornerRadius 12.0
  , padding $ Padding 16 20 16 20
  , margin $ MarginTop 12
  ][
   ]

termsAndConditionsView :: forall w . (Action -> Effect Unit) -> CityMetroConfig -> Boolean -> PrestoDOM (Effect Unit) w
termsAndConditionsView push (CityMetroConfig cityMetroConfig) isMarginTop = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , background Color.grey700
  , cornerRadius 12.0
  , padding $ Padding 12 12 12 12
  , margin $ MarginTop 24
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , margin $ MarginBottom 12
     , visibility GONE -- temp disable
     ][ imageView
        [ height $ V 16
        , width $ V 16 
        , layoutGravity "center_vertical"
        , margin $ MarginRight 6
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info"
        ]
        , textView $ 
          [ textFromHtml $ " " <> getString BY_PROCEEDING_YOU_AGREE <> "<span style='color:#0066FF'> " <> 
              (getString TERMS_AND_CONDITIONS_FULL) <> "</span>"
          , onClick (\action -> do
                  _<- push action
                  _ <- JB.openUrlInApp $ cityMetroConfig.termsAndConditionsUrl
                  pure unit
                  ) (const NoAction)
          ] <> FontStyle.body1 TypoGraphy
     ]
     , linearLayout
       [ width MATCH_PARENT
       , height WRAP_CONTENT
       , orientation VERTICAL
       , margin $ MarginLeft 4
       ] (mapWithIndex (\index item ->
           linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , visibility $ boolToVisibility (item /= "")
           ][ 
                textView $
                  [ textFromHtml $ "• " <> item
                  , color Color.black700
                  ] <> FontStyle.paragraphText TypoGraphy
                    <> if EHC.os == "IOS" then [width MATCH_PARENT, height WRAP_CONTENT] else []
              ]
       ) cityMetroConfig.termsAndConditions )
  ]

headerView :: forall w. ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding (PaddingTop EHC.safeMarginTop)
    , background Color.white900
    ][  GenericHeader.view (push <<< GenericHeaderAC) (metroTicketBookingHeaderConfig state)
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT
        , background Color.white900
        ][ linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          , padding $ PaddingRight 16
          , visibility $ boolToVisibility $ state.props.ticketServiceType /= BUS
          , background Color.white900
          ][ textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString MY_TICKETS
              , accessibilityHint $ "My Tickets : Button"
              , accessibility ENABLE
              , rippleColor Color.rippleShade
              , color Color.blueTextColor
              , onClick push (const $ MyMetroTicketAction)
              ] <> FontStyle.subHeading1 LanguageStyle
            ]
          ]
      ]

incrementDecrementView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> MetroConfig -> Boolean -> PrestoDOM (Effect Unit) w
incrementDecrementView push state metroConfig busClicked=
  let (FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
      ticketLimit = if state.data.ticketType == ST.ROUND_TRIP_TICKET then metroBookingConfigResp.roundTripTicketLimit else metroBookingConfigResp.oneWayTicketLimit
      limitReached = (state.data.ticketType == ST.ROUND_TRIP_TICKET && state.data.ticketCount >= ticketLimit) || (state.data.ticketType == ST.ONE_WAY_TICKET && state.data.ticketCount >= ticketLimit) || (busClicked)
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 8.0
    , orientation HORIZONTAL
    , margin $ MarginTop 40
    ][  linearLayout  
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          ][ imageView
              [ height $ V 24  
              , width $ V 24  
              , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_passengers"
              , margin $ MarginRight 8  
              ]
            , textView $
              [ text $ getString NO_OF_PASSENGERS
              , color Color.black800
              , margin $ MarginBottom 8
              ] <> FontStyle.subHeading1 TypoGraphy
          ]
        , linearLayout 
          [ weight 1.0][]
        , linearLayout 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          -- , gravity RIGHT
          ][  imageView  
              [ background Color.grey700
              , height $ V 30 
              , width $ V 30  
              , imageWithFallback $ fetchImage COMMON_ASSET if state.data.ticketCount < 2 then "ny_ic_disable_decrement" else "ny_ic_decrement"
              , rippleColor Color.rippleShade
              , cornerRadius 50.0  
              , onClick push $ const (DecrementTicket)
              ] 
            , textView $
              [ background Color.white900
              , text $ show state.data.ticketCount
              , height WRAP_CONTENT
              , color Color.black800
              -- , weight 1.0
              , gravity CENTER
              , padding $ Padding 14 4 10 0
              ] 
            , imageView  
              [ background Color.black900
              , height $ V 30 
              , width $ V 30  
              , imageWithFallback $ fetchImage COMMON_ASSET if metroBookingConfigResp.oneWayTicketLimit == state.data.ticketCount then "ny_ic_disable_increment" else "ny_ic_increment"
              , rippleColor Color.rippleShade
              , cornerRadius 50.0  
              , margin $ MarginLeft 10
              , onClick push $ const (IncrementTicket )
              ]
          ]
      ]

limitReachedView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
limitReachedView push state = 
  let (FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
      ticketLimit = if state.data.ticketType == ST.ROUND_TRIP_TICKET then metroBookingConfigResp.roundTripTicketLimit else metroBookingConfigResp.oneWayTicketLimit
      limitReached = (state.data.ticketType == ST.ROUND_TRIP_TICKET && state.data.ticketCount >= ticketLimit) || (state.data.ticketType == ST.ONE_WAY_TICKET && state.data.ticketCount >= ticketLimit)
  in 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , visibility $ boolToVisibility  $ limitReached
  , margin $ MarginTop 6
  ][  imageView $
      [ width $ V 16
      , height $ V 16
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey" 
      , layoutGravity "center_vertical"
      , visibility $ boolToVisibility ( state.props.ticketServiceType /= BUS)
      , margin $ MarginRight 4
      ]
    , textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ " " <> (getString MAXIMUM) <> " " <> (show ticketLimit) <> " " <> (getString TICKETS_ALLOWED_PER_USER)
      , color Color.black700
      , gravity LEFT
      , singleLine true
      -- , visibility $ boolToVisibility (state.props.ticketServiceType /= BUS)
      , alpha 1.0
      ]  <> FontStyle.paragraphText TypoGraphy
  ]



updateButtonView :: forall w. ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateButtonView state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , alignParentBottom "true,-1"
  , background Color.transparent
  , visibility $ boolToVisibility $ state.props.currentStage /= ST.OfferSelection && (not state.props.showShimmer)
  , orientation VERTICAL
  ]
  [ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ offersInfoView push state
    , linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , padding $ PaddingVertical 5 24
      , stroke ("1," <> Color.borderColorLight)
      ][PrimaryButton.view (push <<< UpdateButtonAction) (updateButtonConfig state)]
    ]
  ]

locationSelectionView :: (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState ->  forall w. PrestoDOM (Effect Unit) w
locationSelectionView push state =
  relativeLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ]
  [
    linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , cornerRadius 8.0
  , stroke ("1," <> Color.borderColorLight)
  , orientation VERTICAL
  , margin $  MarginTop 5
  , gravity CENTER
  ]
  [srcTextView push state
  , linearLayout[
      height $ V 1
    , width MATCH_PARENT
    , margin $ MarginHorizontal 20 40
    , background Color.borderColorLight
    ][ ]
  , destTextView push state
]
  , linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
    ][ layoutWithWeight, 
        imageView $ 
       [ height $ V 32
       , width $ V 32
       , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_src_dest_edit"
       , cornerRadius 10.0 
       , margin $ Margin 0 46 16 0
       , visibility $ boolToVisibility (not state.props.isRepeatRide)
       , onClick push $ const (SelectLocation Src)
       , alignParentRight "true,-1"
       , gravity RIGHT
       ]
    ]
]
srcTextView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
srcTextView push state = textViewForLocation (getString FROM) Src push state

destTextView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
destTextView push state = textViewForLocation (getString TO) Dest push state

textViewForLocation :: forall w. String -> ST.LocationActionId -> (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
textViewForLocation label actionId push state =
  let 
    fieldConfig = case actionId of
      Src -> do
        let fieldValue = if (DS.null state.data.srcLoc) then (getString STARTING_FROM) <> "?" else state.data.srcLoc
            alpha = if (DS.null state.data.srcLoc) then 0.5 else 1.0
        {fieldText : fieldValue, alphaValue : alpha}
      Dest -> do
        let fieldValue = if (DS.null state.data.destLoc) then (getString WHERE_TO) else state.data.destLoc
            alpha = if (DS.null state.data.destLoc) then 0.5 else 1.0
        {fieldText : fieldValue, alphaValue : alpha}
  in
    linearLayout 
    [ height $ V 54
    , width MATCH_PARENT
    , background Color.white900
    , gravity CENTER_VERTICAL
    , rippleColor Color.rippleShade
    , cornerRadius 8.0
    , onClick push $ const (SelectLocation actionId)
    ]
    [ imageView $ 
            [ height $ V 18
            , width $ V 18
            , imageWithFallback $ fetchImage COMMON_ASSET (if actionId == Src then "ny_ic_pickup_green_indicator" else "ny_ic_drop_red_indicator")
            , cornerRadius 4.0 
            , margin $ MarginLeft 8
            ]
    , textView $ 
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , text $ fieldConfig.fieldText
        , color Color.black800
        , gravity CENTER_VERTICAL
        , singleLine true
        , ellipsize true
        , margin $ MarginHorizontal 10 10
        , alpha fieldConfig.alphaValue
        ] <> (FontStyle.getFontStyle FontStyle.SubHeading1 LanguageStyle)
        
    ]
    
shimmerView :: forall w. ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , visibility if state.props.showShimmer then VISIBLE else GONE
    ]
    [ sfl (V (getHeight 15)) (getHeight 8)  1 true
    , sfl (V (getHeight 40)) (getHeight 8)  1 true
    , linearLayout [weight 1.0][]
    , sfl (V (getHeight 15)) 0  1 true
    ]
  where 
    getHeight percent = getDefaultPixelSize $ JB.getHeightFromPercent percent


sfl :: forall w. Length -> Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
sfl height' marginTop numberOfBoxes visibility' =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop marginTop
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        ( map
            ( \_ ->
                linearLayout
                  [ height height'
                  , background Color.greyDark
                  , cornerRadius 12.0
                  , weight 1.0
                  , stroke $ "1," <> Color.grey900
                  , margin $ Margin 4 4 4 4
                  ]
                  []
            )
            (1 DA... numberOfBoxes)
        )
    ]

offerInfoView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
offerInfoView push state = 
  textView $
  [ text offerTextConfig.text
  , color offerTextConfig.color
  , visibility $ boolToVisibility (offerTextConfig.visibility && (state.props.ticketServiceType /= BUS))
  -- , visibility offerTextConfig.visibility 
  , margin $ Margin 4 8 0 0
  ]  <> FontStyle.paragraphText TypoGraphy
  where
    (FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
    ticketsBookedInEvent = fromMaybe 0 metroBookingConfigResp.ticketsBookedInEvent
    freeTicketInterval = fromMaybe 9999 metroBookingConfigResp.freeTicketInterval
    freeTicketCount = ((ticketsBookedInEvent + state.data.ticketCount) `div` freeTicketInterval) - (ticketsBookedInEvent `div` freeTicketInterval)
    ticketsAfterLastFreeTicket = ((ticketsBookedInEvent + state.data.ticketCount) `mod` freeTicketInterval)
    offerTextConfig = getOfferTextConfig freeTicketCount freeTicketInterval ticketsAfterLastFreeTicket (FRFSConfigAPIRes metroBookingConfigResp)

    getOfferTextConfig :: Int -> Int -> Int -> FRFSConfigAPIRes -> {color :: String, text :: String, visibility :: Boolean}
    getOfferTextConfig freeTicketCount freeTicketInterval ticketsAfterLastFreeTicket (FRFSConfigAPIRes metroBookingConfigResp) = do
      if freeTicketCount > 0 then {color : Color.green900, text : (getString $ FREE_TICKET_AVAILABLE (show $ freeTicketCount * (fromMaybe 0 metroBookingConfigResp.maxFreeTicketCashback)) (show freeTicketCount)), visibility : true}
        else if ticketsAfterLastFreeTicket == (freeTicketInterval - 1) then {color : Color.metroBlue, text : (getString NEXT_FREE_TICKET), visibility : true} 
        else {color : Color.transparent, text : "", visibility : false}

offersInfoView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
offersInfoView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.cosmicLatte
  , padding $ Padding 16 12 16 12
  , gravity CENTER_VERTICAL
  , visibility $ boolToVisibility $ (DA.length (state.data.discounts) > 0) || (DA.length appliedDiscountCodes > 0)
  -- , alignParentBottom "true,-1"
  ]
  [ imageView
    [ height $ V 12
    , width $ V 12
    , gravity CENTER_VERTICAL
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_discount_percentage"
    ]
  , textView $
    [ text $ offerText
    , color Color.black900
    , margin $ MarginLeft 8
    ] <> FontStyle.tags TypoGraphy
  , linearLayout
    [weight 1.0][]
  , textView $
    [ text "Check"
    , padding $ Padding 4 2 4 2
    , color Color.blue900
    , gravity RIGHT
    , onClick push $ const OfferInfoClicked
    , rippleColor Color.rippleShade
    ] <> FontStyle.tags TypoGraphy
  ]
  where
    appliedDiscountCodes :: Array String
    appliedDiscountCodes = maybe [] extractDiscountCodes state.data.applyDiscounts

    extractDiscountCodes :: Array FRFSDiscountReq -> Array String
    extractDiscountCodes discounts = map (\(FRFSDiscountReq discountItem) -> discountItem.code) discounts
    
    offerText :: String
    offerText = case DA.find (\discount -> discount.code == (fromMaybe "" $ DA.head appliedDiscountCodes)) state.data.discounts of
      Just discount -> discount.title <> " Applied ✨"
      _ -> (show $ DA.length state.data.discounts) <> " Offers Available ✨"

offerSelectionView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
offerSelectionView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT    
  , padding $ PaddingBottom 75
  , scrollBarY false
  , visibility $ boolToVisibility $ (not state.props.showShimmer) && state.props.currentStage == ST.OfferSelection
  , background Color.white900
  ] 
  [ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] $
    [ textView $ 
      [ text "For Verification you have to enter Aadhaar details"
      , background Color.blue600
      , padding $ Padding 16 12 16 12
      , color Color.black700 
      , width MATCH_PARENT
      ] <> FontStyle.body3 TypoGraphy
    ] <> map (\discount -> offerCardView push state discount) state.data.discounts
  ]

offerCardView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> FRFSDiscountRes -> PrestoDOM (Effect Unit) w
offerCardView push state discount =
  -- let 
  --   allTexts = 
  --     case offerCardType of
  --       ST.Ladki -> {title1 : "40% OFF", title2 : "for Women Passenger", title3 : ""}
  --       ST.Buddha -> {title1 : "50% OFF", title2 : "for Senior Citizen", title3 : "Passenger age should be above 60 years"}
  -- in
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 8.0
    , stroke ("1," <> Color.borderColorLight)
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    , padding $ Padding 16 16 16 16
    , alpha if discount.eligibility == true then 1.0 else 0.5
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $
          [ text discount.title
          , color Color.black800
          , gravity CENTER
          -- , padding $ Padding 8 8 8 8
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text discount.description
          , color Color.black800
          , gravity CENTER
          -- , padding $ Padding 8 8 8 8
          ] <> FontStyle.subHeading1 TypoGraphy
        ]
      , linearLayout [weight 1.0] []
      , textView $
        [ text "Apply"
        , cornerRadius 20.0
        , padding $ Padding 12 6 12 6
        , color Color.blue800
        , stroke $ "1," <> Color.blue800
        , onClick push $ const $ if discount.eligibility == true then ApplyOffer discount.code else NoAction
        , rippleColor Color.rippleShade
        ] <> FontStyle.body27 TypoGraphy
      ]
    , textView $
      [ textFromHtml discount.tnc
      , color Color.black800
      , gravity CENTER
      , visibility $ boolToVisibility $ discount.tnc /= ""
      , margin $ MarginTop 8
      -- , padding $ Padding 8 8 8 8
      ] <> FontStyle.body3 TypoGraphy
    ]