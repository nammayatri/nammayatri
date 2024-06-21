module Screens.CustomerUtils.FavouriteDriverTrips.View where

import Prelude

import Common.Types.App
import Screens.CustomerUtils.FavouriteDriverTrips.ComponentConfig

import Animation as Anim
import Animation as Anim
import Components.GenericHeader as GenericHeader
import Components.SourceToDestination as SourceToDestination
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.BackTrack (liftFlowBT)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getVehicleVariantImage, getVariantRideType, getCityConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (capitalize)
import Prelude ((<>), show)
import Prelude (Unit, const, map, unit, ($), (&&), (/=), (<<<), (<=), (<>), (==), (/), not, (-), (||))
import PrestoDOM (Accessiblity(..), FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), accessibility, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, background, color, cornerRadius, disableClickFeedback, editText, fontStyle, frameLayout, gravity, height, hint, hintColor, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, multiLineEditText, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd, alpha, textFromHtml)
import Screens.CustomerUtils.FavouriteDriverTrips.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Effect.Aff (launchAff)
import Services.API (FavouriteDriverTripsResp(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.Utils as EHU
import Services.FlowCache as FlowCache
import Types.App (GlobalState(..), defaultGlobalState, FlowBT)
import Helpers.Pooling (delay)
import Effect.Aff (Milliseconds(..))

screen :: ST.FavouriteDriverTripsState -> GlobalState -> Screen Action ST.FavouriteDriverTripsState ScreenOutput
screen initialState st =
  { initialState
  , view
  , name : "FavouriteDriverTrips"
  , globalEvents : [
    (\push -> do
        _ <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ getFavouriteDriverTrips GetFavouriteDriversTripsAPIResponseAction push initialState
        pure $ pure unit
      )
    ]
  , eval : \state  action -> do
      let _ = spy  "FavouriteDriverTrips action " state
      let _ = spy  "FavouriteDriverTrips state " action
      eval state action
  }

getFavouriteDriverTrips :: forall action. (FavouriteDriverTripsResp -> action) -> (action -> Effect Unit) -> ST.FavouriteDriverTripsState -> FlowBT String Unit
getFavouriteDriverTrips action push state = do
  void $ lift $ lift $ EHU.toggleLoader true
  (FavouriteDriverTripsResp favouriteDriverResp ) <- FlowCache.updateAndFetchFavouriteDriverTrips "" "" "" "" 
  void $ lift $ lift $ delay $ Milliseconds 2000.0
  void $ lift $ lift $ EHU.toggleLoader false
  liftFlowBT $ push $ action ( FavouriteDriverTripsResp favouriteDriverResp)
  pure unit

view :: forall w. (Action -> Effect Unit) -> ST.FavouriteDriverTripsState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $ linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  ][ 
      header push state
    , linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.greySmoke
      ] []
    , driverList push state 
    , removeFavourite push state 
  ]

header :: forall w. (Action -> Effect Unit) -> ST.FavouriteDriverTripsState -> PrestoDOM (Effect Unit) w
header push state =
  linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ][
      GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
    , linearLayout[
      height MATCH_PARENT
    , width MATCH_PARENT
    ][
       textView $ 
        [ text "Know Your Driver"
        , color Color.blue900
        , weight 1.0
        , gravity CENTER
        , margin $ MarginTop 20
        ] <> FontStyle.body1 TypoGraphy
    ]
    ]

driverList :: forall w. (Action -> Effect Unit) -> ST.FavouriteDriverTripsState -> PrestoDOM (Effect Unit) w
driverList push state =
  scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , weight 1.0
    ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding $ PaddingVertical 16 16
      , gravity CENTER_VERTICAL
      ]
        (map
          (\item -> linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ] [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ] [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , margin $ Margin 15 10 15 10
                , background Color.blue600
                , padding $ Padding 16 16 16 16
                , cornerRadius 8.0
                ] [ 
                  
                  linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation HORIZONTAL
                    , gravity CENTER_VERTICAL
                    ] [ imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ic_vehicle_side" 
                        , width $ V 35
                        , visibility VISIBLE
                        , height $ V 35
                        ]
                      , linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , orientation VERTICAL
                        , margin $ MarginLeft 10
                        ] [ linearLayout
                            [ height WRAP_CONTENT
                            , width WRAP_CONTENT
                            , orientation HORIZONTAL
                            , gravity CENTER_VERTICAL
                            ] [ textView $
                                [ text "Last trip on "
                                , accessibilityHint $ "date : " <> (EHC.convertUTCtoISC (fromMaybe "" item.startTime) "DD:MM:YYYY")
                                , accessibility ENABLE
                                , color Color.darkCharcoal
                                ] <> FontStyle.body1 LanguageStyle
                              , textView $
                                [ text (EHC.convertUTCtoISC (fromMaybe "" item.startTime) "DD:MM:YYYY")
                                , color Color.darkCharcoal
                                ] <> FontStyle.body1 LanguageStyle
                                ]
                              , textView $
                                [ text $ capitalize $ DS.toLower (EHC.convertUTCtoISC (fromMaybe "" item.startTime) "hh:mm A")
                                , accessibilityHint $ "date : " <> (fromMaybe "" item.startTime)
                                , accessibility ENABLE
                                , color Color.greyShade
                                , margin $ MarginTop 4
                                ] <> FontStyle.body3 LanguageStyle
                            ]
                        , linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , gravity RIGHT
                          , orientation VERTICAL
                          ] [ textView $
                              [ text $ "₹"<>show (fromMaybe 0 item.totalFare)
                              , accessibilityHint $  ( DS.replaceAll (DS.Pattern "₹") (DS.Replacement "") $ show (fromMaybe 0 item.totalFare)) <> "Rupees"
                              , accessibility ENABLE
                              , color Color.black
                              ] <> FontStyle.h3 LanguageStyle
                          , textView $
                            [ text $ "Paid By Cash"
                            , color Color.greyShade
                            , accessibility DISABLE
                            ] <> FontStyle.captions LanguageStyle
                          ]
                      ]

                    , linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , margin $ MarginVertical 16  16
                      , background Color.grey800
                      ] []


                    , SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig item)

                    , linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , margin $ MarginVertical 16  8
                      , background Color.grey800
                      ] []


                    , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , gravity CENTER_VERTICAL
                      , orientation HORIZONTAL
                      , visibility VISIBLE
                      ] [ textView $ 
                          [ text $ getString YOU_RATED <> ":   "
                          , accessibilityHint $ "You Rated " <> (show item.rideRating) <> " Stars"
                          , accessibility ENABLE
                          , color Color.greyDavy
                          ] <> FontStyle.tags LanguageStyle

                        , imageView
                          [ width $ V 22
                          , height $ V 22
                          , margin $ MarginTop 1
                          , imageWithFallback $ fetchImage FF_COMMON_ASSET if item.rideRating == Just 5 then "ic_greenthumbs_up" else "ic_redthumbs_down"
                          ]
                        ]

                  ]
                ] 
            ]) state.data.details)
      ]

removeFavourite :: forall w. (Action -> Effect Unit) -> ST.FavouriteDriverTripsState -> PrestoDOM (Effect Unit) w
removeFavourite push state =
  linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , onClick push $ const RemoveFav 
    , margin $ Margin 0 7 0 35
    ][
      textView $ 
        [ textFromHtml $ "<u>" <> "Remove From Favourites" <> "</u>"
        , accessibilityHint $ "Remove From Favourites"
        , accessibility ENABLE
        , color Color.black700
        ] <> FontStyle.body2 LanguageStyle
    ]