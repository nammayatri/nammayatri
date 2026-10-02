module Screens.DriverProfileScreenCommon.View where

import Prelude
import Screens.DriverProfileScreenCommon.Controller (Action(..), eval, ScreenOutput, getVariant, getPillText, getAspirationsText)
import Screens.DriverProfileScreenCommon.ScreenData (DriverProfileScreenCommonState(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PrestoDOM 
import Effect (Effect)
import Animation as Anim
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App as CT
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Data.Array as DA
import Data.String (joinWith)
import Data.Maybe (fromMaybe, Maybe(..))
import Mobility.Prelude
import Services.Common.Backend as SB
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Effect.Aff (launchAff)
import Data.Time.Duration (Milliseconds(..))
import Types.App (defaultGlobalState, FlowBT, ScreenType(..))
import JBridge as JB
import Control.Monad.Except.Trans (runExceptT)
import Services.Common.API
import Control.Transformers.Back.Trans (runBackT)
import Control.Monad.Trans.Class (lift)
import Data.Either(Either(..))
import PrestoDOM.Animation 
import Engineering.Helpers.BackTrack (liftFlowBT)
import Presto.Core.Types.Language.Flow (delay, fork, await)
import Effect.Uncurried(runEffectFn1)
import Debug (spy)
import Data.Int (fromNumber, round, fromString)
import Engineering.Helpers.Commons
import Language.Strings (getString)
import Font.Size as FontSize
import PrestoDOM.Properties (lineHeight, cornerRadii, ellipsize)
import Font.Style (bold, semiBold, regular)
import Common.Types.App (LazyCheck(..))
import PrestoDOM (FontWeight(..), fontStyle, lineHeight, textSize, fontWeight, textFromHtml)
import Language.Types (STR(..))
import Common.Resources.Constants (secondsInOneYear)
import Data.Function.Uncurried (runFn2)

screen :: DriverProfileScreenCommonState -> Screen Action DriverProfileScreenCommonState ScreenOutput
screen initialState =
    { initialState
    , view: view
    , name: "DriverProfileScreenCommon"
    , globalEvents : [
    (\push -> do
       _ <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ getDriverProfile push initialState
       _ <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ getDriverProfileWithImages push initialState
       pure $ pure unit
      )
    ]
    , eval:
        \action state -> do
            let _ = spy "DriverProfileScreen state " state
            let _ = spy "DriverProfileScreen action " action
            eval action state
    }

getDriverProfileWithImages :: forall action. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> FlowBT String Unit
getDriverProfileWithImages push state = do
    (driversProfileRespWithImage) <- lift $ lift $ if state.props.rideId == "" 
    then SB.getDriverProfileById state.props.driverId true
    else SB.getDriverProfile state.props.rideId true
    case driversProfileRespWithImage of
            Right resp -> do
                liftFlowBT $ push $ GetDriverProfileAPIResponseAction resp
                pure unit
            Left _ -> do
                void $ pure $ EHU.showToast $ getString NOT_AVAILABLE
                liftFlowBT $ push $ GoBack
                pure unit

getDriverProfile :: forall action. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> FlowBT String Unit
getDriverProfile push state = do
  void $ lift $ lift $ EHU.loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
  void $ lift $ lift $ EHU.toggleLoader true
  driversProfileResponse <- lift $ lift $ fork $ do
    (driversProfileResp) <- if state.props.rideId == "" 
        then SB.getDriverProfileById state.props.driverId false
        else SB.getDriverProfile state.props.rideId false
    case driversProfileResp of
        Right resp -> do
            EHC.liftFlow $ push $ GetDriverProfileAPIResponseAction resp
            pure unit
        Left _ -> do
            void $ pure $ EHU.showToast $ getString NOT_AVAILABLE
            EHC.liftFlow $ push $ GoBack
            pure unit

  lift $ lift $ await driversProfileResponse
  void $ lift $ lift $ EHU.toggleLoader false
  pure unit

view :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    scrollView
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , background Color.white900
     , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
     ] $
     [  
        relativeLayout[
            height MATCH_PARENT
        ,   width MATCH_PARENT
        ][
            linearLayout[
                height MATCH_PARENT
            ,   width MATCH_PARENT
            ,   orientation VERTICAL
            ,   padding $ PaddingBottom 16
            ,   visibility $ boolToVisibility $ not $ state.data.shimmerView
            ,   onBackPressed push $ const GoBack
            ][
                back push state
            ,   header push state
            ,   driverStats push state
            ,   driverInfoCard push state
            ,   ratings push state
            ,   reviews push state
            ,   trainings push state
            ,   pledge push state
            ,   aboutMe push state
            ]
        ,   shimmerView state 
        ]
    ]

shimmerView :: forall w. DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
shimmerView state =
    linearLayout[
        height MATCH_PARENT
    ,   width MATCH_PARENT
    ,   orientation VERTICAL
    ,   margin $ MarginTop 15
    ,   visibility $ boolToVisibility $ state.data.shimmerView
    ][
        sfl 45 8.0
    ,   sfl 300 8.0
    ,   sfl 200 8.0
    ,   sfl 200 8.0
    ]

sfl :: forall w. Int -> Number -> PrestoDOM (Effect Unit) w
sfl height' radius' =
  shimmerFrameLayout
  [ cornerRadius radius'
  , stroke $ "1," <> Color.grey900
  , margin $ Margin 15 0 15 18
  , width MATCH_PARENT
  , height $ V height'
  ][linearLayout 
    [ width MATCH_PARENT
    , height $ V height'
    , cornerRadius radius'
    , background Color.grey900
    ][]
  ]

back :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
back push state =
    imageView [
      width $ V 22
    , height $ V 22
    , margin $ if DA.null state.data.displayImages then Margin 15 20 0 0 else  Margin 15 20 0 12
    , onClick push $ const GoBack
    , accessibilityHint "Back : Button"
    , accessibility ENABLE
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close_bold"
    ]

driverProfile :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
driverProfile push state = 
    let wid = screenWidth unit
        hgt = if DA.length state.data.displayImages > 0 then ((screenWidth unit)*4)/5 else ((screenWidth unit)*165)/373 -- if there is an actual image then we have to make height / width = 5 / 4 else 373 / 165
    in
    relativeLayout[
        width $ V wid
    ,   height $ V hgt
    ][
        linearLayout
        [ width $ V $ wid
        , height $ V $ hgt
        , id $ getNewIDWithTag "add_image_component_images"
        ][]
    ,   linearLayout[
            height WRAP_CONTENT,
            width MATCH_PARENT,
            gravity CENTER
        ][
            imageView [
                width $ V $ 88
                , height $ V $ 88
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_driver_avatar"
                , visibility $ boolToVisibility $ DA.null state.data.displayImages
                , background Color.white900
                , margin $ MarginLeft 16
            ]
        ]
    ,   linearLayout[
            width MATCH_PARENT
        ,   height WRAP_CONTENT
        ,   margin $ MarginTop 150
        ][
            linearLayout[
                width WRAP_CONTENT
            ,   height WRAP_CONTENT
            ,   weight 1.0
            ][
                imageView [
                width $ V 30
                , height $ V 30
                , margin $ MarginLeft 5
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_white"
                , onClick push $ const PrevImg
                , gravity LEFT
                , visibility $ boolToVisibility $ state.data.imgIdx > 0
                ]
            ]
        ,   imageView [
              width $ V 30
            , height $ V 30
            , margin $ MarginRight 5
            , onClick push $ const NextImg
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_white"
            , visibility $ boolToVisibility $ state.data.imgIdx < DA.length state.data.displayImages - 1
            ]
        ]
    ]

driverProfileData :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
driverProfileData push state = 
    let (DriverStatSummary stats) = state.data.driverStats
        wid = screenWidth unit
        hgt = if DA.length state.data.displayImages > 0 then ((screenWidth unit)*4)/5 else ((screenWidth unit)*165)/373
        colour = if DA.length state.data.displayImages > 0 then Color.white900 else Color.black900
        mrg = if DA.length state.data.displayImages > 0 then 0 else 16
    in 
    linearLayout[
            width $ V $ wid
        ,   height $ V $ hgt
        ,   orientation VERTICAL
        ,   margin $ MarginTop mrg
        ][
            linearLayout[weight 1.0][]
        ,   linearLayout
            [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 15 0 0 15
            , orientation VERTICAL
            , gravity $ if DA.null state.data.displayImages then CENTER else LEFT
            ][
                textView $ 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text state.data.driverName
                , color $ colour
                , margin $ MarginBottom 5
                ] <> FontStyle.h2 CT.TypoGraphy

            ,   linearLayout
                [
                width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , margin $ MarginBottom 10
                ][
                    textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getVariant state.data.vechicleVariant
                    , color $ colour
                    , margin $ MarginRight 5
                    ] <> FontStyle.body3 CT.TypoGraphy
                ,   linearLayout[
                        width WRAP_CONTENT
                    ,   height WRAP_CONTENT
                    ,   visibility $ boolToVisibility $ DA.length state.data.vehicleTags > 0
                    ][
                        imageView
                        [ width $ V 5
                        , height $ V 5
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_dot"
                        , margin $ Margin 0 6 5 0
                        ]
                    ,   textView $ 
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text $ getElement 0 state.data.vehicleTags
                        , color $ colour
                        , margin $ Margin 0 0 5 0
                        ] <> FontStyle.body3 CT.TypoGraphy
                    ,   linearLayout[
                            width WRAP_CONTENT
                        ,   height WRAP_CONTENT
                        ,   visibility $ boolToVisibility $ DA.length state.data.vehicleTags > 1
                        ][
                            imageView
                            [ width $ V 5
                            , height $ V 5
                            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_dot"
                            , margin $ Margin 0 6 5 0
                            ]
                        ,   textView $ 
                            [ width WRAP_CONTENT
                            , height WRAP_CONTENT
                            , text $ getElement 1 state.data.vehicleTags
                            , color $ colour
                            , margin $ Margin 0 0 5 0
                            ] <> FontStyle.body3 CT.TypoGraphy
                        ,   linearLayout[
                                width WRAP_CONTENT
                            ,   height WRAP_CONTENT
                            ,   visibility $ boolToVisibility $ DA.length state.data.vehicleTags > 2
                            ][
                                imageView
                                [ width $ V 5
                                , height $ V 5
                                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_dot"
                                , margin $ Margin 0 6 5 0
                                ]
                            ,   textView $ 
                                [ width WRAP_CONTENT
                                , height WRAP_CONTENT
                                , text $ getElement 2 state.data.vehicleTags
                                , color colour
                                , margin $ Margin 0 0 5 0
                                ] <> FontStyle.body3 CT.TypoGraphy  
                            ]
                        ] 
                    ]
                ]
        ,   linearLayout
            [
              height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation HORIZONTAL
            , background $ if DA.length state.data.displayImages > 0 then Color.lightGreyBlue1 else Color.grey800
            , cornerRadius 24.0
            , visibility $ boolToVisibility $ stats.likedByRidersNum > 0
            ][
                imageView [ 
                width $ V 16
                , height $ V 16
                , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET $  if DA.length state.data.displayImages > 0 then "ny_ic_heart_cream" else "ny_ic_heart_black"
                , margin $ Margin 10 5 8 5
                ]
            ,   textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getString BY <> " " <> show stats.likedByRidersNum <> " " <> if stats.likedByRidersNum > 1 then getString CUSTOMERS else getString CUSTOMER
                , color $ if DA.length state.data.displayImages > 0 then Color.yellow800 else Color.black900
                , margin $ Margin 0 4 6 0
                ] <> FontStyle.tags CT.TypoGraphy
                ]
            ]
        ,   linearLayout[ 
                width MATCH_PARENT, 
                height $ V $ 1, 
                margin $ Margin 15 0 15 0,
                background Color.grey900
            ][]
        ]

getElement :: Int -> Array String -> String
getElement idx arr = fromMaybe "" (DA.index arr idx)

header :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
header push state =
    relativeLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ][
        driverProfile push state
    ,   driverProfileData push state
    ]

driverStats :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
driverStats push state =
    let (DriverStatSummary stats) = state.data.driverStats
    in
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   orientation HORIZONTAL
    ,   margin $ Margin 0 16 0 16
    ][
        linearLayout[
            height WRAP_CONTENT
        ,   width $ V $ (screenWidth unit) / 3
        ,   orientation VERTICAL
        ][
            linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   orientation HORIZONTAL
            ,   gravity CENTER
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ show (fromMaybe 0.0 stats.avgRating) <> " ⭐"
                , color Color.black800
                ] <> FontStyle.h2 CT.TypoGraphy
            ]
        ,   linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   gravity CENTER
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getString RATING
                , color Color.black700
                , margin $ MarginRight 9
                ] <> FontStyle.body3 CT.TypoGraphy
            ]
        ]
    ,   linearLayout [ 
            width $ V 1
        ,   height $ V 35
        ,   margin $ MarginTop 7
        ,   background Color.black500
        ][]
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width $ V $ (screenWidth unit) / 3
        ,   orientation VERTICAL
        ][
            linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   gravity CENTER
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ show stats.numTrips
                , color Color.black800
                ] <> FontStyle.h2 CT.TypoGraphy
            ]
        ,   linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   gravity CENTER
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getString TRIPS
                , color Color.black700
                ] <> FontStyle.body3 CT.TypoGraphy
            ]
        ]
    ,   linearLayout [ 
            width $ V 1
        ,   height $ V 35
        ,   margin $ MarginTop 7
        ,   background Color.black500
        ][]
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width $ V $ (screenWidth unit) / 3
        ,   orientation VERTICAL
        ][
            linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   gravity CENTER
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ show stats.cancellationRate <> "%"
                , color Color.black800
                ] <> FontStyle.h2 CT.TypoGraphy
            ]
        ,   linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   gravity CENTER
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ getString CANCELLATION
                , color Color.black700
                ] <> FontStyle.body3 CT.TypoGraphy
            ]
        ]
    ]

driverInfoCard :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
driverInfoCard push state =
    let drivingYears = (runFn2 JB.differenceBetweenTwoUTC (EHC.getCurrentUTC "") state.data.onboardedAt)
        textForLanguages = "🌏     " <> getString I_SPEAK <> " " <> joinWith "" (DA.mapWithIndex(\index item -> do
            if index == 0 then "<b>" <> item <> "</b>" 
            else if index /= 0 && index /= (DA.length state.data.languages) - 1 then ", " <> "<b>" <> item <> "</b>"
            else getString AND <> "<b>" <> item <> "</b>"
            )state.data.languages)
    in
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 5 10 5
    ,   orientation VERTICAL
    ][
        textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ fromMaybe "" state.data.aboutMe
            , color Color.black700
            , margin $ MarginBottom 20
            , visibility $ boolToVisibility $ not (state.data.aboutMe == Nothing)
            ] <> FontStyle.body20 CT.TypoGraphy
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width WRAP_CONTENT
        ,   orientation VERTICAL
        ,   margin $ MarginBottom 15
        ,   visibility $ boolToVisibility $ not (DA.length state.data.languages == 0)
        ][
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , textFromHtml $ textForLanguages
            , color Color.black700
            ] <> FontStyle.body1 CT.TypoGraphy
        ]
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width WRAP_CONTENT
        ,   orientation VERTICAL
        ,   margin $ MarginBottom 15
        ,   visibility $ boolToVisibility $ (state.data.aboutMe == Nothing) && (drivingYears >= secondsInOneYear)
        ][
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ "🗓️    " <> (getString $ WITH_NAMMAYATRI_FOR $ show (drivingYears/secondsInOneYear))
            , color Color.black700
            ] <> FontStyle.body1 CT.TypoGraphy
        ]
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width WRAP_CONTENT
        ,   margin $ Margin 0 0 0 0
        ,   visibility $ boolToVisibility $ not (state.data.vehicleNum == Nothing)
        ][
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ "ℹ️    " <> getString VEHICLE_NUMBER
            , color Color.black700
            ] <> FontStyle.body1 CT.TypoGraphy
        ,   textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ fromMaybe "" state.data.vehicleNum
            , color Color.black700
            , textSize FontSize.a_14
            , lineHeight "30"
            , fontStyle $ bold LanguageStyle
            , fontWeight $ FontWeight 700
            , margin $ MarginTop $ if os == "IOS" then 2 else 0
            ]
        ]
    ]

reviewItem :: forall w. DriverReview -> PrestoDOM (Effect Unit) w
reviewItem (DriverReview item) =
    linearLayout[
        height $ V 173
    ,   width $ V 300
    ,   background Color.white900
    ,   padding $ Padding 15 15 10 15
    ,   stroke $ "1,"<> Color.grey900
    ,   cornerRadius 10.0
    ,   orientation VERTICAL
    ,   margin $ MarginRight 10
    ][
        textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ fromMaybe "" item.review
        , color Color.black800
        , margin $ MarginBottom 10
        , visibility $ boolToVisibility $ not (item.review == Nothing)
        , maxLines $ if DA.length item.feedBackPills == 0 then 4 else if DA.length item.feedBackPills < 3 then 2 else 1
        , ellipsize true
        ] <> FontStyle.paragraphText CT.TypoGraphy
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width WRAP_CONTENT
        ,   orientation VERTICAL
        ](
            map(\idx -> 
                linearLayout[
                    height WRAP_CONTENT
                ,   width WRAP_CONTENT
                ,   margin $ MarginBottom 4
                ](
                    map(\indx -> pill indx)idx
                )
                ) (DA.take 2 (EHC.convertTo2DArray item.feedBackPills))
        )
    ,   linearLayout[weight 1.0][]
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width WRAP_CONTENT
        ,   margin $ MarginTop 10
        ][
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ show item.rating
            , color Color.black
            , background Color.grey800
            , cornerRadius 50.0
            , padding $ Padding 15 4 15 5
            , textSize FontSize.a_12
            , lineHeight "20"
            , fontStyle $ semiBold LanguageStyle
            , fontWeight $ FontWeight 600
            ]
        ,   linearLayout[
                height WRAP_CONTENT
            ,   width WRAP_CONTENT
            ,   orientation VERTICAL
            ,   margin $ Margin 10 0 0 5
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ fromMaybe "" item.riderName
                , color Color.black800
                , margin $ MarginBottom 3
                , textSize FontSize.a_12
                , lineHeight "18"
                , fontStyle $ semiBold LanguageStyle
                , fontWeight $ FontWeight 600
                ]

            ,   textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ (EHC.convertUTCtoISC item.tripDate "MMM DD YYYY")
                , color Color.black800
                , textSize FontSize.a_11
                , lineHeight "18"
                , fontStyle $ FontStyle.regular LanguageStyle
                , fontWeight $ FontWeight 400
                ]
            ]
        ]
    ]

reviews :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
reviews push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   orientation VERTICAL
    ,   visibility $ boolToVisibility $ not (DA.length state.data.topReviews == 0)
    ][
        horizontalScrollView[
            height WRAP_CONTENT
        ,   width MATCH_PARENT
        ,   background Color.blue600
        ,   scrollBarX false
        ,   padding $ PaddingBottom 24
        ][
            linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   padding $ PaddingLeft 16
            ,   margin $ MarginBottom $ if os == "IOS" then 24 else 0
            ](
                map(\item -> reviewItem item)state.data.topReviews
            )
        ]
    ]

ratings :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
ratings push state =
    let (DriverStatSummary stats) = state.data.driverStats
    in
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   orientation VERTICAL
    ,   margin $ MarginTop 15
    ][
        textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ getString WHAT_PEOPLE_SAY
            , color Color.black800
            , margin $ Margin 16 0 0 16
            ] <> FontStyle.body7 CT.TypoGraphy
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width MATCH_PARENT
        ,   background Color.blue600
        ,   padding $ Padding 16 24 16 24
        ,   orientation VERTICAL
        ][
            linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ][
                imageView
                    [ width $ V 17
                    , height $ V 17
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_small_blue_star"
                    , margin $ Margin 14 39 0 0
                    ]
            ,   imageView
                    [ width $ V 54
                    , height $ V 54
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_star_white_border"
                    , weight 1.0
                    ]
            ,   imageView
                    [ width $ V 38
                    , height $ V 38
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_big_blue_star"
                    ]
            ]
        ,   linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   orientation VERTICAL
            ,   margin $ MarginTop 5
            ][
                linearLayout[
                    height WRAP_CONTENT
                ,   width MATCH_PARENT
                ,   gravity CENTER
                ][
                    textView $
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text $ (show $ round $ fromMaybe 0.0 stats.avgRating) <> " " <> getString STAR_RATING
                    , color "#000000"
                    , gravity CENTER
                    , textSize FontSize.a_19
                    , lineHeight "24"
                    , fontStyle $ bold LanguageStyle
                    , fontWeight $ FontWeight 700
                    ]
                ]
            ,   textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ state.data.driverName <> getString CARD_TEXTS
                , color Color.black800
                , gravity CENTER
                , margin $ Margin 20 5 20 0
                ] <> FontStyle.body3 CT.TypoGraphy
            ]
        ]
    ]

pill :: forall w. String -> PrestoDOM (Effect Unit) w
pill title =
    linearLayout[
        height WRAP_CONTENT
    ,   width WRAP_CONTENT
    ,   stroke $ "1,"<> Color.grey800
    ,   cornerRadius 8.0
    ,   background Color.white900
    ,   padding $ Padding 5 5 7 7
    ,   margin $ MarginRight 10
    ][
        textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ getPillText title
            , color "#333333"
            , margin $ Margin 5 2 0 0
            , textSize FontSize.a_12
            , lineHeight "14"
            , fontStyle $ semiBold LanguageStyle
            , fontWeight $ FontWeight 600
            ]
    ]

pills :: forall w. String -> PrestoDOM (Effect Unit) w
pills title =
    linearLayout[
        height WRAP_CONTENT
    ,   width WRAP_CONTENT
    ,   stroke $ "1,"<> Color.grey800
    ,   cornerRadius 8.0
    ,   background Color.white900
    ,   padding $ Padding 5 5 10 7
    ,   margin $ Margin 0 0 10 10
    ][
        textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ getPillText title
            , color Color.black800
            , margin $ Margin 5 0 0 0
            , textSize FontSize.a_13
            , lineHeight "20"
            , fontStyle $ FontStyle.regular LanguageStyle
            , fontWeight $ FontWeight 400
            ]
    ]

trainings :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
trainings push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   orientation VERTICAL
    ,   margin $ Margin 16 26 0 0
    ,   visibility $ boolToVisibility $ not (DA.length state.data.certificates == 0)
    ][
        textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ getString TRAININGS_I_COMPLETED
            , color Color.black800
            ] <> FontStyle.body7 CT.TypoGraphy
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width WRAP_CONTENT
        ,   margin $ MarginTop 17
        ,   orientation VERTICAL
        ] (map(\item -> 
            linearLayout[
                height WRAP_CONTENT
            ,   width WRAP_CONTENT
            ](
                map(\item -> pills item)item
            )
            )state.data.certificates)
    ]

pledge :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
pledge push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   background "#E5F9E5"
    ,   cornerRadius 10.0
    ,   margin $ Margin 16 24 16 0
    ,   padding $ Padding 16 16 0 19
    ,   visibility $ boolToVisibility $ not (DA.length state.data.pledges == 0)
    ][
        linearLayout[
            height WRAP_CONTENT
        ,   width WRAP_CONTENT
        ,   orientation VERTICAL
        ][
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ getString I_PLEDGE
            , color "#CC53BB6F"
            , margin $ MarginBottom 2
            , textSize FontSize.a_14
            , lineHeight "15"
            , fontStyle $ semiBold LanguageStyle
            , fontWeight $ FontWeight 600
            ]
        ,   linearLayout[
                height WRAP_CONTENT
            ,   width WRAP_CONTENT
            ,   orientation VERTICAL
            ](
                (map(\item -> 
                    let 
                        firstItem = fromMaybe "" (item DA.!! 0)
                        secondItem = fromMaybe "" (item DA.!! 1)
                    in 
                    textView $
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text $ "\"" <> firstItem <> (if secondItem == "" then "" else ", ") <> secondItem  <> "\""
                    , color "#53BB6F"
                    , margin $ MarginTop 2
                    ] <> FontStyle.body7 CT.TypoGraphy
                )(EHC.convertTo2DArray state.data.pledges))
            )
        ]
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width MATCH_PARENT
        ,   gravity RIGHT
        ,   margin $ MarginTop 5
        ][
            imageView [
                width $ V 17
            ,   height $ V 17
            ,   imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_star1"
            ]
        ,   imageView [ 
                width $ V 38
            ,   height $ V 38
            ,   imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_star2"
            ]
        ]
    ]

aboutMe :: forall w. (Action -> Effect Unit) -> DriverProfileScreenCommonState -> PrestoDOM (Effect Unit) w
aboutMe push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 24 16 0
    ,   orientation VERTICAL
    ,   visibility $ boolToVisibility $ not (state.data.drivingSince == Nothing && DA.length state.data.aspirations == 0)
    ][
        textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ getString MORE_ABOUT_ME
            , color Color.black800
            ] <> FontStyle.body7 CT.TypoGraphy
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width MATCH_PARENT
        ,   orientation VERTICAL
        ][
            linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   margin $ MarginTop 16
            ,   visibility $ boolToVisibility $ not (state.data.drivingSince == Nothing)
            ][
                textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ "🚗    " <> (getString DRIVING_SINCE) <> show (fromMaybe 0 state.data.drivingSince)
                , color Color.black800
                , textSize FontSize.a_13
                , lineHeight "18"
                , fontStyle $ semiBold LanguageStyle
                , fontWeight $ FontWeight 500
                ]
            ]
        ,   linearLayout[
                height WRAP_CONTENT
            ,   width MATCH_PARENT
            ,   orientation VERTICAL
            ,   visibility $ boolToVisibility $ not (DA.length state.data.aspirations == 0)
            ](
                (map(\item -> 
                    textView $
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text $ getAspirationsText item
                    , color Color.black800
                    , margin $ Margin 0 16 0 0
                    , textSize FontSize.a_13
                    , lineHeight "18"
                    , fontStyle $ semiBold LanguageStyle
                    , fontWeight $ FontWeight 500
                    ]
                )state.data.aspirations)
            )
        ]
    ]