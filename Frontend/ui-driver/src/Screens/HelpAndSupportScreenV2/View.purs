{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreenV2.View
  where

import Common.Types.App (LazyCheck(..), CategoryListType)
import Data.Array (cons, uncons,length, mapWithIndex)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Language.Strings (getString)
import Resource.Localizable.StringsV2 (getStringV2)
import Resource.Localizable.TypesV2 as LT2
import Language.Types (STR(..))
import Prelude (Unit, const, map, unit, ($), (*), (/), (<>),bind,pure,(/=),(<<<),(==), discard, (||), (&&), (>), void, show, not, when)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, stroke, lineHeight, visibility, afterRender, scrollBarY, imageWithFallback, rippleColor, clickable, relativeLayout )
import PrestoDOM.Elements.Elements (scrollView)
import PrestoDOM.Events (onClick)
import PrestoDOM.Properties (cornerRadius, fontStyle, gravity, height, imageWithFallback, layoutGravity, margin, padding, scrollBarY, weight)
import Screens.HelpAndSupportScreenV2.Controller (Action(..), ScreenOutput, eval)
import Screens.HelpAndSupportScreen.ScreenData (otherIssueList,IssueOptions(..))
import Services.API as API
import Services.Backend as Remote
import Effect.Aff (launchAff)
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (flowRunner, screenWidth)
import Effect.Class (liftEffect)
import Language.Types(STR(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Components.IssueList as IssueList
import Animation as Anim
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Screens.Types as ST
import Types.App (defaultGlobalState)
import Screens.HelpAndSupportScreenV2.ComponentConfig as Config
import Components.PopUpModal as PopUpModal
import Debug
import Data.String as DS
import Mobility.Prelude (boolToVisibility)

screen :: ST.HelpAndSupportScreenState -> Screen Action ST.HelpAndSupportScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "HelpAndSupportScreenV2"
  , globalEvents : []
  , eval:
      ( \action state -> do
          let
            _ = spy "HelpAndSupportScreenV2 action" action
          let
            _ = spy "HelpAndSupportScreenV2 state" state
          eval action state
      )
  }

view :: forall w . (Action -> Effect Unit) -> ST.HelpAndSupportScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const BackPressed
    , afterRender push (const AfterRender)
    , background Color.white900
    ]([linearLayout
     [height MATCH_PARENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , visibility $ boolToVisibility $ state.data.issueListType == ST.HELP_AND_SUPPORT_SCREEN_MODAL
     ]
      [ headerLayout state push
        , scrollView
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , scrollBarY false
          ] [ linearLayout
               [ height MATCH_PARENT
               , width MATCH_PARENT
               , orientation VERTICAL
               ]( if state.props.showOperationsHub 
                    then [ operationsHubView state push ]
                    else [ testRideRequestView state push
                         , reportAnIssueHeader state push (getString REPORT_AN_ISSUE)
                         , recentRideDetails state push
                         ]
                )
          ]
       ]
   , if state.props.enableDummyPopup then testRideConfirmation push state else linearLayout[][]

   ])



-------------------------------------------------- headerLayout --------------------------
headerLayout :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , layoutGravity "center_vertical"
        , padding $ Padding 5 16 5 16
        ]
        [ imageView
            [ width $ V 40
            , height $ V 40
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_arrow_left_black"
            , onClick push $ const BackPressed
            , padding $ Padding 7 7 7 7
            , margin $ MarginLeft 5 
            , rippleColor Color.rippleShade
            , cornerRadius 20.0
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ if state.props.showOperationsHub then (getStringV2 LT2.operations_hub) else (getString HELP_AND_SUPPORT)
              , textSize FontSize.a_18
              , margin $ MarginLeft 10
              , weight 1.0
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

------------------------------------------ headerView ------------------
reportAnIssueHeader :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> String -> forall w . PrestoDOM (Effect Unit) w
reportAnIssueHeader state push leftText =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , padding (Padding 15 10 10 10)
 , background Color.lightGreyBlue
 ][ textView $
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , text leftText
    , gravity CENTER_VERTICAL
    , color Color.black800
    , lineHeight "25"
    ] <> FontStyle.h3 LanguageStyle
  , linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ textView $
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , textSize FontSize.a_17
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity RIGHT
        , color Color.blueTextColor
        ] <> FontStyle.subHeading1 LanguageStyle
    ]
 ]

-------------------------------------------------- recentRideDetails --------------------
getModifiedCategories :: forall a . Array a -> Array (Tuple (Maybe a) (Maybe a))
getModifiedCategories xs = case uncons xs of
    Nothing -> []
    Just {head: y, tail: ys} -> case uncons ys of
        Nothing -> [Tuple (Just y) Nothing]
        Just {head: z, tail: zs} -> cons (Tuple (Just y) (Just z)) $ getModifiedCategories zs

recentRideDetails :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
recentRideDetails state push =
  let vw = (toNumber (screenWidth unit)) / 100.0
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , orientation VERTICAL
  , padding (Padding 0 (round (2.0 * vw)) 0 (round (2.0 * vw)))
  ] (map (\categoryGroup ->
           linearLayout
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , orientation HORIZONTAL
             ][
             case (fst categoryGroup) of
                Just cat -> categoryView cat 3.0 2.0 state push
                _        -> linearLayout [] []
             ,
             case (snd categoryGroup) of
                  Just cat -> categoryView cat 2.0 3.0 state push
                  _        -> linearLayout [] []
             
            ]
         ) (getModifiedCategories [
          {categoryName: getStringV2 LT2.faqs, categoryImageUrl: Just "ny_ic_help_and_support_faq", categoryAction: Just "FAQs", categoryId: "FAQs", isRideRequired: true, maxAllowedRideAge: Just 10, allowedRideStatuses: Just ["ONGOING"], categoryType: "FAQS"},
          {categoryName: getStringV2 LT2.whatsapp_chat, categoryImageUrl: Just "ny_ic_help_and_support_whatsapp", categoryAction: Just "WhatsApp Chat", categoryId: "WhatsApp Chat", isRideRequired: false, maxAllowedRideAge: Just 10, allowedRideStatuses: Just ["ONGOING"], categoryType: "WHATSAPP_CHAT"},
          {categoryName: getStringV2 LT2.mail_us, categoryImageUrl: Just "ny_ic_help_and_support_mail_us", categoryAction: Just "Mail Us", categoryId: "Mail Us", isRideRequired: false, maxAllowedRideAge: Just 10, allowedRideStatuses: Just ["ONGOING"], categoryType: "MAIL_US"},
          {categoryName: getStringV2 LT2.call_us, categoryImageUrl: Just "ny_ic_help_and_support_call_us", categoryAction: Just "Call Us", categoryId: "Call Us", isRideRequired: false, maxAllowedRideAge: Just 10, allowedRideStatuses: Just ["ONGOING"], categoryType: "CALL_US"},
          {categoryName: getStringV2 LT2.operations_hub, categoryImageUrl: Just "ny_ic_help_and_support_operations_hub", categoryAction: Just "Operations Hub", categoryId: "Operations Hub", isRideRequired: false, maxAllowedRideAge: Just 10, allowedRideStatuses: Just ["ONGOING"], categoryType: "OPERATIONS_HUB"}
         ]))


categoryView :: CategoryListType -> Number -> Number -> ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
categoryView categoryGroup marginLeft marginRight state push =
  let vw = (toNumber (screenWidth unit)) / 100.0
  in
  linearLayout
  [ height WRAP_CONTENT
  , width (V (round (45.0 * vw)))
  , orientation VERTICAL
  , padding (Padding 0 20 0 20)
  , onClick push $ const $ HelpAndSupportCategoryAC categoryGroup
  , margin (Margin (round (marginLeft * vw)) (round (2.0 * vw)) (round (marginRight * vw)) (round (2.0 * vw)))
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , gravity CENTER
  , rippleColor Color.rippleShade
  ][ imageView
      [ width (V (round (20.0 * vw)))
      , height(V (round (20.0 * vw)))
      , cornerRadius (15.0 * vw)
      , padding (Padding 8 8 8 8)
      , imageWithFallback $ fetchImage COMMON_ASSET (fromMaybe "" categoryGroup.categoryImageUrl)
      ]
      , textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin (Margin 0 10 0 0)
      , text $ categoryGroup.categoryName 
      , gravity CENTER
      , color Color.black800
      , textSize FontSize.a_14
      , lineHeight "20"
      ] <> FontStyle.body1 LanguageStyle
  ]

testRideRequestView :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
testRideRequestView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , visibility GONE
    ][ reportAnIssueHeader state push (getString CHECK_APP)
     , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 10 15 10 15
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , stroke $ "1," <> Color.grey900
            , cornerRadius 8.0
            ][ imageView
                [ width $ V 64
                , height $ V 64
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_dummy_ride_request"
                ]
             , linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , margin $ MarginTop 10
                ][ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString CHECK_YOUR_APP_BY_TEST_RIDE_REQUEST
                    , color Color.black800
                    ] <> FontStyle.body1 LanguageStyle
                 , textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString CHECK_NOW
                    , color Color.blue900
                    , margin $ MarginTop 3
                    , padding $ PaddingBottom 4
                    , onClick push $ const CheckDummyRide
                    , clickable true
                    ] <> FontStyle.body1 LanguageStyle
                 ]
             ]
         ]
    ]

operationsHubView :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
operationsHubView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginVertical 8 8 
    ](mapWithIndex (\index (API.OperationHub hub) -> 
        linearLayout 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ Margin 16 8 16 8
        , stroke $ "1," <> Color.grey900
        , cornerRadius 8.0
        , orientation VERTICAL
        ][ textView $ 
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ hub.name
            , color Color.black900
            , textSize FontSize.a_14
            , lineHeight "20"
            , margin $ Margin 16 12 16 12
            ] <> FontStyle.subHeading1 LanguageStyle
        , textView $ 
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ hub.address
            , color Color.black800
            , textSize FontSize.a_14
            , lineHeight "20"
            , margin $ Margin 16 0 16 16
            ] <> FontStyle.subHeading2 LanguageStyle
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin $ Margin 16 8 16 0
            , onClick push $ const $ CallHub hub.mobileNumber
            , gravity CENTER_VERTICAL
            ][ imageView 
                [ height $ V 16
                , width $ V 16
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_phone_unfilled_blue"
                , margin $ MarginRight 8
                ]
            , textView $ 
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text $ getStringV2 LT2.contact_hub
                , color Color.blue900
                ] <> FontStyle.body1 LanguageStyle
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin $ Margin 16 12 16 12
            , onClick push $ const $ OpenMaps (API.OperationHub hub)
            , gravity CENTER_VERTICAL
            ][ imageView 
                [ height $ V 16
                , width $ V 16
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_locate_on_map_purple"
                , margin $ MarginRight 8
                ]
            , textView $ 
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text $ getStringV2 LT2.locate_on_map
                , color Color.blue900
                ] <> FontStyle.body1 LanguageStyle
            ]
        ]) state.data.operationHubs)

testRideConfirmation :: forall w . (Action -> Effect Unit) -> ST.HelpAndSupportScreenState -> PrestoDOM (Effect Unit) w
testRideConfirmation push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalAction) (Config.testRideConfirmationConfig state )]