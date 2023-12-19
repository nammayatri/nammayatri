{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.View where

import Common.Types.App (LazyCheck(..))
import Data.Array (cons, uncons,length)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Language.Strings (getString)
import Components.IssueListFlow.Controller (getTitle)
import Language.Types (STR(..))
import Prelude (Unit, const, map, unit, ($), (*), (/), (<>),bind,pure,(/=),(<<<),(==), discard, (||), (&&), (>), void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), ScopedScreen, background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, stroke, lineHeight, visibility, afterRender, scrollBarY, imageWithFallback)
import PrestoDOM.Elements.Elements (scrollView)
import PrestoDOM.Events (onClick)
import PrestoDOM.Properties (cornerRadius, fontStyle, gravity, height, imageWithFallback, layoutGravity, margin, padding, scrollBarY, weight)
import Screens.HelpAndSupportScreen.Controller (Action(..), ScreenOutput, eval, getIssueTitle)
import Screens.HelpAndSupportScreen.ScreenData (otherIssueList,IssueOptions(..))
import Services.API (FetchIssueListResp(..),FetchIssueListReq(..))
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
import Components.IssueListFlow as IssueListFlow
import Animation as Anim
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Screens.Types as ST
import Types.App (defaultGlobalState)

screen :: ST.HelpAndSupportScreenState -> ScopedScreen Action ST.HelpAndSupportScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "HelpAndSupportScreen"
  , eval
  , globalEvents : [( \push -> do
        _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
          (FetchIssueListResp issueListResponse) <- Remote.fetchIssueListBT (FetchIssueListReq)
          lift $ lift $ doAff do liftEffect $ push $ FetchIssueListApiCall issueListResponse.issues
        pure $ pure unit)]
  , parent: Nothing
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.HelpAndSupportScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
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
     , visibility (if (state.data.issueListType /= ST.HELP_AND_SUPPORT_SCREEN_MODAL) then GONE else VISIBLE)
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
               ] [ reportAnIssueHeader state push (getString REPORT_AN_ISSUE)
                 , recentRideDetails state push
                 , reportAnIssueHeader state push (getString MORE_OPTIONS)
                 , allOtherTopics state push


               ]
          ]

       ]
   ,  if (state.data.issueListType /= ST.HELP_AND_SUPPORT_SCREEN_MODAL) then issueListModal push state else dummyTextView
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
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (getString HELP_AND_SUPPORT)
              , textSize FontSize.a_18
              , margin $ MarginLeft 20
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
             (case (fst categoryGroup) of
                Just cat -> categoryView cat 3.0 2.0 state push
                _        -> linearLayout [] []
             ),
             (case (snd categoryGroup) of
                  Just cat -> categoryView cat 2.0 3.0 state push
                  _        -> linearLayout [] []
             )
            ]
         ) (getModifiedCategories state.data.categories))

categoryView :: ST.CategoryListType -> Number -> Number -> ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
categoryView categoryGroup marginLeft marginRight state push =
  let vw = (toNumber (screenWidth unit)) / 100.0
  in
  linearLayout
  [ height MATCH_PARENT
  , width (V (round (45.0 * vw)))
  , orientation VERTICAL
  , padding (Padding 0 20 0 20)
  , onClick push (case categoryGroup.categoryAction of
                    "LOST_AND_FOUND" -> (const $ SelectRide categoryGroup)
                    "RIDE_RELATED"   -> (const $ SelectRide categoryGroup)
                    _                -> (const $ OpenChat categoryGroup)
                  )
  , margin (Margin (round (marginLeft * vw)) (round (2.0 * vw)) (round (marginRight * vw)) (round (2.0 * vw)))
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , gravity CENTER
  ][ imageView
      [ width (V (round (30.0 * vw)))
      , height(V (round (30.0 * vw)))
      , cornerRadius (15.0 * vw)
      , imageWithFallback ("," <> categoryGroup.categoryImageUrl)
      , background Color.grey800
      ]
      , textView
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin (Margin 0 10 0 0)
      , text $ getCategoryName categoryGroup.categoryAction
      , gravity CENTER
      , color Color.black800
      , textSize FontSize.a_17
      ]
  ]



getCategoryName :: String -> String
getCategoryName categoryName = case categoryName of
  "LOST_AND_FOUND" -> (getString LOST_AND_FOUND)
  "RIDE_RELATED" -> (getString RIDE_RELATED)
  "APP_RELATED" -> (getString APP_RELATED)
  "FARE" -> (getString FARE)
  _ -> ""

------------------------------------------------- allOtherTopics ------------------------------
allOtherTopics :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
allOtherTopics state push =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 5 0 5)
    ] (map(\optionItem ->
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , visibility (if (((optionItem.menuOptions) == OngoingIssues && ((length (state.data.ongoingIssueList))) > 0) ) || (((optionItem.menuOptions) == ResolvedIssues && ((length (state.data.resolvedIssueList)) > 0))) || ((optionItem.menuOptions) /= ResolvedIssues && (optionItem.menuOptions /= OngoingIssues) ) then VISIBLE else GONE )
            , onClick push (const $ OptionClick optionItem.menuOptions)
            ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              , padding (Padding 15 17 15 17)
              ][  textView $
                  [ height WRAP_CONTENT
                  , weight 1.0
                  , text  (if (optionItem.menuOptions) == OngoingIssues then ((getIssueTitle optionItem.menuOptions) <> " : " <> (toStringJSON (length (state.data.ongoingIssueList)))) else if (optionItem.menuOptions) == ResolvedIssues then (getIssueTitle optionItem.menuOptions)  else (getIssueTitle optionItem.menuOptions))
                  , margin (MarginLeft 10)
                  , color Color.black800
                  ] <> FontStyle.body5 LanguageStyle
                  , imageView
                  [ width $ V 20
                  , height $ V 20
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_grey"
                  ]
              ]
              , horizontalLineView
            ]
          ) otherIssueList
    )

-------------------------------------------- issueListModal ------------------------------------------
issueListModal :: forall w . (Action -> Effect Unit) -> ST.HelpAndSupportScreenState -> PrestoDOM (Effect Unit) w
issueListModal push state =
  IssueListFlow.view (push <<< IssueScreenModal) (issueListState state)

issueListState :: ST.HelpAndSupportScreenState -> IssueListFlow.IssueListFlowState
issueListState state = let
      config' = IssueListFlow.config
      inAppModalConfig' = config'{
        issues = (if (state.data.issueListType == ST.ONGOING_ISSUES_MODAL) then state.data.ongoingIssueList else state.data.resolvedIssueList),
        issueListTypeModal = state.data.issueListType ,
        headerConfig {
          headTextConfig {
            text = (if (state.data.issueListType == ST.ONGOING_ISSUES_MODAL) then (( getString ONGOING_ISSUE) <>(" : ") <> (toStringJSON (length state.data.ongoingIssueList))) else ((getString RESOLVED_ISSUE)))
          }
        },
        fourthTextConfig {
          visibility = (if (state.data.issueListType == ST.ONGOING_ISSUES_MODAL) then VISIBLE else GONE),
          text = (getString REMOVE_ISSUE)
        },
         fifthTextConfig {
          visibility = (if (state.data.issueListType == ST.ONGOING_ISSUES_MODAL) then VISIBLE else GONE),
          text = (getString CALL_SUPPORT_NUMBER)
        }



      }
      in inAppModalConfig'
--------------------------------------------------------------- horizontalLineView ----------------------------
horizontalLineView :: forall w . PrestoDOM (Effect Unit) w
horizontalLineView =
 linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.grey800
  , alpha 0.9
  ][]

--------------------------------------------------------------- dummyTextView ----------------------------
dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ]


