{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SosActiveScreen.View
  ( ImageTextViewConfig
  , callPoliceView
  , dialPoliceView
  , emergencyContactsView
  , getHeaderTitle
  , screen
  , sfl
  , shimmerView
  , sosActionsView
  , sosDescriptionView
  , testSafetyHeaderView
  , view
  ) where

import Animation
import Prelude
import PrestoDOM
import Screens.NammaSafetyFlow.ComponentConfig
import Screens.NammaSafetyFlow.Components.ContactsList
import Screens.NammaSafetyFlow.Components.HelperViews
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModel as StepsHeaderModel
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex, null, (!!), (..))
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, requestCameraAndMicrophonePermissions)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Animation as PrestoAnim
import Screens.EmergencyContactsScreen.View (getFirstChar, getLastChar)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.SosActiveScreen.Controller (Action(..), ScreenOutput, eval, checkForContactsAndSupportDisabled)
import Screens.Types (NammaSafetyScreenState, SafetySetupStage(..), NewContacts, RecordingState(..), StepsHeaderModelState)
import Screens.Types as ST
import Services.API (GetSosDetailsRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SosActiveScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  lift $ lift $ doAff do liftEffect $ push $ PlaceCall
                  pure unit
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "SosActiveScreen action " action
        let
          _ = spy "SosActiveScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  -- screenAnimation
  --   $ 
    linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.black900
        , padding padding'
        , onBackPressed push $ const $ BackPressed
        ]
        [ case state.props.showTestDrill of
            true -> testSafetyHeaderView push
            false -> Header.view (push <<< SafetyHeaderAction) headerConfig
        , case state.props.showCallPolice of
            true -> dialPoliceView state push
            false -> sosActiveView state push
        -- , shimmerView state
        ]
  where
  padding' = if EHC.os == "IOS" then (PaddingVertical EHC.safeMarginTop (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

  headerConfig = Header.config { useLightColor = true, title = if not state.props.showCallPolice then getString EMERGENCY_SOS else "Call Police", learnMoreTitle = getString LEARN_ABOUT_NAMMA_SAFETY, showLearnMore = false }

------------------------------------- dashboardView -----------------------------------
sosActiveView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosActiveView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ sosDescriptionView state push
    , sosActionsView state push
    , layoutWithWeight
    , separatorView Color.black800 $ MarginTop 0
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ]
        [ callPoliceView state push
        , PrimaryButton.view (push <<< MarkRideAsSafe) (cancelSOSBtnConfig state)
        ]
    ]

sosDescriptionView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosDescriptionView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    , gravity CENTER
    ]
    [ textView
        $ [ text $ getString EMERGENCY_REQUEST_SENT
          , color Color.white900
          ]
        <> FontStyle.h3 TypoGraphy
    , textView
        $ [ text $ getString
               if null state.data.contactsList then
                  PLEASE_STAY_CALM_TEAM_ALERTED
                else
                  SOS_TRIGGERED_DESC
          , color Color.white900
          , margin $ MarginTop 8
          , gravity CENTER
          ]
        <> FontStyle.body1 TypoGraphy
    , imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_emergency_sent"
        , width $ V if not $ null state.data.contactsList then 200 else 250
        , height $ V if not $ null state.data.contactsList then 180 else 250
        , margin $ MarginTop 20
        ]
    ]

sosActionsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosActionsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        , gravity CENTER
        ]
        [ textView
            $ [ text $ getString TRY_ANOTHER_CONTACT
              , color Color.white900
              , visibility $ boolToVisibility $ not $ null state.data.contactsList
              ]
            <> FontStyle.paragraphText TypoGraphy
        , emergencyContactsView state push
        ]
    ]

emergencyContactsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
emergencyContactsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.black700
    , background Color.blackOpacity12
    , orientation VERTICAL
    , margin $ MarginTop 8
    , padding $ Padding 16 14 16 14
    , cornerRadius 12.0
    , visibility $ boolToVisibility $ not $ null state.data.contactsList
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        ( mapWithIndex
            ( \index item ->
                linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , margin $ MarginVertical 10 10
                  , gravity CENTER_VERTICAL
                  ]
                  [ ContactCircle.view (ContactCircle.getContactConfig item index false) (push <<< ContactCircleAction)
                  , textView
                      $ [ text item.name
                        , color Color.white900
                        ]
                      <> FontStyle.body1 TypoGraphy
                  , layoutWithWeight
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , gravity CENTER_VERTICAL
                      , background Color.green900
                      , cornerRadius 24.0
                      , padding $ Padding 8 8 8 8
                      , onClick push $ const $ CallContact index
                      ]
                      [ imageView
                          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_call_white_unfilled"
                          , width $ V 20
                          , height $ V 20
                          ]
                      ]
                  ]
            )
            state.data.contactsList
        )
    ]

callPoliceView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
callPoliceView state push =
  linearLayout
    ( [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding $ PaddingVertical 12 12
      , cornerRadius 8.0
      , background Color.redOpacity20
      ]
        <> if state.props.showTestDrill then [ alpha 0.6 ] else [ onClick push $ const $ if state.props.showCallPolice then CallPolice else ShowPoliceView ]
    )
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_police"
        , height $ V 26
        , width $ V 26
        ]
    , textView
        $ [ text $ getString CALL_POLICE
          , gravity CENTER
          , color Color.white900
          ]
        <> FontStyle.subHeading2 TypoGraphy
    ]

type ImageTextViewConfig
  = { text' :: String
    , isActive :: Boolean
    , useLightTheme :: Boolean
    , useMargin :: Boolean
    , usePadding :: Boolean
    , useFullWidth :: Boolean
    , image :: Maybe String
    }

getHeaderTitle :: SafetySetupStage -> String
getHeaderTitle stage = getString NAMMA_SAFETY

shimmerView :: forall w. ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , visibility $ boolToVisibility state.props.showShimmer
    ]
    [ sfl (V 400) 16 1 true
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        ]
        [ sfl (V 80) 130 3 (getValueToLocalStore IS_SOS_ACTIVE == "true")
        , sfl (V 80) 130 1 (getValueToLocalStore IS_SOS_ACTIVE /= "true")
        ]
    ]

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
            ( \item ->
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
            (1 .. numberOfBoxes)
        )
    ]

testSafetyHeaderView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
testSafetyHeaderView push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.yellow800
    , padding $ Padding 16 16 16 16
    , gravity CENTER_VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , orientation VERTICAL
        ]
        [ textView
            $ [ text $ getString TEST_SAFETY_DRILL
              , color Color.black900
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , textView
            [ text $ getString THIS_IS_NOT_A_REAL_SOS_SITUATION
            , color Color.black900
            ]
        ]
    , layoutWithWeight
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        ]
        [ textView
            [ textFromHtml $ "<u>" <> getString LEARN_MORE <> "</u>"
            , color Color.black900
            , gravity RIGHT
            , margin $ MarginRight 16
            , onClick push $ const LearnMoreClicked
            ]
        ]
    ]

dialPoliceView :: forall w. NammaSafetyScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dialPoliceView state push =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , stroke $ "1," <> Color.black700
        , background Color.blackOpacity12
        , orientation VERTICAL
        , margin $ MarginVertical 16 16
        , padding $ Padding 16 16 16 16
        , cornerRadius 12.0
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_map_pin_white"
                , height $ V 24
                , width $ V 24
                , margin $ MarginRight 10
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ textView
                    $ [ text $ getString YOUR_CURRENT_LOCATION
                      , color Color.white900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text state.data.currentLocation
                      , color Color.white900
                      , margin $ MarginTop 4
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            , margin $ MarginTop 16
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_vehicle_details"
                , height $ V 24
                , width $ V 24
                , margin $ MarginRight 10
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ textView
                    $ [ text $ getString YOUR_VEHICLE_INFO
                      , color Color.white900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text state.data.vehicleDetails
                      , color Color.white900
                      , margin $ MarginTop 4
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
        , separatorView Color.black500 $ Margin 16 16 16 16
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
                , height $ V 24
                , width $ V 24
                , margin $ MarginRight 5
                ]
            , textView
                $ [ text $ getString POLICE_VIEW_INSTRUCTION
                  , color Color.black500
                  ]
                <> FontStyle.tags TypoGraphy
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , margin $ MarginBottom 16
        ]
        [ callPoliceView state push
        ]
    ]
