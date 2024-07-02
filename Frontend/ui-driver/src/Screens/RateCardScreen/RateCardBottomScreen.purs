module Screens.RateCardScreen.RateCardBottomScreen where

import Prelude
import Screens.Types
import Prelude
import PrestoDOM
import Effect
import Data.Maybe
import Common.Types.App
import Data.Array
import Data.FoldableWithIndex
import Data.Maybe
import Debug
import Effect
import Helpers.Utils
import Mobility.Prelude
import Prelude
import Services.API
import PrestoDOM.Core.Types.Language.Flow (showScreenWithNameSpace, initUIWithNameSpace)
import PrestoDOM.Core (terminateUI)
import Animation as Anim
import Effect.Aff (Milliseconds(..), launchAff)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Style as FontStyle
import Helpers.API as API
import JBridge (getHeightFromPercent, getWidthFromPercent)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes (__IS_ANDROID)
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App
import Presto.Core.Types.Language.Flow
import Types.App (defaultGlobalState)
import ConfigProvider
import Data.String as DS
import Common.Animation.Config
import Helpers.Pooling (delay)
import Storage

type State
  = { serviceTier :: Maybe String
    , rateCardData :: RateCard
    , ridePreference :: RidePreference
    , animate :: Boolean
    }

screen :: State -> ScopedScreen Action State ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "RateCardBottomScreen"
  , parent: Just "RateCardBottomScreen"
  , globalEvents: [ (\push -> pure $ pure unit) ]
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet
    [ Anim.fadeInWithDuration 150 state.animate
    ]
    $ linearLayout
        [ height $ V $ EHC.screenHeight unit
        , width MATCH_PARENT
        , background Color.blackLessTrans
        , gravity BOTTOM
        , orientation VERTICAL
        , onClick push $ const BackClick
        ]
        [ PrestoAnim.animationSet
            [ Anim.translateInYAnim animConfig { duration = 150, fromY = EHC.screenHeight unit, toY = 0, ifAnim = state.animate } 
            ]
            $ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                ]
                [ linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , padding $ Padding 16 20 16 20
                    , background "#E0D1FF"
                    , cornerRadii $ Corners 24.0 true true false false
                    ]
                    [ textView
                        $ [ text $ "Pricing" <> fromMaybe "" (state.serviceTier >>= (\a -> Just $ " - " <> a))
                          , color Color.black900
                          ]
                        <> FontStyle.h2 TypoGraphy
                    , linearLayout [ weight 1.0 ] []
                    , linearLayout
                        [ height $ V 30
                        , width $ V 30
                        , gravity CENTER
                        , background Color.white900
                        , cornerRadius 15.0
                        , onClick push $ const BackClick
                        ]
                        [ imageView
                            [ height $ V 15
                            , width $ V 15
                            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
                            ]
                        ]
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , background Color.white900
                    , padding $ Padding 16 20 16 (EHC.safeMarginBottomWithDefault 20)
                    , orientation VERTICAL
                    ]
                    [ linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        , margin $ MarginVertical 6 6
                        ]
                        (map (\item -> tableViewCell (spy "item" item)) (getUpdatedFareList state))
                    , linearLayout
                        [ height $ V 1
                        , width MATCH_PARENT
                        , background Color.grey900
                        , margin $ MarginVertical 16 16
                        ]
                        []
                    , textView
                        $ [ text $ getNightCharges state
                          , color Color.black800
                          ]
                        <> FontStyle.body3 TypoGraphy
                    ]
                ]
        ]

tableViewCell :: forall w. { key :: String, val :: String } -> PrestoDOM (Effect Unit) w
tableViewCell item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 10 10
    ]
    [ textView
        $ [ text item.key
          , color Color.black800
          , weight 1.0
          , height WRAP_CONTENT
          ]
        <> FontStyle.body6 TypoGraphy
    , textView
        $ [ text item.val
          , color Color.black800
          ]
        <> FontStyle.body4 TypoGraphy
    ]

getNightCharges :: State -> String
getNightCharges state =
  let
    fromRateCard = foldr (\item acc -> if DS.contains (DS.Pattern "daytime charges") item then acc <> item else acc) "" state.rateCardData.fareInfoDescription
  in
    if fromRateCard == "" then "1.5x of daytime charges applicable at night from 10 PM to 5 AM" else fromRateCard

getUpdatedFareList :: State -> Array { key :: String, val :: String }
getUpdatedFareList state =
  let
    cur = getCurrency appConfig
  in
    ( case state.ridePreference.perKmRate of
        Just fare ->
          [ { key: "Per Mile Fare"
            , val: EHU.priceToBeDisplayed fare false <> "/" <> (getDistanceUnitForCity $ getValueToLocalStore DRIVER_LOCATION)
            }
          ]
        Nothing -> []
    )
      <> ( case state.ridePreference.perMinRate of
            Just fare ->
              [ { key: "Per Minute Fare"
                , val: cur <> show fare <> "/" <> (getDistanceUnitForCity $ getValueToLocalStore DRIVER_LOCATION)
                }
              ]
            Nothing -> []
        )
      <> ( case last state.rateCardData.extraFare of
            Nothing -> []
            Just val -> [ { key: val.key, val: val.val } ]
        )

data ScreenOutput
  = Back State

data Action
  = BackClick

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State
eval BackClick state = updateAndExit state{animate = false} $ Back state

showRateCard :: State -> Flow GlobalState Unit
showRateCard state = do
  EHC.liftFlow $ initUIWithNameSpace "RateCardBottomScreen" Nothing
  void $ showScreenWithNameSpace $ screen state
  void $ EHC.liftFlow $ terminateUI $ Just "RateCardBottomScreen"
