-- @React.Component

module ReactComponents.ZooBookingModel.View where

import Prelude
import Debug 
import Common.Types.App

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Data.Tuple (Tuple(Tuple))
import Data.Maybe (Maybe(Just), Maybe(Nothing))
import Data.Array (dropEnd, findIndex, snoc, take, unsnoc, (..))
import React.Basic.Hooks (JSX, Component, component, readRef, useContext, useEffect, useEffectAlways, useEffectOnce, useRef, useState, writeRef)
import React.Render.CustomBase
import React.Basic.Hooks as React
import React.Hooks.Hooks (createContext, createKeyedPortal_, createPortal_, provider)
import BasicHoooksTest (heavyComponent)
import React.Basic.Utils (createRenderer, keyedJSX)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Unsafe (unsafePerformEffect)
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App
import Screens.Types as ST
import ReactComponents.IncrementDecrement.View as IncrementDecrementView
import ReactComponents.IncrementDecrement.Controller as IncrementDecrementController
import Screens.RideHistoryScreen.ComponentConfig (incrementDecrementConfig)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), height, width)
import Halogen.VDom.Types (VDom(..))
import PrestoDOM as PrestoDOM
import Screens.RideHistoryScreen.Controller (Action(..))
import ReactComponents.PrimaryButton.View as PrimaryButtonView
import ReactComponents.PrimaryButton.Controller as PrimaryButtonController
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl)
import JBridge (lottieAnimationConfig, startLottieProcess)
import Engineering.Helpers.Commons (getNewIDWithTag)
-- import React.Render.CustomBase (lottieAnimationView)

-- view :: forall action. (action -> Effect Unit) -> ST.RideHistoryScreenState -> Effect Unit
-- view push state = PrestoDOM.linearLayout[
--       height MATCH_PARENT
--     , width MATCH_PARENT
--   ] [ ]

app :: forall action. (Int -> action) -> (action -> Effect Unit) -> Component{push :: (Action -> Effect Unit), state :: ST.RideHistoryScreenState}
app action halfPush = do
  component "App" \{push, state} -> React.do    
    
    useEffectAlways (\_ -> do
      log "useEffectAlways"
      _ <- pure $ startLottieProcess lottieAnimationConfig { rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/accepted_by_another_driver_lottie.json", lottieId = (getNewIDWithTag "splashLottieAnimation"), speed = 1.8 }
      -- _ <- push action
      pure $ pure unit
    )

    -- ref <- useRef ("safas")
    -- r <- readRef(ref)
    -- _ <- log (unsafePerformEffect r)

    pure $
      linearLayout {
        height: "match_parent"
      , width: "match_parent"
      , orientation: "vertical"
      , background: Color.grey900
      } [ linearLayout {
            height: "wrap_content"
          , width: "match_parent"
          , cornerRadius: "8.0"
          , background: Color.white900
          , orientation: "vertical"
          , padding: "20, 20, 20, 20"
          , margin: "20, 20, 20, 20"
        } [ textView {
              text: "Heritage Cruise"
            , color: Color.black800
            , textSize: "18"
            , fontWeight: "700" 
            , margin: "0, 0, 0, 24"
            }
          , textView {
                text: "Cruise Ticket (₹169 per person)"
              , color: Color.black800
              , textSize: "14"
              , margin: "0, 0, 0, 0"
            }
          , linearLayout {weight: "1.0"} []
          -- , IncrementDecrementView.app action halfPush (incrementDecrementConfig push state)
          , PrimaryButtonView.primaryButton BackPressed push {text: "Pay Money"}
        --   , imageView {
        --       height: "24"
        --     , width: "24"
        --     , imageWithFallback: fetchImage FF_COMMON_ASSET $ "ny_ic_radio_selected"
        --     , visibility: "visible"
        --     , onClick: halfPush $ action 0 
        --     }
          ]
          -- , lottieAnimationView {  
          --     height: "match_parent"
          --   , width: "match_parent"
          --   , id: (getNewIDWithTag "splashLottieAnimation")
          -- }
        ] 