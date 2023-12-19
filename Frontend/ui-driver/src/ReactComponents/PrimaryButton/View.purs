{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.PrimaryButton.View (primaryButton) where 

import Common.Types.App
import Effect.Console (log)
import Prelude

import React.Basic.Hooks (Component, component)
import React.Render.CustomBase (textView)
import Styles.Colors as Color
import Effect (Effect)
import React.Render.CustomBase (linearLayout, relativeLayout, imageView, textView)
import ReactComponents.PrimaryButton.Controller (Config)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Animation (Animation(..))
import Animation as Anim
import Record (merge)
-- import Helpers.Utils ((:<>))
import React.Props (Props)

-- app :: forall action. (Int -> action) -> (action -> Effect Unit) -> Component Config
-- app _ _ = 
--   component "app" \config -> React.do
--     pure $
--       linearLayout {
--           height: show config.height
--         , width: show config.width
--         , margin: show config.margin
--         , visibility: show config.visibility
--         , background: Color.white900
--         , cornerRadius: show config.cornerRadius
--       } [ relativeLayout {
--           height: show config.height
--         , width: show config.width
--         , visibility: show config.visibility
--         , gravity: "center"
--         } [ linearLayout ({
--                 height: show config.height
--               , cornerRadius: show config.cornerRadius
--               , background: show config.background
--               , padding: show config.padding
--               , gravity: show config.gravity
--               , clickable: show (if config.enableLoader then false else config.isClickable)
--               , onClick: log "clicked"
--               , orientation: "horizontal"
--               , onAnimationEnd: log "animation end"
--               , alpha: show (if config.enableLoader then 0.5 else config.alpha)
--               , stroke: config.stroke
--             } :<> (if config.isGradient then ({height: show config.height, background: ""}) else ({height: "", background: config.background})))
--             --  :<> (if config.isGradient then {height: show config.height} else {background: config.background}))
--               -- <> (if config.isGradient then [gradient: config.gradient] else [background: config.background])
--               -- <> (case config.weight of
--               --   Nothing -> [width: config.width]
--               --   Just value ->  [weight: value])
--               -- <> (if config.enableButtonLayoutId then [id $ getNewIDWithTag (config.id <> "_buttonLayout")] else []))
--             [ linearLayout {
--                 width: "wrap_content"
--               , height: "wrap_content"
--               , orientation: "horizontal"
--               , accessibility: "disable"
--               , gravity: show config.gravity
--               , visibility: if config.enableLoader then "invisible" else "visible"
--               , afterRender: log "after render"
--               } [ 
--                   prefixImageLayout config
--                 , textView ( merge {
--                     height: show config.textConfig.height
--                   , accessibilityHint: show config.textConfig.accessibilityHint
--                   , accessibility: "enable"
--                   , color: show config.textConfig.color
--                   , gravity: show config.textConfig.gravity
--                   -- , lineHeight: "20"
--                   , weight: show 1.0
--                   } {weight: show 1.0} )
--                   -- merge (textView {height: show config.textConfig.height}))
--                   -- <> case config.textConfig.textFromHtml of 
--                   --       Just htmlText -> [textFromHtml htmlText]
--                   --       Nothing -> [text config.textConfig.text]
--                   -- <> (FontStyle.getFontStyle config.textConfig.textStyle LanguageStyle)
--                   -- <> (case config.textConfig.weight of
--                   --       Nothing -> [width config.textConfig.width]
--                   --       Just val -> [weight val])
--                 , suffixImageLayout config
--                 ]
--             ]
--         ]
--       ]

-- prefixImageLayout :: Component Config
-- prefixImageLayout = do
--   component "prefixImageLayout" \config ->
--     pure $
--         textView {
--             height: show config.prefixImageConfig.height
--           -- , width: show (if config.isPrefixImage then config.prefixImageConfig.width else 0)
--           , imageWithFallback: show config.prefixImageConfig.imageUrl
--           , padding: show config.prefixImageConfig.padding
--           , visibility: if config.isPrefixImage then "visible" else "gone"
--           , margin: show config.prefixImageConfig.margin
--         }

-- suffixImageLayout :: Component Config
-- suffixImageLayout = do
--   component "suffixImageLayout" \config ->
--     pure $
--       imageView {
--           height: show config.suffixImageConfig.height
--         -- , width: show (if config.isSuffixImage then config.suffixImageConfig.width else 0)
--         -- , imageWithFallback: show config.suffixImageConfig.imageUrl
--         , padding: show config.suffixImageConfig.padding
--         , visibility: if config.isSuffixImage then "visible" else "gone"
--         , margin: show config.suffixImageConfig.margin
--       }

primaryButton :: forall action. action -> (action -> Effect Unit) -> Component{text :: String}
primaryButton action push = do
  component "PrimaryButton" \{text} ->
    pure $
      textView {
          height: "wrap_content"
        , width: "match_parent"
        , cornerRadius: "8.0"
        , background: Color.black900
        , text: text
        , textSize: "16"
        , color: Color.yellow900
        , padding: "16, 16, 16, 16"
        , gravity: "center"
        -- , rippleColor: Color.black600
        , onClick: push $ action
      }

-- -- mergeCmp :: 

-- -- view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
-- -- view push config =
-- --   linearLayout
-- --     [ height config.height
-- --     , width config.width
-- --     , margin config.margin
-- --     , visibility config.visibility
-- --     , background Color.white900
-- --     , cornerRadius config.cornerRadius
-- --     ]
-- --     [ relativeLayout
-- --         [ height config.height
-- --         , width config.width
-- --         , visibility config.visibility
-- --         , gravity CENTER
-- --         ]
-- --         [ PrestoAnim.animationSet
-- --           [ Anim.triggerOnAnimationEnd true] $
-- --           linearLayout
-- --             ([ height config.height
-- --             , cornerRadius config.cornerRadius
-- --             , background config.background
-- --             , padding config.padding
-- --             , gravity config.gravity
-- --             , clickable if config.enableLoader then false else config.isClickable
-- --             , onClick
-- --                 ( \action -> do
-- --                     _ <- pure $ toggleBtnLoader config.id true
-- --                     _ <- pure $ startLottieProcess lottieAnimationConfig{ rawJson = config.lottieConfig.lottieURL, lottieId = getNewIDWithTag config.id}
-- --                     push action
-- --                 )
-- --                 (const OnClick)
-- --             , orientation HORIZONTAL
-- --             , onAnimationEnd
-- --                 ( \action -> do
-- --                     _ <- pure $ if config.lottieConfig.autoDisableLoader then (toggleBtnLoader config.id false) else unit
-- --                     push action
-- --                 )
-- --                 (const NoAction)
-- --             , alpha if config.enableLoader then 0.5 else config.alpha
-- --             , stroke config.stroke
-- --             ]  <> (if config.isGradient then [gradient config.gradient] else [background config.background])
-- --               <> (case config.weight of
-- --                 Nothing -> [width config.width]
-- --                 Just value ->  [weight value])
-- --               <> (if config.enableButtonLayoutId then [id $ getNewIDWithTag (config.id <> "_buttonLayout")] else []))
-- --             [ linearLayout
-- --                 [ width WRAP_CONTENT
-- --                 , height WRAP_CONTENT
-- --                 , orientation HORIZONTAL
-- --                 , accessibility DISABLE
-- --                 , gravity config.gravity
-- --                 , visibility if config.enableLoader then INVISIBLE else VISIBLE
-- --                 , afterRender push (const NoAction)
-- --                 ]
-- --                 [ prefixImageLayout config
-- --                 , textView
-- --                     $ [ height config.textConfig.height
-- --                       , accessibilityHint config.textConfig.accessibilityHint
-- --                       , accessibility ENABLE
-- --                       , color config.textConfig.color
-- --                       , gravity config.textConfig.gravity
-- --                       , lineHeight "20"
-- --                       , weight 1.0
-- --                       ]
-- --                     <> case config.textConfig.textFromHtml of 
-- --                         Just htmlText -> [textFromHtml htmlText]
-- --                         Nothing -> [text config.textConfig.text]
-- --                     <> (FontStyle.getFontStyle config.textConfig.textStyle LanguageStyle)
-- --                     <> (case config.textConfig.weight of
-- --                           Nothing -> [width config.textConfig.width]
-- --                           Just val -> [weight val])
-- --                 , suffixImageLayout config
-- --                 ]
-- --             ]
-- --         , linearLayout
-- --             ([ height config.height
-- --             , gravity CENTER
-- --             ] <> (case config.weight of
-- --                 Nothing -> [width config.width]
-- --                 Just value ->  [weight value]))
-- --             [ lottieAnimationView
-- --                 [ id (getNewIDWithTag config.id)
-- --                 , visibility if config.enableLoader then VISIBLE else GONE
-- --                 , height config.lottieConfig.height
-- --                 , width config.lottieConfig.width
-- --                 ]
-- --             ]
-- --         ]
-- --     ]

-- -- prefixImageLayout :: forall w. Config -> PrestoDOM (Effect Unit) w
-- -- prefixImageLayout config =
-- --   PrestoAnim.animationSet config.prefixImageConfig.animation $ imageView
-- --     [ height config.prefixImageConfig.height
-- --     , width $ if config.isPrefixImage then config.prefixImageConfig.width else V 0
-- --     , imageWithFallback config.prefixImageConfig.imageUrl
-- --     , padding config.prefixImageConfig.padding
-- --     , visibility if config.isPrefixImage then VISIBLE else GONE
-- --     , margin config.prefixImageConfig.margin
-- --     ]

-- -- suffixImageLayout :: forall w . Config -> PrestoDOM (Effect Unit) w
-- -- suffixImageLayout config =
-- --   PrestoAnim.animationSet config.suffixImageConfig.animation $  imageView
-- --     [ height config.suffixImageConfig.height
-- --     , width $ if config.isSuffixImage then config.suffixImageConfig.width else V 0
-- --     , imageWithFallback config.suffixImageConfig.imageUrl
-- --     , padding config.suffixImageConfig.padding
-- --     , visibility if config.isSuffixImage then VISIBLE else GONE
-- --     , margin config.suffixImageConfig.margin
-- --     ]
