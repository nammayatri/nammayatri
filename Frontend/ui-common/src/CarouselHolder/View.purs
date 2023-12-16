{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module CarouselHolder.View where

import Debug
import Engineering.Helpers.Commons
import Prelude
import PrestoDOM
import PrestoDOM.Animation
import PrestoDOM.List

import Animation (scaleYAnimWithDelay, triggerOnAnimationEnd)
import CarouselHolder.Controller (CarouselHolderConfig)
import Data.Array (length, mapWithIndex)
import Data.Int (even, toNumber)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Uncurried (runEffectFn1)
import Halogen.VDom.DOM.Prop (PropValue)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import JBridge (getArray)
import Presto.Core.Types.Language.Flow (Flow, delay)
import PrestoDOM.Events (registerEvent)
import Styles.Colors as Color
import Type.Row.Homogeneous (class Homogeneous)
import Types.App (defaultGlobalState)

carouselView :: forall w a action. Homogeneous a PropValue => (action -> Effect Unit) -> CarouselHolderConfig a action -> Layout w
carouselView push config =
  let itemsLen = length config.items
  in
  frameLayout
  [ height $ V 100
  , width MATCH_PARENT
  , gravity BOTTOM
  , visibility if itemsLen  <= 0 then GONE else VISIBLE
  ]$
    if itemsLen  <= 0 
      then [] 
      else 
        [ viewPager2
          $ [ listItem  $ config.view
          , listDataV2 $ config.items
          , height $ V 100
          , width MATCH_PARENT
          , currentItem config.currentPage
          , scrollDirection config.orientation
          , afterRender (\_ -> when (itemsLen > 1) $ checkAndStartAutoLoop push config) (pure unit)
          , registerEvent "RestartAutoScroll" (\_ -> when (itemsLen > 1) $ checkAndStartAutoLoop push config) (pure unit)
          ] <> maybe [] (\action -> if (itemsLen > 1) then [onPageSelected push action] else []) config.onPageSelected
            <> maybe [] (\action -> if (itemsLen > 1) then [onPageScrollStateChanged push action] else []) config.onPageScrollStateChanged
            <> maybe [] (\action -> if (itemsLen > 1) then [onPageScrolled push action] else []) config.onPageScrolled
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding $ PaddingBottom 8
          ][ linearLayout [weight 1.0, orientation VERTICAL] []
          , scrollIndicator itemsLen config
          ]
        ]


scrollIndicator :: forall w a action. Homogeneous a PropValue => Int -> CarouselHolderConfig a action -> Layout w
scrollIndicator itemsLen config = 
  let zeroArray = getArray $ itemsLen
  in
  relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , visibility if itemsLen <= 1 then GONE else VISIBLE
  ][  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding $ Padding 5 4 5 4
      , gravity CENTER
      , background Color.black900
      , cornerRadius 8.0
      , alpha 0.2
      ] $ mapWithIndex (\idx _ -> indicatorDot (getDotSize config.currentIndex idx) INVISIBLE (if idx == (itemsLen - 1) then MarginLeft 0 else MarginRight 2)) $ zeroArray
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding $ Padding 5 4 5 4
      , gravity CENTER
      ] $ mapWithIndex (\idx _ -> indicatorDot (getDotSize config.currentIndex idx) VISIBLE (if idx == (itemsLen - 1) then MarginLeft 0 else MarginRight 2)) $ zeroArray
  ]


indicatorDot :: forall w. Int -> Visibility -> Margin -> Layout w
indicatorDot size visible dotMargin =
  linearLayout
  [ height $ V size
  , width $ V size
  , background Color.white900
  , visibility visible
  , margin dotMargin
  , cornerRadius $ toNumber $ size / 2
  ][]

type Layout w = PrestoDOM (Effect Unit) w


checkAndStartAutoLoop :: forall a action. (action -> Effect Unit) -> CarouselHolderConfig a action -> Effect Unit
checkAndStartAutoLoop push config = do
  id <- runEffectFn1 updateIdMap config.id
  when config.autoScroll do void $ launchAff $ flowRunner defaultGlobalState $ do autoLoop id push config 

autoLoop :: forall a c st. Int -> (a -> Effect Unit) -> CarouselHolderConfig c a -> Flow st Unit
autoLoop id push config = do
  storedId <- liftFlow $ runEffectFn1 getValueFromIdMap config.id
  when (storedId == id) $ do
    delay $ Milliseconds config.autoScrollDelay
    updatedId <- liftFlow $ runEffectFn1 getValueFromIdMap config.id
    when (updatedId == id) $ maybe (pure unit) (\action -> do
      liftFlow $ push action) config.autoScrollAction
    autoLoop id push config

getDotSize :: Int -> Int -> Int
getDotSize currentPage idx =  
  if currentPage == idx 
    then 6
    else 
      if currentPage == (idx - 1) || currentPage == (idx + 1) 
        then 5
        else 4