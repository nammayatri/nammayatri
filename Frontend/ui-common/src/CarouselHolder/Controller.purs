{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module CarouselHolder.Controller where

import Halogen.VDom.DOM.Prop
import PrestoDOM
import PrestoDOM.List
import Effect
import Data.Maybe
import Prelude
import Presto.Core.Types.Language.Flow
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons
import Presto.Core.Types.Language.Flow (Flow, delay)
import Types.App (defaultGlobalState)


type CarouselHolderConfig item a = {
    view :: ListItem
  , items :: Array (Record item)
  , orientation :: Orientation
  , currentPage :: Int
  , autoScroll :: Boolean
  , autoScrollDelay :: Number
  , autoScrollAction :: Maybe a
  , id :: String
  , onPageSelected :: Maybe (String -> a)
  , onPageScrollStateChanged :: Maybe (String -> a)
  , onPageScrolled :: Maybe (String -> a)
  , currentIndex :: Int
}

checkAndStartAutoLoop :: forall a action. (action -> Effect Unit) -> CarouselHolderConfig a action -> Effect Unit
checkAndStartAutoLoop push config = do
  carouselHolderData <- runEffectFn1 updateIdMap config.id
  when config.autoScroll do void $ launchAff $ flowRunner defaultGlobalState $ do autoLoop carouselHolderData.id push config 


autoLoop :: forall a c st. Int -> (a -> Effect Unit) -> CarouselHolderConfig c a -> Flow st Unit
autoLoop id push config = do
  storedCarousel <- liftFlow $ runEffectFn1 getValueFromIdMap config.id
  when (storedCarousel.id == id) $ do
    delay $ Milliseconds config.autoScrollDelay
    updatedCarousel <- liftFlow $ runEffectFn1 getValueFromIdMap config.id
    when ((updatedCarousel.id == id) && updatedCarousel.shouldPush) $ maybe (pure unit) (\action -> do
      liftFlow $ push action) config.autoScrollAction
    autoLoop id push config