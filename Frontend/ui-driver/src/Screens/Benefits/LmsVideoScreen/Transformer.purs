{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.Benefits.LmsVideoScreen.Transformer where

import Prelude
import Services.API as API
import Screens.Types as ST
import Common.Types.App (LazyCheck(..), ReelModal(..), ReelButtonConfig(..))
import Common.Styles.Colors as Color
import Data.Maybe (Maybe(..), maybe)
import RemoteConfig as RC

buildLmsVideoeRes :: API.LmsGetVideosRes -> ST.LmsGetVideo
buildLmsVideoeRes (API.LmsGetVideosRes res) =
  { completed : res.completed
  , pending : res.pending
  , quizStatus : res.quizStatus
  , quizEnabled : res.quizEnabled
  , selectedTranslatedModule : Just $ res.selectedModuleInfo
  }

transformReelsPurescriptDataToNativeData :: Array API.LmsVideoRes -> ReelModal
transformReelsPurescriptDataToNativeData reelsData = do
  let transformedData = map (\ (API.LmsVideoRes video) ->
    { id : video.videoId
    , thumbnailImageUrl : Nothing
    , videoUrl : video.url
    , title : video.title
    , description : video.description
    , shareLink : video.shareLink
    , carouselBigImageUrl : Nothing
    , carouselSmallImageUrl : Nothing
    , carouselTextString : Nothing
    , carouselTextColor : Nothing
    , bottomButtonConfig : video.bottomButtonConfig
    , sideButtonConfig : video.sideButtonConfig
    , thresholdConfig : Just {
        isThresholdEnabled : Just video.thresholdEnabled
      , isStartThresholdEnabled : Just $ maybe false (\_ -> true) video.startThresholdInPercentage
      , isEndThresholdEnabled : Just $ maybe false (\_ -> true)  video.completedThresholdInPercentage
      , startThreshold : video.startThresholdInPercentage
      , endThreshold : video.completedThresholdInPercentage
      , sendCallbackAfterEverySecondEnabled : Just false
      }
    } ) reelsData

  { reelData : transformedData
  , titleConfig : {
      size : 18,
      color : Color.white900,
      maxLines : 2
    }
  , descriptionConfig : {
      size : 14,
      color : Color.white900,
      maxLines : 2
    }
  , reelExtraConfig : Just {
      bounceAnimationEnabled : Just true,
      bounceAnimationCount : Just 2,
      bounceAnimationDuration : Just 400,
      progressBarColor : Just Color.white40Alpha,
      progressBarVisible : Just true,
      autoSwipeToNext : Just true,
      seekEnabled : Just false
    }
  }