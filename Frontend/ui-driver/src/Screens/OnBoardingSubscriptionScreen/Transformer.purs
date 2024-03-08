module Screens.OnBoardingSubscriptionScreen.Transformer where

import Prelude
import Common.Types.App(ReelModal(..))
import RemoteConfig as RC
import Common.Styles.Colors as Color

transformReelsPurescriptDataToNativeData :: Array RC.ReelItem -> ReelModal
transformReelsPurescriptDataToNativeData reelsData = do
  let transformedData = map (\ eachItem ->
    { id : eachItem.id
    , thumbnailImageUrl : eachItem.thumbnailImageUrl
    , videoUrl : eachItem.videoUrl
    , title : eachItem.title
    , description : eachItem.description
    , shareLink : eachItem.shareLink
    , carouselBigImageUrl : eachItem.carouselBigImageUrl
    , carouselSmallImageUrl : eachItem.carouselSmallImageUrl
    , carouselTextString : eachItem.carouselTextString
    , carouselTextColor : eachItem.carouselTextColor
    , bottomButtonConfig : eachItem.bottomButtonConfig
    , sideButtonConfig : eachItem.sideButtonConfig
    } ) reelsData

  { reelData : transformedData
  , titleConfig : {
      size : 18,
      color : Color.white900,
      maxLines : 2
    }
  , descriptionConfig : {
      size : 12,
      color : Color.white900,
      maxLines : 2
    }
  }