module Screens.OnBoardingSubscriptionScreen.Transformer where

import Prelude
import Common.Types.App(ReelModal(..))
import RemoteConfig as RC
import Common.Styles.Colors as Color
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Services.API as API
import Data.Foldable (foldl)
import Data.Array as DA

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
    , thresholdConfig : Nothing
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
      autoSwipeToNext : Just false,
      seekEnabled : Just true
    }
  }

transformReelsRespToReelsData :: API.ReelsResp -> Array RC.ReelItem
transformReelsRespToReelsData (API.ReelsResp resp) = map (\(API.ReelsData item) -> 
  let thresholdConfig = maybe Nothing (\(API.ReelVideoThresholdConfig config) -> Just config) item.thresholdConfig
      sideButtons = generateButtonConfig item.sideButtonConfig
      bottomButtons = generateButtonConfig item.bottomButtonConfig
  in
  { title : fromMaybe "" item.title,
    description : item.description,
    id : item.id,
    shareLink : item.shareLink,
    thumbnailImageUrl : item.thumbnailImageUrl,
    videoUrl : item.videoUrl,
    carouselBigImageUrl : item.carouselBigImageUrl,
    carouselSmallImageUrl : item.carouselSmallImageUrl,
    carouselTextString : item.carouselTextString,
    carouselTextColor : item.carouselTextColor,
    bottomButtonConfig : if DA.null bottomButtons then Nothing else Just bottomButtons,
    sideButtonConfig : if DA.null sideButtons then Nothing else Just sideButtons,
    thresholdConfig : thresholdConfig
  }) resp.reels

  where 
    generateButtonConfig = foldl (\acc (API.ReelRowButtonConfig rowConfig) -> acc <> [(map (\buttonConfig -> generateSingleButtonConfig buttonConfig) rowConfig.row)] ) [[]]

    generateSingleButtonConfig (API.ReelButtonConfig buttonConfig) = {
        prefixImage : buttonConfig.prefixImage,
        suffixImage : buttonConfig.suffixImage,
        actions : buttonConfig.actions,
        shareLink : buttonConfig.shareLink,
        shareText : buttonConfig.shareText,
        shareMessageTitle : Nothing,
        buttonColor : buttonConfig.buttonColor,
        cornerRadius : buttonConfig.cornerRadius,
        text : buttonConfig.text,
        textSize : buttonConfig.textSize,
        textColor : buttonConfig.textColor,
        prefixImageHeight : buttonConfig.prefixImageHeight,
        prefixImageWidth : buttonConfig.prefixImageWidth,
        suffixImageHeight : buttonConfig.suffixImageHeight,
        suffixImageWidth : buttonConfig.suffixImageWidth,
        activeIndex : buttonConfig.activeIndex,
        inActiveIndex : buttonConfig.inActiveIndex,
        activeIndexWidth : buttonConfig.activeIndexWidth,
        activeIndexHeight : buttonConfig.activeIndexHeight,
        inActiveIndexWidth : buttonConfig.inActiveIndexWidth,
        inActiveIndexHeight : buttonConfig.inActiveIndexHeight
    }