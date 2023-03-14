module Screens.NotificationsScreen.ScreenData where

import Data.Maybe (Maybe(..))
import PrestoDOM (Visibility(..), toPropValue)
import Screens.Types (AnimationState(..), NotificationCardPropState, NotificationsScreenState, NotificationCardState)
import Services.APITypes (MediaType(..))

initData :: NotificationsScreenState
initData = {
  shimmerLoader : AnimatingIn,
  prestoListArrayItems : [],
  recievedResponse: false,
  notificationList: [],
  loadMoreDisabled: false,
  selectedItem: dummyNotificationCard,
  offsetValue: 0,
  loaderButtonVisibility: false,
  notificationDetailModelState : {
    mediaUrl : "",
    title : "",
    timeLabel : "",
    description : "",
    actionText : "",
    actionVisibility : VISIBLE,
    addCommentModelVisibility : GONE,
    comment : Nothing,
    commentBtnActive : false,
    messageId : "",
    notificationNotSeen : false,
    imageUrl : "",
    mediaType : Nothing
  },
  notifsDetailModelVisibility : GONE,
  loadMore : false
}

dummyNotificationCard :: NotificationCardState 
dummyNotificationCard = {
    mediaUrl : "",
    title : "",
    description : "",
    timeLabel : "",
    action1Text : "",
    action2Text : "",
    notificationLabel : "",
    messageId : "",
    notificationNotSeen : false,
    comment : Nothing,
    imageUrl : "",
    mediaType : Nothing
  }
