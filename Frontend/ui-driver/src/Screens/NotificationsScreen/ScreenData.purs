{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NotificationsScreen.ScreenData where

import Data.Maybe (Maybe(..))
import ConfigProvider
import PrestoDOM (Visibility(..), toPropValue)
import Screens.Types (AnimationState(..), NotificationCardPropState, NotificationsScreenState, NotificationCardState)
import Services.API (MediaType(..))

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
    description : [],
    actionText : "",
    actionVisibility : VISIBLE,
    addCommentModelVisibility : GONE,
    comment : Nothing,
    commentBtnActive : false,
    messageId : "",
    notificationNotSeen : false,
    imageUrl : "",
    mediaType : Nothing,
    likeCount : 0,
    likeStatus : false,
    viewCount : 0,
    shareable : false
  },
  notifsDetailModelVisibility : GONE,
  loadMore : false,
  selectedNotification : Nothing,
  deepLinkActivated : false,
  config : getAppConfig appConfig
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
    mediaType : Nothing,
    likeCount : 0,
    viewCount : 0,
    likeStatus : false,
    shareable : false
  }
