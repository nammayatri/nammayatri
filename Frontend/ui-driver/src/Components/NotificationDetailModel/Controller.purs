module Components.NotificationDetailModel.Controller where

import Components.PopUpModal as PopUpModal

data Action
  = BackArrow
  | AddCommentModelAction PopUpModal.Action
  | AddCommentClick
  | AfterRender
  | NoAction
