{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.NotificationDetailModel.Controller where

import Components.PopUpModal as PopUpModal
import Data.String (take, drop, split, Pattern (..))
import Prelude

instance showAction :: Show Action where
  show (BackArrow) = "BackArrow"
  show (AddCommentModelAction var1) = "AddCommentModelAction_" <> show var1
  show (AddCommentClick) = "AddCommentClick"
  show (AfterRender) = "AfterRender"
  show (LikeMessage) = "LikeMessage"
  show (IncreaseViewCount) = "IncreaseViewCount"
  show (NoAction) = "NoAction"
  show (YoutubeVideoStatus _) = "YoutubeVideoStatus"
  show (ShareMessage) = "ShareMessage"
  
data Action
  = BackArrow
  | AddCommentModelAction PopUpModal.Action
  | AddCommentClick
  | AfterRender
  | LikeMessage
  | IncreaseViewCount
  | NoAction
  | YoutubeVideoStatus String
  | ShareMessage


fetchTitleAndUrl :: Int -> String -> Array String
fetchTitleAndUrl desLength str = 
 let
   titleAndUrl = take (desLength - 2) $ drop 1 str
   splitTitleAndUrl = split (Pattern "<>") titleAndUrl
 in
   splitTitleAndUrl
