{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.SavedLocationCard.Controller where

import Screens.Types (LocationListItemState, CardType(..))
import Data.Maybe (Maybe(..))

data Action
  = EditLocation LocationListItemState
  | DeleteLocation String
  | CardClicked LocationListItemState
  | NoAction

getCardType :: String -> Maybe CardType
getCardType cardType = case cardType of
  "HOME_TAG" -> Just HOME_TAG
  "WORK_TAG" -> Just WORK_TAG
  "OTHER_TAG" -> Just OTHER_TAG
  _ -> Nothing
