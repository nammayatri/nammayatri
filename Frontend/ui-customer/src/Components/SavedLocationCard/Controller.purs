module Components.SavedLocationCard.Controller where

import Screens.Types(LocationListItemState, CardType(..))
import Data.Maybe (Maybe(..))

data Action = EditLocation LocationListItemState
            | DeleteLocation String
            | CardClicked LocationListItemState
            | NoAction

getCardType :: String -> Maybe CardType 
getCardType cardType = case cardType of 
  "HOME_TAG" -> Just  HOME_TAG
  "WORK_TAG" -> Just WORK_TAG
  "OTHER_TAG" -> Just OTHER_TAG
  _ -> Nothing