module Components.LocationTagBar.Controller where

import Screens.Types (CardType, LocationListItemState)
import Data.Maybe (Maybe(..))

data Action = TagClick CardType (Maybe LocationListItemState)