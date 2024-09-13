module Components.PlanCard.Controller where

import Data.Maybe
import Screens.Types as ST
import Services.API as API
import Components.Banner as Banner

data Action = 
    NoAction 
    | OnClick ST.PlanCardConfig
    | OfferCardBanner Banner.Action