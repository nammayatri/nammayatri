module Components.PlanCard.Controller where

import Data.Maybe
import Screens.Types as ST
import Services.API as API
import Components.Banner as Banner
import Prelude (class Show, show, (<>))

instance showAction :: Show Action where
  show (NoAction) = "NoAction"
  show (OnClick var1) = "OnClick_" <> show var1
  show (OfferCardBanner var1) = "OfferCardBanner_" <> show var1

data Action = 
    NoAction 
    | OnClick ST.PlanCardConfig
    | OfferCardBanner Banner.Action