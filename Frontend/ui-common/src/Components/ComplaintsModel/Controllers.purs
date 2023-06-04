module Components.ComplaintsModel.Controller
  ( Config(..)
  , CardData(..)
  , config
  ) where

import Data.Maybe (Maybe)

type Config
  = { cardData :: Array CardData
    , privacyPolicyLink :: String 
    }

type CardData
  = { title :: String
    , subTitle :: String
    , addtionalData :: Maybe String
    }

config :: Config
config =
  { cardData: []
  , privacyPolicyLink : ""
  }
