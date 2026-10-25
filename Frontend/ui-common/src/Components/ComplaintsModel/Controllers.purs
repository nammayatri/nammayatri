module Components.ComplaintsModel.Controller
  ( Config(..)
  , CardData(..)
  , config
  ) where

import Data.Maybe (Maybe)

type Config
  = { cardData :: Array CardData
    }

type CardData
  = { title :: String
    , subTitle :: String
    , addtionalData :: Maybe String
    }

config :: Config
config =
  { cardData: []
  }
