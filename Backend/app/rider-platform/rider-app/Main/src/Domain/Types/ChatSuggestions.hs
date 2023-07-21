module Domain.Types.ChatSuggestions where

import Kernel.Prelude

data Messages = Messages
  { en_us :: Text,
    ta_in :: Text,
    kn_in :: Text,
    hi_in :: Text,
    ml_in :: Text,
    bn_in :: Text
  }
  deriving (Show)
