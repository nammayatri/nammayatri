module Beckn.Types.Core.Provider where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude hiding (id)

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    poc :: Maybe Person
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Provider where
  example =
    Provider
      { id = idExample,
        descriptor = example,
        poc = example
      }
