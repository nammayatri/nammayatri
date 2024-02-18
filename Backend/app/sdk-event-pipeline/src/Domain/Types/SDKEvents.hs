{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SDKEvents where

import Data.Text
import Kernel.Prelude

data SDKEventsReq = SDKEventsReq
  { event :: Text
  }
  deriving (Generic, ToJSON, FromJSON)
