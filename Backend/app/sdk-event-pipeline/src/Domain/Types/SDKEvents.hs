{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SDKEvents where

import Data.Aeson
import Kernel.Prelude
import Tools.Auth (ClientType (..))

data SDKEventsReq = SDKEventsReq
  { event :: Text,
    clientType :: Maybe ClientType
  }
  deriving (Generic, ToJSON, FromJSON)
