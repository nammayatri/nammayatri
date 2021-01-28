{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Call where

import Beckn.Types.Core.Ack (Ack (..))
import EulerHS.Prelude

newtype CallReq = CallReq
  { productInstanceId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CallRes = Ack
