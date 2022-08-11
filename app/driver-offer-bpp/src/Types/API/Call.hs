{-# LANGUAGE DuplicateRecordFields #-}

module Types.API.Call where

import Beckn.Types.Core.Ack
import qualified Domain.Types.CallStatus as SCS
import EulerHS.Prelude

type CallCallbackRes = AckResponse

type GetCallStatusRes = SCS.CallStatusAPIEntity

type MobileNumberResp = Text
