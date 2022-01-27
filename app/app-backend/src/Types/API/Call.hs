{-# LANGUAGE DuplicateRecordFields #-}

module Types.API.Call where

import qualified Beckn.External.Exotel.Types as Call
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import qualified Domain.Types.CallStatus as DCS
import EulerHS.Prelude

newtype CallRes = CallRes
  { callId :: Id DCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = Call.ExotelCallCallback

type CallCallbackRes = AckResponse

type GetCallStatusRes = DCS.CallStatusAPIEntity
