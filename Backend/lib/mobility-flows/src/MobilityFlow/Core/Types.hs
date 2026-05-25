{-
  Core types for the mobility flow building blocks.

  Design principles:
  - No Value types — everything is strongly typed
  - No Maybe hacks — different phases use appropriate context types
  - Types align with existing codebase (BookingStatus, RideStatus from beckn-spec)
  - Flow is parameterized by domain types, works with existing Booking, Ride, etc.
-}
module MobilityFlow.Core.Types
  ( -- * Phase definition
    Side (..),

    -- * Modification sync models
    SyncModel (..),
    DriverPromptConfig (..),
    BPPOutcome (..),
    DriverPromptData (..),
  )
where

import Kernel.Prelude

-- | Which side of the Beckn protocol a handler runs on.
data Side = BAP | BPP
  deriving (Show, Eq, Generic)

-- | How BAP and BPP coordinate for a mid-flow modification.
data SyncModel
  = -- | BPP processes + notifies driver. No callback to BAP.
    Immediate
  | -- | BPP processes + sends on_update callback. BAP applies changes.
    FireAndForget
  | -- | BPP calculates impact, shows to driver. Driver accepts/rejects.
    WithDriverApproval DriverPromptConfig
  deriving (Show, Eq, Generic)

data DriverPromptConfig = DriverPromptConfig
  { overlayKey :: Text,
    expirySeconds :: Int
  }
  deriving (Show, Eq, Generic)

-- | What the BPP handler returns after processing a modification.
-- Parameterized by the callback payload type (fully typed, no Value).
data BPPOutcome m callbackPayload
  = Done
  | SendCallback callbackPayload
  | ShowToDriver
      DriverPromptData
      (m callbackPayload) -- onAccept
      (m callbackPayload) -- onReject

-- | Data shown to driver in the approval prompt.
data DriverPromptData = DriverPromptData
  { promptTitle :: Text,
    promptDescription :: Text
  }
  deriving (Show, Generic)
