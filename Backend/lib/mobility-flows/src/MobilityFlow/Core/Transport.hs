{-
  Transport abstraction: generic Beckn communication channel.

  Handles ALL Beckn action pairs (search/on_search, select/on_select,
  confirm/on_confirm, update/on_update, etc.)

  In production (BecknHTTP): sends HTTP, receives callbacks on separate endpoints.
  In sync mode (DirectCall): direct function call, both sides in one process.

  The transport is parameterized by message type — each Beckn action
  has its own strongly-typed message. No Value.
-}
module MobilityFlow.Core.Transport
  ( FlowTransport (..),
  )
where

import Kernel.Prelude

-- | Abstraction over BAP <-> BPP communication.
--
-- Each Beckn action pair has its own typed messages. The transport
-- doesn't care about the message contents — it just delivers them.
--
-- In production, the instance serializes msg to Beckn JSON and sends HTTP.
-- In sync mode, the instance directly calls the other side's handler.
class FlowTransport t m where
  -- | BAP -> BPP: send any Beckn action message
  sendToBPP :: (Show msg) => t -> msg -> m ()

  -- | BPP -> BAP: send any Beckn callback message
  sendToBAP :: (Show msg) => t -> msg -> m ()
