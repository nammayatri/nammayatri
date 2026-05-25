{-
  PhaseLink: bundles handler + ACL for one direction of Beckn communication.

  A Beckn action (e.g., search) involves:
    Domain handler output → ACL (toWire) → Transport → ACL (fromWire) → Domain handler input

  PhaseLink captures the ACL part: toWire + fromWire.
  In sync mode, runFlowSync chains: handler → toWire → fromWire → handler (no transport).
  In async mode, transport sits between toWire and fromWire.

  This is the bridge between domain types (different on BAP and BPP)
  and the Beckn wire format (shared protocol).
-}
module MobilityFlow.Core.PhaseLink
  ( PhaseLink (..),
    runLinkSync,
  )
where

import Kernel.Prelude

-- | One-directional ACL link: domain → wire → domain
--
-- Type params:
--   m          - monad
--   domainOut  - what the sender's handler produces (e.g., SearchRes on BAP)
--   wireMsg    - Beckn wire format (e.g., Spec.SearchReqV2 from beckn-spec)
--   domainIn   - what the receiver's handler expects (e.g., ValidatedDSearchReq on BPP)
data PhaseLink m domainOut wireMsg domainIn = PhaseLink
  { -- | ACL: sender's domain type → Beckn wire format
    -- Maps to existing functions like ACL.buildSearchReqV2
    toWire :: domainOut -> m wireMsg,
    -- | ACL: Beckn wire format → receiver's domain type
    -- Maps to existing functions like ACL.buildOnSearchReqV2
    fromWire :: wireMsg -> m domainIn
  }

-- | In sync mode: skip transport, directly chain toWire → fromWire.
-- In async mode: transport (HTTP) sits between them.
runLinkSync :: (Monad m) => PhaseLink m domainOut wireMsg domainIn -> domainOut -> m domainIn
runLinkSync link domainOut = do
  wire <- link.toWire domainOut
  link.fromWire wire
