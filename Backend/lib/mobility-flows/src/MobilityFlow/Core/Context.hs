{-
  Flow context types: per-session data that's stable for the duration of a flow.

  Parameterized by entity types so we use real Kernel.Types.Id without
  depending on rider-app/driver-app domain types directly.

  When wired in rider-app:
    BAPContext Person Client
  When wired in driver-app:
    BPPContext Merchant MerchantOperatingCity
-}
module MobilityFlow.Core.Context
  ( BAPContext (..),
    BPPContext (..),
    ClientInfo (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Types.Version (Version)

-- | BAP-side context: created once when a rider initiates a flow.
-- Captures everything about the rider's session that doesn't change per phase.
--
-- Parameterized by:
--   person - rider entity type (Domain.Types.Person in rider-app)
--   client - client app entity type (Domain.Types.Client in rider-app)
data BAPContext person client = BAPContext
  { riderId :: Id person,
    clientInfo :: ClientInfo client,
    isDashboardRequest :: Bool
  }

-- | Client app metadata (versions, device info).
-- Extracted from the various Maybe Version params scattered across handlers.
data ClientInfo client = ClientInfo
  { bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientId :: Maybe (Id client),
    device :: Maybe Text
  }

-- | BPP-side context: created once when BPP processes a flow.
-- Captures merchant/operator info that's stable across all phases.
--
-- Parameterized by:
--   merchant      - merchant entity type (Domain.Types.Merchant in driver-app)
--   merchantOpCity - operating city type (Domain.Types.MerchantOperatingCity)
data BPPContext merchant merchantOpCity = BPPContext
  { bppMerchant :: merchant,
    bppMerchantOpCityId :: Id merchantOpCity
  }
