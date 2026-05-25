{-
  Driver-app thin shell: BPP-side wiring.

  This module routes Beckn UPDATE events and driver API calls to the shared
  flow library (mobility-flows). It runs ONLY the BPP side of each recipe.

  The actual flow logic lives in lib/mobility-flows/.
  This file is just plumbing: Beckn event → flow handler, driver API → flow handler.

  TODO Phase 2: Wire modification framework
  TODO Phase 4: Wire main flow phases
-}
module Wiring.BPP where

import Kernel.Prelude

-- Placeholder — will be populated in Phase 2
