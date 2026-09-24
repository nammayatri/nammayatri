module SharedLogic.SharedCab.LegState
  ( isSharedCabAgency,
  )
where

import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import Kernel.Prelude

-- | Shared cabs ship in GTFS under the SHARED_CAB agency (agency gtfsId `<feed>:SHARED_CAB`).
isSharedCabAgency :: Text -> Bool
isSharedCabAgency agencyGtfsId = gtfsIdtoDomainCode agencyGtfsId == "SHARED_CAB"
