{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Decides at /rideSearch dispatch time whether to fire the parallel sync
-- search to the onUs BPP. Configuration types live in
-- 'Domain.Types.Extra.RiderConfig' so they can be referenced from the
-- RiderConfig storage YAML; this module hosts only the decision logic.
module SharedLogic.SyncSearchDispatch
  ( shouldDispatchSync,
  )
where

import qualified Domain.Types.Extra.RiderConfig as ERC
import Kernel.Prelude
import Kernel.Types.Id (Id)
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.Search as SLS

ruleForRequest :: ERC.SyncSearchDispatchConfig -> SLS.SearchReq -> Maybe ERC.RequestKindRule
ruleForRequest cfg = \case
  SLS.OneWaySearch _ -> cfg.oneWay
  SLS.RentalSearch _ -> cfg.rental
  SLS.InterCitySearch _ -> cfg.interCity
  SLS.AmbulanceSearch _ -> cfg.ambulance
  SLS.DeliverySearch _ -> cfg.delivery
  SLS.PTSearch _ -> cfg.publicTransport
  SLS.FixedRouteSearch _ -> cfg.fixedRoute
  -- No dedicated sync-dispatch config field for EasyBooking yet; Nothing here
  -- just means "no rule configured" -> shouldDispatchSync falls through to
  -- its own `Just cfg -> ... _ -> False` default (async dispatch, same as
  -- any other unconfigured category).
  SLS.EasyBookingSearch _ -> Nothing

shouldDispatchSync ::
  Maybe ERC.SyncSearchDispatchConfig ->
  SLS.SearchReq ->
  Maybe (Id SL.SpecialLocation) ->
  Bool
shouldDispatchSync Nothing _ _ = False
shouldDispatchSync (Just cfg) req mbFromSpecialLocId =
  case ruleForRequest cfg req of
    Just rule | rule.enabled -> matchesScope (fromMaybe ERC.AllRides rule.scope) mbFromSpecialLocId
    _ -> False

matchesScope :: ERC.SyncDispatchScope -> Maybe (Id SL.SpecialLocation) -> Bool
matchesScope ERC.AllRides _ = True
matchesScope (ERC.SpecialLocationOnly mbAllowed) mbSId =
  case (mbAllowed, mbSId) of
    (_, Nothing) -> False
    (Nothing, Just _) -> True
    (Just allowed, Just sId) -> sId `elem` allowed
