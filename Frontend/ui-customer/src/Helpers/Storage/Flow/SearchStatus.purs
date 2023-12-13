{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Helpers.Storage.Flow.SearchStatus where

import Prelude
import Services.API (GetProfileRes(..))
import Types.App (FlowBT)
import Storage (setValueToLocalStore, KeyStore(..), getValueToLocalStore)
import Mobility.Prelude (catMaybeStrings)
import Data.Lens ((^.))
import Accessor
import Data.Maybe

updateFlowStatusStorage :: GetProfileRes -> FlowBT String Unit
updateFlowStatusStorage response = do
  let
    name = catMaybeStrings [ response ^. _firstName, response ^. _middleName, response ^. _lastName ]

    userRideStatus =  -- TODO:: Confirm with @kranti, can we fix this as code doesn't look good here because of vairable values
      if response ^. _hasTakenRide then
        "HAS_TAKEN_RIDE"
      else if (response ^. _referralCode /= Nothing && not (response ^. _hasTakenRide)) then
        "REFERRED_NOT_TAKEN_RIDE"
      else
        "NOT_REFERRED_NOT_TAKEN_RIDE"
  -- (PersonStatsRes resp) <- Remote.getPersonStatsBT "" -- TODO:: Make this function async in non critical flow @ashkriti
  setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL" --TODO:: How is this being used @rohit??
  setValueToLocalStore DISABILITY_UPDATED $ show $ isJust $ response ^. _hasDisability
  setValueToLocalStore REFERRAL_STATUS userRideStatus
  setValueToLocalStore HAS_TAKEN_FIRST_RIDE $ show $ response ^. _hasTakenRide
  setValueToLocalStore USER_NAME name
  void $ pure $ setValueToLocalStore DISABILITY_NAME <$> response ^. _disability
  void $ pure $ setValueToLocalStore USER_EMAIL <$> response ^. _email
