{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | City BF (bt:) action-type wire strings — single source of truth for Redis keys
--   bt:DRIVER:<actionType>:<counterType>:<driverId>.
--
-- Campaign tick names that share the same wire strings (quote receive/accept,
-- ride cancellation) MUST reuse these constants via 'CampaignNames.campaignTickActionText'.
-- Do not redefine these literals in CampaignQuote / CampaignCancel — a rename there
-- would silently move every driver's BF series and zero cancellation/acceptance rates.
module Lib.BehaviorTracker.ActionTypes
  ( quoteResponseActionType,
    quoteResponseAcceptActionType,
    rideCancellationActionType,
  )
where

import Kernel.Prelude

-- | SRD send / shared eligibility series (ELIGIBLE_COUNT). Also campaign receive tick.
quoteResponseActionType :: Text
quoteResponseActionType = "QUOTE_RESPONSE"

-- | Quote Accept ACTION_COUNT series. Also campaign accept tick.
quoteResponseAcceptActionType :: Text
quoteResponseAcceptActionType = "QUOTE_RESPONSE_ACCEPT"

-- | Cancellation ACTION_COUNT / rides ELIGIBLE_COUNT series. Also campaign driver-cancel tick.
rideCancellationActionType :: Text
rideCancellationActionType = "RIDE_CANCELLATION"
