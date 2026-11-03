{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | MG (membership) campaign perk snapshot stored on driver_information.mg_campaign.
-- Column Nothing  → not an MG member → hot path may use fleet perks.
-- Column Just     → MG member → never fall back to fleet for cancel/pooling/auto-assign;
--                   perks apply only when optedIn && validTill && milestone timeBounds.
-- Perk flags live on cohort_journey_mapping (not incentive_journey).
module Lib.IncentiveJourney.MgCampaign
  ( MgCampaignSnapshot (..),
    mkSnapshotFromCjm,
    setOptedIn,
    clearOptedIn,
    isMgPerksActive,
    decodeSnapshot,
    encodeSnapshot,
  )
where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import Kernel.Prelude
import qualified Kernel.Types.TimeBound as TB
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Window as Window

data MgCampaignSnapshot = MgCampaignSnapshot
  { journeyId :: Text,
    cohortJourneyMappingId :: Text,
    -- | False after invite / disqualify / period clear; True after successful opt-in.
    optedIn :: Bool,
    validTill :: Maybe UTCTime,
    -- | Copied from the (single) MG milestone; gates perk clock with validTill.
    timeBounds :: Maybe TB.TimeBound,
    -- | FleetConfig.allowCancellationByDriver semantics (False = cancel forbidden).
    allowCancellationByDriver :: Bool,
    autoAssign :: Maybe Value,
    poolingPriority :: Maybe Value
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

mkSnapshotFromCjm ::
  Text ->
  DCJM.CohortJourneyMapping ->
  Maybe TB.TimeBound ->
  Bool ->
  Maybe UTCTime ->
  MgCampaignSnapshot
mkSnapshotFromCjm journeyIdText cjm mbTimeBounds optedIn validTill =
  MgCampaignSnapshot
    { journeyId = journeyIdText,
      cohortJourneyMappingId = cjm.id.getId,
      optedIn = optedIn,
      validTill = validTill,
      timeBounds = mbTimeBounds,
      allowCancellationByDriver = fromMaybe True cjm.allowCancellationByDriver,
      autoAssign = cjm.autoAssign,
      poolingPriority = cjm.poolingPriority
    }

setOptedIn :: MgCampaignSnapshot -> UTCTime -> Maybe TB.TimeBound -> MgCampaignSnapshot
setOptedIn snap validTill mbTimeBounds =
  snap
    { optedIn = True,
      validTill = Just validTill,
      timeBounds = maybe snap.timeBounds Just mbTimeBounds
    }

clearOptedIn :: MgCampaignSnapshot -> MgCampaignSnapshot
clearOptedIn snap =
  snap
    { optedIn = False,
      validTill = Nothing
    }

-- | Perks ON only when membership opted in, within validTill, and inside milestone timeBounds.
isMgPerksActive :: UTCTime -> MgCampaignSnapshot -> Bool
isMgPerksActive localTime snap =
  snap.optedIn
    && maybe False (localTime <=) snap.validTill
    && Window.isWithinTimeBound localTime snap.timeBounds

decodeSnapshot :: Value -> Maybe MgCampaignSnapshot
decodeSnapshot val =
  case A.fromJSON val of
    A.Success s -> Just s
    A.Error _ -> Nothing

encodeSnapshot :: MgCampaignSnapshot -> Value
encodeSnapshot = A.toJSON
