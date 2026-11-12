{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign control snapshot helpers (allocator/cancel/AA paths).
-- Membership gate is IJCampaign person tag; controlFeatures live on CJM (pod in-mem cache).
-- Perks apply when optedIn && validTill && timeBounds (tag expiry gates reads).
module Lib.IncentiveJourney.MgCampaign
  ( MgCampaignSnapshot (..),
    mkSnapshotFromCjm,
    setOptedIn,
    clearOptedIn,
    isMgPerksActive,
    decodeSnapshot,
    encodeSnapshot,
    cjmHasControlFeatures,
    valueHasControlFeatures,
    parseControlFeatures,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Vector as V
import Kernel.Prelude
import qualified Kernel.Types.TimeBound as TB
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Window as Window

data MgCampaignSnapshot = MgCampaignSnapshot
  { journeyId :: Text,
    cohortJourneyMappingId :: Text,
    -- | False after invite / period clear; True after successful opt-in.
    optedIn :: Bool,
    validTill :: Maybe UTCTime,
    -- | Copied from first milestone; gates perk clock with validTill when enableIn=TimeBound.
    timeBounds :: Maybe TB.TimeBound,
    -- | FleetConfig.allowCancellationByDriver semantics (False = cancel forbidden).
    allowCancellationByDriver :: Bool,
    autoAssign :: Maybe Value,
    poolingPriority :: Maybe Value
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

valueHasControlFeatures :: Maybe Value -> Bool
valueHasControlFeatures = \case
  Just (A.Array xs) -> not (V.null xs)
  Just _ -> True
  Nothing -> False

cjmHasControlFeatures :: DCJM.CohortJourneyMapping -> Bool
cjmHasControlFeatures cjm = valueHasControlFeatures cjm.controlFeatures

-- | Map CJM.controlFeatures into the legacy DI snapshot shape for allocator/cancel paths.
mkSnapshotFromCjm ::
  Text ->
  DCJM.CohortJourneyMapping ->
  Maybe TB.TimeBound ->
  Bool ->
  Maybe UTCTime ->
  MgCampaignSnapshot
mkSnapshotFromCjm journeyIdText cjm mbTimeBounds optedIn validTill =
  let (allowCancel, mbAA, mbPooling) = parseControlFeatures cjm.controlFeatures
   in MgCampaignSnapshot
        { journeyId = journeyIdText,
          cohortJourneyMappingId = cjm.id.getId,
          optedIn = optedIn,
          validTill = validTill,
          timeBounds = mbTimeBounds,
          allowCancellationByDriver = allowCancel,
          autoAssign = mbAA,
          poolingPriority = mbPooling
        }

parseControlFeatures :: Maybe Value -> (Bool, Maybe Value, Maybe Value)
parseControlFeatures = \case
  Just (A.Array xs) -> foldl' step (True, Nothing, Nothing) (V.toList xs)
  _ -> (True, Nothing, Nothing)
  where
    step (allowCancel, mbAA, mbPooling) item =
      case item of
        A.Object o ->
          let feature = case AKM.lookup (AKey.fromText "feature") o of
                Just (A.String f) -> f
                _ -> ""
              config = AKM.lookup (AKey.fromText "config") o
           in case feature of
                "CanDxCancelBooking" ->
                  let allow = case config of
                        Just (A.Bool b) -> b
                        Just (A.Object co) ->
                          case AKM.lookup (AKey.fromText "enabled") co of
                            Just (A.Bool b) -> b
                            _ -> allowCancel
                        _ -> allowCancel
                   in (allow, mbAA, mbPooling)
                "AutoAssign" -> (allowCancel, config <|> mbAA, mbPooling)
                "PoolingPriority" -> (allowCancel, mbAA, config <|> mbPooling)
                _ -> (allowCancel, mbAA, mbPooling)
        _ -> (allowCancel, mbAA, mbPooling)

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

-- | Perks ON when optedIn, not past validTill (Nothing = open-ended / tag-gated), and inside timeBounds.
isMgPerksActive :: UTCTime -> MgCampaignSnapshot -> Bool
isMgPerksActive localTime snap =
  snap.optedIn
    && maybe True (localTime <=) snap.validTill
    && Window.isWithinTimeBound localTime snap.timeBounds

decodeSnapshot :: Value -> Maybe MgCampaignSnapshot
decodeSnapshot val =
  case A.fromJSON val of
    A.Success s -> Just s
    A.Error _ -> Nothing

encodeSnapshot :: MgCampaignSnapshot -> Value
encodeSnapshot = A.toJSON
