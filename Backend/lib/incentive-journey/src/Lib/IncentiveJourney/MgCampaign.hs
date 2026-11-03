{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign control snapshot helpers (allocator/cancel/AA paths).
-- Membership gate is IJCampaign person tag; controlFeatures live on CJM (pod in-mem cache).
-- Membership (optedIn + validTill) occupies the fleet slot; each feature's enableIn
-- (AllDay | TimeBound) gates that perk against milestone timeBounds independently.
module Lib.IncentiveJourney.MgCampaign
  ( ControlFeatureEnableIn (..),
    ParsedControlFeatures (..),
    MgCampaignSnapshot (..),
    mkSnapshotFromCjm,
    setOptedIn,
    clearOptedIn,
    isMgMembershipActive,
    isMgPerksActive,
    isControlFeatureActive,
    decodeSnapshot,
    encodeSnapshot,
    cjmHasControlFeatures,
    valueHasControlFeatures,
    parseControlFeatures,
    defaultParsedControlFeatures,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Vector as V
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import qualified Kernel.Types.TimeBound as TB
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Window as Window

-- | Per-feature window from controlFeatures[].enableIn.
-- AllDay  — perk while membership is active (ignore milestone peaks).
-- TimeBound — perk only inside first-milestone timeBounds (Unbounded/Nothing ⇒ always).
-- Missing / unknown enableIn defaults to TimeBound (legacy shared peak gate).
data ControlFeatureEnableIn = AllDay | TimeBound
  deriving (Generic, Show, Eq)

instance ToJSON ControlFeatureEnableIn where
  toJSON AllDay = A.String "AllDay"
  toJSON TimeBound = A.String "TimeBound"

instance FromJSON ControlFeatureEnableIn where
  parseJSON = A.withText "ControlFeatureEnableIn" $ \t ->
    case T.toLower t of
      "allday" -> pure AllDay
      "timebound" -> pure TimeBound
      _ -> pure TimeBound

data ParsedControlFeatures = ParsedControlFeatures
  { allowCancellationByDriver :: Bool,
    cancelEnableIn :: ControlFeatureEnableIn,
    autoAssign :: Maybe Value,
    autoAssignEnableIn :: ControlFeatureEnableIn,
    poolingPriority :: Maybe Value,
    poolingEnableIn :: ControlFeatureEnableIn
  }
  deriving (Generic, Show, Eq)

defaultParsedControlFeatures :: ParsedControlFeatures
defaultParsedControlFeatures =
  ParsedControlFeatures
    { allowCancellationByDriver = True,
      cancelEnableIn = TimeBound,
      autoAssign = Nothing,
      autoAssignEnableIn = TimeBound,
      poolingPriority = Nothing,
      poolingEnableIn = TimeBound
    }

data MgCampaignSnapshot = MgCampaignSnapshot
  { journeyId :: Text,
    cohortJourneyMappingId :: Text,
    -- | False after invite / period clear; True after successful opt-in.
    optedIn :: Bool,
    validTill :: Maybe UTCTime,
    -- | First milestone timeBounds; used when a feature's enableIn = TimeBound.
    timeBounds :: Maybe TB.TimeBound,
    -- | FleetConfig.allowCancellationByDriver semantics (False = cancel forbidden).
    allowCancellationByDriver :: Bool,
    cancelEnableIn :: ControlFeatureEnableIn,
    autoAssign :: Maybe Value,
    autoAssignEnableIn :: ControlFeatureEnableIn,
    poolingPriority :: Maybe Value,
    poolingEnableIn :: ControlFeatureEnableIn
  }
  deriving (Generic, Show, Eq)

instance ToJSON MgCampaignSnapshot where
  toJSON MgCampaignSnapshot {..} =
    A.object
      [ "journeyId" A..= journeyId,
        "cohortJourneyMappingId" A..= cohortJourneyMappingId,
        "optedIn" A..= optedIn,
        "validTill" A..= validTill,
        "timeBounds" A..= timeBounds,
        "allowCancellationByDriver" A..= allowCancellationByDriver,
        "cancelEnableIn" A..= cancelEnableIn,
        "autoAssign" A..= autoAssign,
        "autoAssignEnableIn" A..= autoAssignEnableIn,
        "poolingPriority" A..= poolingPriority,
        "poolingEnableIn" A..= poolingEnableIn
      ]

instance FromJSON MgCampaignSnapshot where
  parseJSON = A.withObject "MgCampaignSnapshot" $ \o -> do
    journeyId <- o A..: "journeyId"
    cohortJourneyMappingId <- o A..: "cohortJourneyMappingId"
    optedIn <- o A..: "optedIn"
    validTill <- o A..:? "validTill"
    timeBounds <- o A..:? "timeBounds"
    allowCancellationByDriver <- o A..:? "allowCancellationByDriver" A..!= True
    cancelEnableIn <- o A..:? "cancelEnableIn" A..!= TimeBound
    autoAssign <- o A..:? "autoAssign"
    autoAssignEnableIn <- o A..:? "autoAssignEnableIn" A..!= TimeBound
    poolingPriority <- o A..:? "poolingPriority"
    poolingEnableIn <- o A..:? "poolingEnableIn" A..!= TimeBound
    pure MgCampaignSnapshot {..}

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
  let parsed = parseControlFeatures cjm.controlFeatures
   in MgCampaignSnapshot
        { journeyId = journeyIdText,
          cohortJourneyMappingId = Id.getId cjm.id,
          optedIn = optedIn,
          validTill = validTill,
          timeBounds = mbTimeBounds,
          allowCancellationByDriver = parsed.allowCancellationByDriver,
          cancelEnableIn = parsed.cancelEnableIn,
          autoAssign = parsed.autoAssign,
          autoAssignEnableIn = parsed.autoAssignEnableIn,
          poolingPriority = parsed.poolingPriority,
          poolingEnableIn = parsed.poolingEnableIn
        }

parseEnableIn :: Maybe Value -> ControlFeatureEnableIn
parseEnableIn = \case
  Just (A.String t) ->
    case T.toLower t of
      "allday" -> AllDay
      "timebound" -> TimeBound
      _ -> TimeBound
  _ -> TimeBound

-- | Parse controlFeatures JSON. enableIn is per feature (default TimeBound when omitted).
parseControlFeatures :: Maybe Value -> ParsedControlFeatures
parseControlFeatures = \case
  Just (A.Array xs) -> foldl' step defaultParsedControlFeatures (V.toList xs)
  _ -> defaultParsedControlFeatures
  where
    -- Explicit type: ParsedControlFeatures shares field names with MgCampaignSnapshot.
    step :: ParsedControlFeatures -> Value -> ParsedControlFeatures
    step acc item =
      case item of
        A.Object o ->
          let feature = case AKM.lookup (AKey.fromText "feature") o of
                Just (A.String f) -> f
                _ -> ""
              config = AKM.lookup (AKey.fromText "config") o
              enableIn = parseEnableIn (AKM.lookup (AKey.fromText "enableIn") o)
           in case feature of
                "CanDxCancelBooking" ->
                  let allow = case config of
                        Just (A.Bool b) -> b
                        Just (A.Object co) ->
                          case AKM.lookup (AKey.fromText "enabled") co of
                            Just (A.Bool b) -> b
                            _ -> acc.allowCancellationByDriver
                        _ -> acc.allowCancellationByDriver
                   in acc{allowCancellationByDriver = allow,
                          cancelEnableIn = enableIn
                         }
                "AutoAssign" ->
                  acc{autoAssign = config <|> acc.autoAssign,
                      autoAssignEnableIn = enableIn
                     }
                "PoolingPriority" ->
                  acc{poolingPriority = config <|> acc.poolingPriority,
                      poolingEnableIn = enableIn
                     }
                _ -> acc
        _ -> acc

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

-- | Membership ON when optedIn and not past validTill (Nothing = open-ended / tag-gated).
-- Does not apply enableIn / timeBounds — those gate individual perks.
isMgMembershipActive :: UTCTime -> MgCampaignSnapshot -> Bool
isMgMembershipActive localTime snap =
  snap.optedIn
    && maybe True (localTime <=) snap.validTill

-- | Whether a single controlFeatures entry's enableIn window is open.
isControlFeatureActive :: ControlFeatureEnableIn -> UTCTime -> Maybe TB.TimeBound -> Bool
isControlFeatureActive AllDay _ _ = True
isControlFeatureActive TimeBound localTime mbTb = Window.isWithinTimeBound localTime mbTb

-- | Legacy helper: membership + TimeBound window (old “all perks share peak” gate).
-- Prefer isMgMembershipActive + isControlFeatureActive per feature.
isMgPerksActive :: UTCTime -> MgCampaignSnapshot -> Bool
isMgPerksActive localTime snap =
  isMgMembershipActive localTime snap
    && isControlFeatureActive TimeBound localTime snap.timeBounds

decodeSnapshot :: Value -> Maybe MgCampaignSnapshot
decodeSnapshot val =
  case A.fromJSON val of
    A.Success s -> Just s
    A.Error _ -> Nothing

encodeSnapshot :: MgCampaignSnapshot -> Value
encodeSnapshot = A.toJSON
