{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Per-domain enablement bundles for the behaviour engine. A behaviour is enabled in a
-- city by applying its pack (rollout + overlays + prereq checks + block reasons), never
-- by hand-assembling the pieces. The canonical rulebook is NOT part of the pack: it lives
-- in the DB as the (domain, version) referenced by the "default"-city 0% base rollout.
module SharedLogic.BehaviourManagement.Packs where

import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types as LYT

-- | A transporter_config precondition the behaviour needs before its rules can fire.
-- v1 only CHECKS prereqs (status endpoint + enable-time validation with an actionable
-- error); typed setters can be added per toggle when a behaviour that needs one ships.
data ConfigPrereq = ConfigPrereq
  { prereqName :: Text,
    isSatisfied :: DTC.TransporterConfig -> Bool
  }

data BlockReasonSeed = BlockReasonSeed
  { seedReasonCode :: Text,
    seedReasonDescription :: Maybe Text,
    seedBlockTimeInHours :: Maybe Int
  }

data BehaviourPack = BehaviourPack
  { packDomain :: LYT.LogicDomain,
    requiredOverlayKeys :: [Text],
    configPrereqs :: [ConfigPrereq],
    blockReasonSeeds :: [BlockReasonSeed]
  }

behaviourPacks :: [BehaviourPack]
behaviourPacks =
  [ BehaviourPack
      { packDomain = LYT.RATING_BEHAVIOR,
        requiredOverlayKeys = ["LOW_RATING_NUDGE", "LOW_RATING_WARN"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "LOW_RATING" (Just "Blocked for sustained low customer rating") (Just 72)]
      },
    BehaviourPack
      { packDomain = LYT.CANCELLATION_RATE_BEHAVIOR,
        requiredOverlayKeys = ["CANCELLATION_RATE_NUDGE_DAILY", "CANCELLATION_RATE_NUDGE_WEEKLY"],
        configPrereqs = [],
        blockReasonSeeds = []
      },
    BehaviourPack
      { packDomain = LYT.GPS_TOLL_BEHAVIOR,
        requiredOverlayKeys = [],
        configPrereqs = [ConfigPrereq "enable_gps_toll_behavior" (.enableGpsTollBehavior)],
        blockReasonSeeds = []
      }
  ]

findPack :: LYT.LogicDomain -> Maybe BehaviourPack
findPack d = find (\p -> p.packDomain == d) behaviourPacks
