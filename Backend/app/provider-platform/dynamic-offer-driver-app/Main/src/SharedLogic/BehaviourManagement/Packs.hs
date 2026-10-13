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
import qualified SharedLogic.BehaviourManagement.IssueBreach as IB

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
      },
    BehaviourPack
      { packDomain = LYT.PICKUP_STALL_BEHAVIOR,
        requiredOverlayKeys = ["PICKUP_STALL_NUDGE"],
        configPrereqs = [ConfigPrereq "pickup_stall_monitoring_config" (isJust . (.pickupStallMonitoringConfig))],
        blockReasonSeeds = [BlockReasonSeed "PickupStall" (Just "Blocked for repeatedly stalling at pickup") (Just 24)]
      },
    BehaviourPack
      { packDomain = LYT.QUOTE_RESPONSE_BEHAVIOR,
        requiredOverlayKeys = ["QUOTE_RESPONSE_NUDGE"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "QuoteResponseBlock" (Just "Blocked for rejecting too many ride requests") (Just 4)]
      },
    BehaviourPack
      { packDomain = LYT.AUTO_ACCEPT_CANCELLATION_BEHAVIOR,
        requiredOverlayKeys = ["AUTO_ACCEPT_CANCELLATION_NUDGE"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "AutoAcceptCancellation" (Just "Blocked for repeatedly cancelling auto-assigned rides") (Just 24)]
      },
    BehaviourPack
      { packDomain = LYT.ISSUE_BREACH_BEHAVIOR,
        requiredOverlayKeys = ["EXTRA_FARE_NUDGE"],
        configPrereqs = [ConfigPrereq "issue_breach_config (EXTRA_FARE_MITIGATION entry)" hasExtraFareIssueBreachConfig],
        blockReasonSeeds =
          [ BlockReasonSeed "ExtraFareDaily" (Just "Blocked for repeated extra-fare complaints (daily)") (Just 12),
            BlockReasonSeed "ExtraFareWeekly" (Just "Blocked for repeated extra-fare complaints (weekly)") (Just 24)
          ]
      },
    BehaviourPack
      { packDomain = LYT.DRUNK_DRIVE_BEHAVIOR,
        requiredOverlayKeys = ["DRUNK_AND_DRIVE_WARNING"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "DrunkAndDriveViolation" (Just "Blocked for repeated drunk-and-drive reports") Nothing]
      },
    BehaviourPack
      { packDomain = LYT.TOLL_ISSUE_BEHAVIOR,
        requiredOverlayKeys = [],
        configPrereqs = [],
        blockReasonSeeds = []
      },
    BehaviourPack
      { packDomain = LYT.AC_RESTRICTION_BEHAVIOR,
        requiredOverlayKeys = [],
        configPrereqs = [],
        blockReasonSeeds = []
      },
    BehaviourPack
      { packDomain = LYT.UNHYGIENIC_VEHICLE_BEHAVIOR,
        requiredOverlayKeys = ["UNHYGIENIC_VEHICLE_WARN"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "UNHYGIENIC_VEHICLE" (Just "Blocked for repeated vehicle hygiene complaints") (Just 24)]
      },
    BehaviourPack
      { packDomain = LYT.VEHICLE_UNSAFE_BEHAVIOR,
        requiredOverlayKeys = ["VEHICLE_UNSAFE_WARN"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "VEHICLE_UNSAFE" (Just "Blocked for repeated vehicle safety complaints") (Just 48)]
      }
  ]

hasExtraFareIssueBreachConfig :: DTC.TransporterConfig -> Bool
hasExtraFareIssueBreachConfig tc =
  maybe False (any ((== IB.EXTRA_FARE_MITIGATION) . (.ibIssueBreachType))) tc.issueBreachConfig

findPack :: LYT.LogicDomain -> Maybe BehaviourPack
findPack d = find (\p -> p.packDomain == d) behaviourPacks
