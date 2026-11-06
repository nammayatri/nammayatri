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
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.IssueBreach as IB
import qualified SharedLogic.DriverPool as DriverPool

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

-- | How a behaviour keeps an events window, for rulebook versions of type EVENTS ("offences
-- in the last X events"). A behaviour with this set may save and run EVENTS versions; its
-- window is written only at its own points, through
-- 'SharedLogic.BehaviourManagement.EventsWindow.recordEventsWindow', next to its
-- (unchanged) counter writes.
data EventsSupport = EventsSupport
  { -- | whose window it is, e.g. DRIVER
    windowEntity :: BTT.EntityType,
    -- | the window's key name: bt:{entity}:{key}:EVENTS:{entityId}
    windowKeyName :: Text,
    -- | where an offence goes in the window
    windowMark :: BTT.EventsWindowMark,
    -- | overlays an EVENTS version needs, in place of 'requiredOverlayKeys' (which belong to
    -- the TIME template). Their templates live under the "default" city, like the pack's other
    -- overlays.
    eventsOverlayKeys :: [Text]
  }

data BehaviourPack = BehaviourPack
  { packDomain :: LYT.LogicDomain,
    -- | overlays a TIME (daily/weekly) version needs
    requiredOverlayKeys :: [Text],
    configPrereqs :: [ConfigPrereq],
    blockReasonSeeds :: [BlockReasonSeed],
    -- | Nothing = TIME versions only
    eventsSupport :: Maybe EventsSupport
  }

behaviourPacks :: [BehaviourPack]
behaviourPacks =
  [ BehaviourPack
      { packDomain = LYT.RATING_BEHAVIOR,
        requiredOverlayKeys = ["LOW_RATING_NUDGE", "LOW_RATING_WARN"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "LOW_RATING" (Just "Blocked for sustained low customer rating") (Just 72)],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.CANCELLATION_RATE_BEHAVIOR,
        requiredOverlayKeys = ["CANCELLATION_RATE_NUDGE_DAILY", "CANCELLATION_RATE_NUDGE_WEEKLY"],
        configPrereqs = [],
        blockReasonSeeds = [],
        -- '0' = ride assigned, '1' = at-fault cancel of it (Lib.DriverScore). Overlay templates:
        -- migrations-after-release 0020.
        eventsSupport =
          Just
            EventsSupport
              { windowEntity = BTT.DRIVER,
                windowKeyName = DriverPool.rideCancellationActionType,
                windowMark = BTT.MarkLatestEligible,
                eventsOverlayKeys = ["CANCELLATION_RATE_NUDGE_EVENTS", "CANCELLATION_RATE_WARN_EVENTS"]
              }
      },
    BehaviourPack
      { packDomain = LYT.GPS_TOLL_BEHAVIOR,
        requiredOverlayKeys = [],
        configPrereqs = [ConfigPrereq "enable_gps_toll_behavior" (.enableGpsTollBehavior)],
        blockReasonSeeds = [],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.PICKUP_STALL_BEHAVIOR,
        requiredOverlayKeys = ["PICKUP_STALL_NUDGE"],
        configPrereqs = [ConfigPrereq "pickup_stall_monitoring_config" (isJust . (.pickupStallMonitoringConfig))],
        blockReasonSeeds = [BlockReasonSeed "PickupStall" (Just "Blocked for repeatedly stalling at pickup") (Just 24)],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.QUOTE_RESPONSE_BEHAVIOR,
        requiredOverlayKeys = ["QUOTE_RESPONSE_NUDGE"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "QuoteResponseBlock" (Just "Blocked for rejecting too many ride requests") (Just 4)],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.AUTO_ACCEPT_CANCELLATION_BEHAVIOR,
        requiredOverlayKeys = ["AUTO_ACCEPT_CANCELLATION_NUDGE"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "AutoAcceptCancellation" (Just "Blocked for repeatedly cancelling auto-assigned rides") (Just 24)],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.ISSUE_BREACH_BEHAVIOR,
        requiredOverlayKeys = ["EXTRA_FARE_NUDGE"],
        configPrereqs = [ConfigPrereq "issue_breach_config (EXTRA_FARE_MITIGATION entry)" hasExtraFareIssueBreachConfig],
        blockReasonSeeds =
          [ BlockReasonSeed "ExtraFareDaily" (Just "Blocked for repeated extra-fare complaints (daily)") (Just 12),
            BlockReasonSeed "ExtraFareWeekly" (Just "Blocked for repeated extra-fare complaints (weekly)") (Just 24)
          ],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.DRUNK_DRIVE_BEHAVIOR,
        requiredOverlayKeys = ["DRUNK_AND_DRIVE_WARNING"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "DrunkAndDriveViolation" (Just "Blocked for repeated drunk-and-drive reports") Nothing],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.TOLL_ISSUE_BEHAVIOR,
        requiredOverlayKeys = [],
        configPrereqs = [],
        blockReasonSeeds = [],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.AC_RESTRICTION_BEHAVIOR,
        requiredOverlayKeys = [],
        configPrereqs = [],
        blockReasonSeeds = [],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.UNHYGIENIC_VEHICLE_BEHAVIOR,
        requiredOverlayKeys = ["UNHYGIENIC_VEHICLE_WARN"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "UNHYGIENIC_VEHICLE" (Just "Blocked for repeated vehicle hygiene complaints") (Just 24)],
        eventsSupport = Nothing
      },
    BehaviourPack
      { packDomain = LYT.VEHICLE_UNSAFE_BEHAVIOR,
        requiredOverlayKeys = ["VEHICLE_UNSAFE_WARN"],
        configPrereqs = [],
        blockReasonSeeds = [BlockReasonSeed "VEHICLE_UNSAFE" (Just "Blocked for repeated vehicle safety complaints") (Just 48)],
        eventsSupport = Nothing
      }
  ]

hasExtraFareIssueBreachConfig :: DTC.TransporterConfig -> Bool
hasExtraFareIssueBreachConfig tc =
  maybe False (any ((== IB.EXTRA_FARE_MITIGATION) . (.ibIssueBreachType))) tc.issueBreachConfig

findPack :: LYT.LogicDomain -> Maybe BehaviourPack
findPack d = find (\p -> p.packDomain == d) behaviourPacks

-- | Events-window support of a behaviour (Nothing = TIME versions only).
findEventsSupport :: LYT.LogicDomain -> Maybe EventsSupport
findEventsSupport d = findPack d >>= (.eventsSupport)

-- | Behaviours that may save and run EVENTS versions.
eventsCapableDomains :: [LYT.LogicDomain]
eventsCapableDomains = [p.packDomain | p <- behaviourPacks, isJust p.eventsSupport]
