{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | MG campaign "Assured ride" rider label: ShowAssuredLabel + an active AutoAssign / PoolingPriority perk, with the
-- perk's enableIn window in city-local (IST) time, tag expiry, campaign enabled / started, and the label-only rules
-- (not a perk for the one-campaign slot, rejected on save). Also the rider-facing driverTags shape.
module MgAssuredLabelTests (tests) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as AKM
import Data.List (sort)
import Data.Text (Text)
import Data.Time (NominalDiffTime, TimeOfDay (..), UTCTime (..), addUTCTime, fromGregorian)
import qualified Data.Vector as V
import "dynamic-offer-driver-app" Domain.Action.Internal.KnowYourDriver (withMgAssuredFlag)
import qualified "mobility-core" Kernel.Types.TimeBound as TB
import qualified "incentive-journey" Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified "incentive-journey" Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified "incentive-journey" Lib.IncentiveJourney.MgCampaign as Mg
import qualified "yudhishthira" Lib.Yudhishthira.Types as LYT
import "dynamic-offer-driver-app" SharedLogic.IncentiveJourney.CampaignControlsCache (CampaignControls (..))
import "dynamic-offer-driver-app" SharedLogic.IncentiveJourney.MgCampaign (assuredLabelFromTagsAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

-- | Thursday 2026-11-12, as a city-local wall clock (the way the campaign code carries local time).
localAt :: Int -> Int -> Int -> UTCTime
localAt h m s = UTCTime (fromGregorian 2026 11 12) (fromIntegral (h * 3600 + m * 60 + s))

-- | transporterConfig.timeDiffFromUtc for IST.
istOffset :: NominalDiffTime
istOffset = 19800

-- | The UTC instant for a city-local (IST) wall clock.
utcOf :: UTCTime -> UTCTime
utcOf = addUTCTime (negate istOffset)

-- | 07:00-11:00 every day.
morning :: TB.TimeBound
morning =
  let w = [(TimeOfDay 7 0 0, TimeOfDay 11 0 0)]
   in TB.BoundedByWeekday (TB.BoundedPeaks w w w w w w w)

feature :: Text -> Value -> Maybe Text -> Value
feature name config mbEnableIn = object (["feature" .= name, "config" .= config] <> maybe [] (\e -> ["enableIn" .= e]) mbEnableIn)

aaF, aaAllDayF, poolF, cancelF, labelF :: Value
aaF = feature "AutoAssign" (object ["defaultOn" .= True, "allowDriverToToggle" .= False]) (Just "TimeBound")
aaAllDayF = feature "AutoAssign" (object ["defaultOn" .= True, "allowDriverToToggle" .= False]) (Just "AllDay")
poolF = feature "PoolingPriority" (String "HIGH") (Just "TimeBound")
cancelF = feature "CanDxCancelBooking" (Bool False) (Just "AllDay")
labelF = feature "ShowAssuredLabel" (Bool True) Nothing

features :: [Value] -> Maybe Value
features = Just . Array . V.fromList

cjm :: Text
cjm = "cjm-assured-1"

-- | Mirrors CampaignControlsCache.toControls (campaign started 2026-11-12 00:00 local, first milestone = morning).
mkControls :: Bool -> UTCTime -> Maybe Value -> CampaignControls
mkControls enabled startDate controlFeatures =
  let parsed = Mg.parseControlFeatures controlFeatures
   in CampaignControls
        { cjmId = cjm,
          enabled = enabled,
          campaignUserLinkType = Just DCJM.OptIn,
          controlFeatures = controlFeatures,
          timeBounds = Just morning,
          trackBehaviorMetrics = False,
          journeyType = DIJ.Daily,
          campaignStartDate = startDate,
          allowCancellationByDriver = parsed.allowCancellationByDriver,
          cancelEnableIn = parsed.cancelEnableIn,
          autoAssign = parsed.autoAssign,
          autoAssignEnableIn = parsed.autoAssignEnableIn,
          poolingPriority = parsed.poolingPriority,
          poolingEnableIn = parsed.poolingEnableIn,
          preferencePolicy = parsed.preferencePolicy,
          preferencePolicyEnableIn = parsed.preferencePolicyEnableIn,
          showAssuredLabel = parsed.showAssuredLabel
        }

-- | An opted-in membership tag, expiring at the given UTC time.
tagExpiring :: Text -> [LYT.TagNameValueExpiry]
tagExpiring expiry = [LYT.TagNameValueExpiry ("IJCampaign:" <> cjm <> "#OptIn;daily#" <> expiry)]

validTag :: [LYT.TagNameValueExpiry]
validTag = tagExpiring "2026-11-14T00:00:00"

-- | The label at a city-local wall clock (tag expiry checked against the matching UTC instant).
labelAt :: UTCTime -> [LYT.TagNameValueExpiry] -> CampaignControls -> Bool
labelAt local tags controls = assuredLabelFromTagsAt (utcOf local) local tags (\c -> if c == cjm then Just controls else Nothing)

started :: UTCTime
started = localAt 0 0 0

tests :: TestTree
tests =
  testGroup
    "MG assured label"
    [ testCase "AutoAssign TimeBound + label, inside the window: on" $
        labelAt (localAt 8 0 0) validTag (mkControls True started (features [aaF, labelF])) @?= True,
      testCase "outside the window: off" $
        labelAt (localAt 12 0 0) validTag (mkControls True started (features [aaF, labelF])) @?= False,
      testCase "window start is exclusive (07:00:00 off, 07:00:01 on)" $
        ( labelAt (localAt 7 0 0) validTag (mkControls True started (features [aaF, labelF])),
          labelAt (localAt 7 0 1) validTag (mkControls True started (features [aaF, labelF]))
        )
          @?= (False, True),
      testCase "window end is exclusive (10:59:59 on, 11:00:00 off)" $
        ( labelAt (localAt 10 59 59) validTag (mkControls True started (features [aaF, labelF])),
          labelAt (localAt 11 0 0) validTag (mkControls True started (features [aaF, labelF]))
        )
          @?= (True, False),
      testCase "IST: 02:30Z is 08:00 local (on); the same UTC clock read as local would be off" $
        let controls = mkControls True started (features [aaF, labelF])
            utcNow = UTCTime (fromGregorian 2026 11 12) (fromIntegral (2 * 3600 + 30 * 60 :: Int))
            lookupC c = if c == cjm then Just controls else Nothing
         in ( assuredLabelFromTagsAt utcNow (addUTCTime istOffset utcNow) validTag lookupC,
              assuredLabelFromTagsAt utcNow utcNow validTag lookupC
            )
              @?= (True, False),
      testCase "AutoAssign AllDay: on outside the milestone window" $
        labelAt (localAt 15 0 0) validTag (mkControls True started (features [aaAllDayF, labelF])) @?= True,
      testCase "PoolingPriority perk + label, inside the window: on" $
        labelAt (localAt 9 0 0) validTag (mkControls True started (features [poolF, labelF])) @?= True,
      testCase "only a cancel perk + label: off (label follows AutoAssign / PoolingPriority)" $
        labelAt (localAt 9 0 0) validTag (mkControls True started (features [cancelF, labelF])) @?= False,
      testCase "no ShowAssuredLabel: off" $
        labelAt (localAt 8 0 0) validTag (mkControls True started (features [aaF])) @?= False,
      testCase "ShowAssuredLabel false: off" $
        labelAt (localAt 8 0 0) validTag (mkControls True started (features [aaF, feature "ShowAssuredLabel" (Bool False) Nothing])) @?= False,
      testCase "expired tag: off" $
        labelAt (localAt 8 0 0) (tagExpiring "2026-11-12T01:00:00") (mkControls True started (features [aaF, labelF])) @?= False,
      testCase "disabled campaign: off" $
        labelAt (localAt 8 0 0) validTag (mkControls False started (features [aaF, labelF])) @?= False,
      testCase "campaign not started yet: off" $
        labelAt (localAt 8 0 0) validTag (mkControls True (localAt 9 0 0) (features [aaF, labelF])) @?= False,
      testCase "no membership tag: off" $
        labelAt (localAt 8 0 0) [] (mkControls True started (features [aaF, labelF])) @?= False,
      testCase "label-only campaign: not a perk campaign, rejected on save" $
        ( Mg.valueHasControlFeatures (features [labelF]),
          Mg.assuredLabelWithoutPerk (features [labelF])
        )
          @?= (False, True),
      testCase "label next to a perk: a perk campaign, accepted" $
        ( Mg.valueHasControlFeatures (features [cancelF, labelF]),
          Mg.assuredLabelWithoutPerk (features [cancelF, labelF]),
          Mg.assuredLabelWithoutPerk (features [aaF])
        )
          @?= (True, False, False),
      testCase "config {\"enabled\": true} also switches the label on" $
        (Mg.parseControlFeatures (features [feature "ShowAssuredLabel" (object ["enabled" .= True]) Nothing])).showAssuredLabel @?= True,
      testCase "rider driverTags: IJCampaign keys dropped, MgAssured added, other tags kept" $
        let tags =
              object
                [ "IJCampaign:cjm-assured-1" .= ("OptIn;daily" :: Text),
                  "FleetDriverEnabled" .= ("true" :: Text),
                  "SafetyCohort" .= ("New" :: Text)
                ]
            keys v = case v of
              Object o -> sort (AKM.keys o)
              _ -> []
         in (keys (withMgAssuredFlag True tags), keys (withMgAssuredFlag False tags))
              @?= (["FleetDriverEnabled", "MgAssured", "SafetyCohort"], ["FleetDriverEnabled", "SafetyCohort"]),
      testCase "rider driverTags: a driver-written MgAssured is dropped, and set only when the label is on" $
        let tags = object ["MgAssured" .= ("true" :: Text), "FleetDriverEnabled" .= ("true" :: Text)]
         in (withMgAssuredFlag False tags, withMgAssuredFlag True (object ["MgAssured" .= ("false" :: Text)]))
              @?= (object ["FleetDriverEnabled" .= ("true" :: Text)], object ["MgAssured" .= ("true" :: Text)])
    ]
