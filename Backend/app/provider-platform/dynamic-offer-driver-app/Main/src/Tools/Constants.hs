module Tools.Constants where

import qualified Lib.Yudhishthira.Types as LYT

validRideTag :: LYT.TagNameValue
validRideTag = LYT.TagNameValue "ValidRide#Yes"

riderEligibleForCabUpgradeTag :: LYT.TagNameValue
riderEligibleForCabUpgradeTag = LYT.TagNameValue "RiderEligibleForCabUpgrade#Yes"

validDriverCancellation :: LYT.TagNameValue
validDriverCancellation = LYT.TagNameValue "DriverCancellation#Valid"

invalidDriverCancellation :: LYT.TagNameValue
invalidDriverCancellation = LYT.TagNameValue "DriverCancellation#Invalid"

acPriorityEligibleTag :: LYT.TagNameValue
acPriorityEligibleTag = LYT.TagNameValue "ACPriorityEligible#Yes"
