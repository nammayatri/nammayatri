module Tools.Constants where

import qualified Lib.Yudhishthira.Types as LYT

rejectUpgradeTag :: LYT.TagNameValue
rejectUpgradeTag = LYT.TagNameValue "CabUpgradeChoice#Reject"

validCustomerCancellation :: LYT.TagNameValue
validCustomerCancellation = LYT.TagNameValue "CustomerCancellation#Valid"

invalidCustomerCancellation :: LYT.TagNameValue
invalidCustomerCancellation = LYT.TagNameValue "CustomerCancellation#Invalid"

validUserNoShowCancellation :: LYT.TagNameValue
validUserNoShowCancellation = LYT.TagNameValue "CustomerNoShowCancellation#Valid"

invalidUserNoShowCancellation :: LYT.TagNameValue
invalidUserNoShowCancellation = LYT.TagNameValue "CustomerNoShowCancellation#Invalid"
