module Screens.HelpAndSupportScreen.Transformer where

import Data.Function.Uncurried (runFn2)
import Helpers.Utils (toStringJSON, getCurrentUTC)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (map, (/), (<>), (>), ($))
import Services.API (IssueReportDriverListItem(..))
import Screens.Types (IssueInfo)
import JBridge (differenceBetweenTwoUTC)


getApiIssueList :: Array IssueReportDriverListItem -> Array IssueInfo
getApiIssueList issueList = map (\(IssueReportDriverListItem issue) -> {
   issueReportId : issue.issueReportId,
   status : issue.status,
   category : case issue.category of
                  "lost and found" -> "Lost Item"
                  "app related" -> "App Related Issue"
                  "ride related" -> "Ride Related Issue"
                  "fare" -> "Fare Related Issue"
                  _ -> ""
              ,
   createdAt : getExactTime $ runFn2 differenceBetweenTwoUTC (getCurrentUTC "") issue.createdAt,
   issueReportShortId : issue.issueReportShortId
}) issueList

getExactTime :: Int -> String
getExactTime sec = 
   let {base , suffix}=  if sec > 31536000 then { base : sec / 31536000, suffix : getString YEARS_AGO}
                          else if sec > 2592000 then {base : sec / 2592000, suffix : getString MONTHS_AGO}
                          else if sec > 86400 then {base : sec / 86400, suffix : getString DAYS_AGO}
                          else if sec > 3600 then {base : sec / 3600, suffix : getString HOURS_AGO}
                          else if sec > 60 then {base : sec / 60,  suffix : getString MIN_AGO}
                          else {base: sec , suffix: getString SEC_AGO}
   in toStringJSON (base) <> " " <> suffix