{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.EndPoints where

import Data.Maybe (Maybe(..))
import Prelude (show, (<>), (==))
import Services.Config (getBaseUrl)


triggerOTP :: String -> String
triggerOTP  dummy = (getBaseUrl "" ) <> "/auth"

verifyToken :: String -> String
verifyToken token = (getBaseUrl "") <> "/auth/"<>token<>"/verify"

resendOTP :: String -> String
resendOTP token = (getBaseUrl "") <> "/auth/otp/"<>token<>"/resend"

driverActiveInactive :: String -> String
driverActiveInactive status = (getBaseUrl "") <> "/driver/setActivity?active="<> status

driverActiveInactiveSilent :: String -> String -> String
driverActiveInactiveSilent status status_n = (getBaseUrl "") <> "/driver/setActivity?active="<> status <>"&mode="<> show status_n

startRide :: String -> String
startRide rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/start"

endRide :: String -> String
endRide rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/end"

cancelRide :: String -> String
cancelRide rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/cancel"

logout :: String -> String
logout dummyString = (getBaseUrl "") <> "/auth/logout"

getDriverInfo :: String -> String
getDriverInfo dummyString = (getBaseUrl "") <> "/driver/profile"

getRideHistory :: String -> String -> String -> String -> String -> String
getRideHistory limit offset isActive status day= do
  case status of
    "null" -> (getBaseUrl "") <> "/driver/ride/list?limit="<>limit<>"&offset="<>offset<>"&onlyActive="<>isActive
    _ -> (getBaseUrl "") <> "/driver/ride/list?onlyActive="<>isActive<>"&status="<> (show status) <> if day == "null" then "" else "&day=" <> day

offerRide :: String -> String
offerRide dummyString = (getBaseUrl "") <> "/driver/searchRequest/quote/offer"

updateDriverInfo :: String -> String
updateDriverInfo dummyString = (getBaseUrl "") <> "/driver/profile"

listCancelReason :: String -> String
listCancelReason dummyString = (getBaseUrl "") <> "/cancellationReason/list"

getRoute :: String -> String
getRoute routeType = (getBaseUrl "") <> "/" <> routeType <>"/route"

registerDriverRC :: String -> String
registerDriverRC dummyString = (getBaseUrl "") <> "/driver/register/rc"

registerDriverDL :: String -> String
registerDriverDL dummyString = (getBaseUrl "") <> "/driver/register/dl"

getAllRcData :: String -> String
getAllRcData dummyString = (getBaseUrl "") <> "/rc/all"

makeRcActiveOrInactive :: String -> String
makeRcActiveOrInactive dummyString = (getBaseUrl "") <> "/rc/setStatus"

deleteRc :: String -> String
deleteRc dummyString = (getBaseUrl "") <> "/rc/delete"

callDriverToDriver :: String -> String
callDriverToDriver rcNo = (getBaseUrl "") <> "/driver/register/call/driver?RC=" <> rcNo 

driverRegistrationStatus :: String -> String
driverRegistrationStatus dummyString = (getBaseUrl "") <> "/driver/register/status?multipleRC=true"

validateImage :: String -> String
validateImage dummyString = (getBaseUrl "") <> "/driver/register/validateImage"

referDriver :: String -> String
referDriver dummyString = (getBaseUrl "") <> "/driver/referral"

getstatsInfo :: String -> String
getstatsInfo day = (getBaseUrl "") <> "/driver/profile/stats?day="<> day

driverArrived :: String -> String
driverArrived rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/arrived/pickup"

flowStatus :: String -> String
flowStatus dummy = (getBaseUrl "33") <> "/frontend/flowStatus"
messageList :: String -> String -> String
messageList limit offset = (getBaseUrl "") <> "/message/list?limit=" <> limit <> "&offset=" <> offset

messageSeen :: String -> String
messageSeen messageId = (getBaseUrl "") <> "/message/" <> messageId <> "/seen"

messageResponse :: String -> String
messageResponse messageId = (getBaseUrl "") <> "/message/" <> messageId <> "/response"

linkReferralCode :: String -> String
linkReferralCode dummyString = (getBaseUrl "") <> "/driver/linkReferralCode"

getPerformance :: String -> String
getPerformance dummyString = (getBaseUrl "") <> "/driver/performance"

driverAlternateNumber :: String -> String
driverAlternateNumber  dummy = (getBaseUrl "" ) <> "/driver/alternateNumber/validate"

alternateNumberResendOTP :: String -> String
alternateNumberResendOTP dummy = (getBaseUrl "") <> "/driver/alternateNumber/resendOtp"

verifyAlternateNumberOTP :: String -> String
verifyAlternateNumberOTP dummy = (getBaseUrl "") <> "/driver/alternateNumber/verify"

removeAlternateNumber :: String -> String
removeAlternateNumber dummy = (getBaseUrl "") <> "/driver/alternateNumber/remove"

getCategories :: String -> String
getCategories language = (getBaseUrl "") <> "/issue/category?language=" <> language

getOptions :: String -> String -> String
getOptions categoryId language = (getBaseUrl "") <> "/issue/option?categoryId=" <> categoryId <> "&language=" <> language

uploadFile :: String -> String
uploadFile dummy = (getBaseUrl "") <> "/issue/upload"

postIssue :: String -> String
postIssue dummy = (getBaseUrl "") <> "/issue"

issueInfo :: String -> String
issueInfo issueId = (getBaseUrl "") <> "/issue/" <> issueId <> "/info"

callDriverToCustomer :: String ->  String
callDriverToCustomer  rideId =  (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/call/customer"


fetchIssueList :: String -> String
fetchIssueList dummy = (getBaseUrl "") <> "/issue/list"

deleteIssue :: String -> String
deleteIssue issueId = (getBaseUrl "") <> "/issue/"<> issueId <> "/delete"

otpRide :: String -> String
otpRide dummyRideOtp = (getBaseUrl "") <> "/driver/otpRide/start"

onCall :: String -> String
onCall _ = (getBaseUrl "") <> "/onCall"

likeMessage :: String -> String
likeMessage messageId = (getBaseUrl "") <> "/message/" <> messageId <> "/like"

leaderBoardDaily :: String -> String
leaderBoardDaily date = (getBaseUrl "") <> "/driver/leaderBoard/daily?date=" <> date

leaderBoardWeekly :: String -> String -> String
leaderBoardWeekly fromDate toDate = (getBaseUrl "") <> "/driver/leaderBoard/weekly?fromDate=" <> fromDate <> "&toDate=" <> toDate

currentDateAndTime :: String -> String
currentDateAndTime _ = "https://tools.aimylogic.com/api/now"

profileSummary :: String -> String
profileSummary _ = getBaseUrl "" <> "/driver/profile/summary"

createOrder :: String -> String
createOrder id = (getBaseUrl "37") <> "/payment/" <> id <>"/createOrder"

orderStatus :: String -> String
orderStatus orderId = (getBaseUrl "37") <> "/payment/" <> orderId <>"/status"

paymentHistory :: String -> String -> Maybe String -> String
paymentHistory to from status = case status of
  Nothing -> (getBaseUrl "") <> "/driver/payments/history?from=" <> from <> "&to=" <> to
  Just status' -> (getBaseUrl "") <> "/driver/payments/history" <> "?status=" <> (show status')

triggerAadhaarOTP :: String -> String
triggerAadhaarOTP _ = (getBaseUrl "") <> "/driver/register/generateAadhaarOtp"

verifyAadhaarOTP :: String -> String
verifyAadhaarOTP _ = (getBaseUrl "") <> "/driver/register/verifyAadhaarOtp"

unVerifiedAadhaarData :: String -> String
unVerifiedAadhaarData _ = (getBaseUrl "") <> "/driver/register/unVerifiedAadhaarData"

getKioskLocations :: String -> String
getKioskLocations _ = (getBaseUrl "") <> "/kioskLocation/list"

getUiPlans :: String -> String 
getUiPlans _ = (getBaseUrl "") <> "/plan/list"

getCurrentPlan :: String -> String 
getCurrentPlan driverId = (getBaseUrl "") <> "/plan/currentPlan"

subscribePlan :: String -> String 
subscribePlan planId = (getBaseUrl "") <> "/plan/"<> planId <>"/subscribe"

paymentDues :: String -> String 
paymentDues _ = (getBaseUrl "") <> "/payment/dues"

resumeMandate :: String -> String 
resumeMandate _ = (getBaseUrl "") <> "/plan/resume"

selectPlan :: String -> String
selectPlan planId = (getBaseUrl "") <> "/plan/"<> planId <>"/select"

suspendMandate :: String -> String 
suspendMandate driverId = (getBaseUrl "") <> "/plan/suspend" 

postRideFeedback :: String -> String 
postRideFeedback _ = (getBaseUrl "") <> "/feedback/rateRide"

generateReferralCode :: String -> String
generateReferralCode dummyString = (getBaseUrl "") <> "/driver/generateReferralCode"

paymentHistoryListV2 :: String -> String -> String -> String
paymentHistoryListV2 limit offset historyType = (getBaseUrl "") <> "/driver/v2/payments/history?limit=" <> limit <> "&offset=" <> offset <> "&paymentMode=" <> show historyType

paymentEntityDetails :: String -> String 
paymentEntityDetails id = (getBaseUrl "") <> "/driver/v2/payments/history/" <> id <> "/entity"

cleardues :: String -> String 
cleardues _ = (getBaseUrl "") <> "/driver/cleardues"

autoComplete :: String -> String
autoComplete _ = (getBaseUrl "") <> "/maps/autoComplete"

placeDetails :: String -> String
placeDetails _ =  (getBaseUrl "") <> "/maps/getPlaceDetails"

getPlaceName :: String -> String 
getPlaceName _ = (getBaseUrl "") <> "/maps/getPlaceName"

activateDriverGoTo :: String -> String -> String
activateDriverGoTo homeLocationId currentLocation = getBaseUrl "" <> "/driver/goHome/activate?homeLocationId=" <> homeLocationId <> "&currentLocation=" <> currentLocation

deactivateDriverGoTo :: String -> String
deactivateDriverGoTo _ = getBaseUrl "" <> "/driver/goHome/deactivate"

addDriverHomeLocation :: String -> String
addDriverHomeLocation _ = getBaseUrl "" <> "/driver/goHome/add"

getDriverHomeLocation :: String -> String
getDriverHomeLocation _ = getBaseUrl "" <> "/driver/goHome/get"

deleteDriverHomeLocation :: String -> String
deleteDriverHomeLocation homeLocationId = getBaseUrl "" <> "/driver/goHome/delete?homeLocationId=" <> homeLocationId

updateDriverHomeLocation :: String -> String
updateDriverHomeLocation homeLocationId = getBaseUrl "" <> "/driver/goHome/update?homeLocationId=" <> homeLocationId

rideRoute :: String -> String
rideRoute rideId = (getBaseUrl "") <> "/" <> rideId <>"/route"

getCoinTransactions :: String -> String
getCoinTransactions date =  (getBaseUrl "") <> "/coins/transactions" <> "?date=" <> date

getCoinUsageHistory :: String -> String
getCoinUsageHistory _ =  (getBaseUrl "") <> "/coins/usageHistory"

convertCoinToCash :: String -> String
convertCoinToCash _ =  (getBaseUrl "") <> "/coins/convertCoinToCash"
