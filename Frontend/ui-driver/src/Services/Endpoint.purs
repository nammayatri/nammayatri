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
import Prelude (show, unit, (<>), (==), (*) , (&&) , (||))
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

arrivedAtStop :: String -> String
arrivedAtStop rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/arrived/stop"

cancelRide :: String -> String
cancelRide rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/cancel"

logout :: String -> String
logout dummyString = (getBaseUrl "") <> "/auth/logout"

getDriverInfo :: String -> String
getDriverInfo dummyString = (getBaseUrl "") <> "/driver/profile"

getDriverInfoV2 :: String -> String
getDriverInfoV2 dummyString = (getBaseUrl "") <> "/driver/profile/info"

getRideHistory :: String -> String -> String -> String -> String -> String
getRideHistory limit offset isActive status day= do
  case status of
    "null" -> (getBaseUrl "") <> "/driver/ride/list?limit="<>limit<>"&offset="<>offset<>"&onlyActive="<>isActive <> if day == "null" then "" else "&day=" <> day
    _ -> (getBaseUrl "") <> "/driver/ride/list?onlyActive="<>isActive<>"&status="<> (show status) <> (if day == "null" then "" else "&day=" <> day) <> (if limit == "null" then "" else "&limit=" <> limit)

getRidesSummaryList :: Array String -> String
getRidesSummaryList dateList = (getBaseUrl "") <> "/rideSummary/list"

submitDriverProfile :: String -> String 
submitDriverProfile dummyString = (getBaseUrl "") <> "/DriverProfileQues"

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

driverRegistrationStatus :: Boolean -> String
driverRegistrationStatus queryParam = (getBaseUrl "") <> "/driver/register/status?makeSelfieAadhaarPanMandatory=" <> show queryParam

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

getMessage :: String -> String
getMessage messageId = (getBaseUrl "") <> "/message/" <> messageId

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
onCall _ = (getBaseUrl "") <> "/callEvent"

voipCall :: String -> String
voipCall _ = (getBaseUrl "") <> "/call/voip"

likeMessage :: String -> String
likeMessage messageId = (getBaseUrl "") <> "/message/" <> messageId <> "/like"

leaderBoardDaily :: String -> String
leaderBoardDaily date = (getBaseUrl "") <> "/driver/leaderBoard/daily?date=" <> date

leaderBoardWeekly :: String -> String -> String
leaderBoardWeekly fromDate toDate = (getBaseUrl "") <> "/driver/leaderBoard/weekly?fromDate=" <> fromDate <> "&toDate=" <> toDate

leaderBoardMonthly :: Int -> String
leaderBoardMonthly month = (getBaseUrl "") <> "/driver/leaderBoard/monthly?month=" <> show month

referredDrivers :: String -> String
referredDrivers dummy = (getBaseUrl "") <> "/driver/referral/getReferredDrivers"

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
getUiPlans vehicleVariant = case vehicleVariant of
  "null" -> (getBaseUrl "") <> "/plan/list"
  _ -> (getBaseUrl "") <> "/plan/list" <> "?vehicleVariant=" <> show vehicleVariant

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

getMerchantIdList :: String -> String 
getMerchantIdList merchantId = getBaseUrl "" <> "/city/" <> merchantId <> "/list"

detectCity :: String -> String
detectCity _ = (getBaseUrl "") <> "/driver/city"

getCoinTransactions :: String -> String
getCoinTransactions date =  (getBaseUrl "") <> "/coins/transactions" <> "?date=" <> date

getCoinUsageHistory :: String -> String -> String
getCoinUsageHistory limit offset =  (getBaseUrl "") <> "/coins/usageHistory?limit="<>limit<>"&offset="<>offset

convertCoinToCash :: String -> String
convertCoinToCash _ =  (getBaseUrl "") <> "/coins/convertCoinToCash"

pushSDKEvents :: String -> String
pushSDKEvents _ =  (getBaseUrl "") <> "/sdk/events"

getAllLmsModules :: String -> String
getAllLmsModules language = (getBaseUrl "") <> "/lms/listAllModules?language="<>language

getAllLmsVideos :: String -> String -> String
getAllLmsVideos moduleId language = (getBaseUrl "") <> "/lms/"<> moduleId <>"/listAllVideos?language="<>language

getAllLmsQuestions :: String -> String -> String
getAllLmsQuestions moduleId language = (getBaseUrl "") <> "/lms/" <> moduleId <> "/listAllQuiz?language="<>language

markVideoAsStarted :: String -> String
markVideoAsStarted _ = (getBaseUrl "") <> "/lms/markVideoAsStarted"

markVideoAsCompleted :: String -> String
markVideoAsCompleted _ = (getBaseUrl "") <> "/lms/markVideoAsCompleted"

confirmQuestion :: String -> String
confirmQuestion _ = (getBaseUrl "") <> "/lms/question/confirm"

getReelsData :: String -> String -> String
getReelsData reelsKey language = (getBaseUrl "") <> "/reels/getAllReelVideos?language=" <> language <> "&reelsKey=" <> reelsKey

dummyRideRequest :: String -> String
dummyRideRequest _ =  (getBaseUrl "") <> "/driver/getDummyRideRequest"

specialLocationList :: String -> String
specialLocationList _ = (getBaseUrl "") <> "/specialLocation/list"

onBoardingConfigs :: Boolean -> Boolean -> String
onBoardingConfigs showAadhaarProfilePan onlyVehicle = (getBaseUrl "") <> "/onboarding/configs?makeSelfieAadhaarPanMandatory=" <> show showAadhaarProfilePan <> "&onlyVehicle=" <> show onlyVehicle

driverVehicleServiceTier :: String -> String
driverVehicleServiceTier _ = (getBaseUrl "") <> "/driver/vehicleServiceTiers"

updateDriverVehicleServiceTier :: String -> String
updateDriverVehicleServiceTier _ = (getBaseUrl "") <> "/driver/updateServiceTiers"

uploadOdometerImage :: String -> String
uploadOdometerImage rideId = (getBaseUrl "") <> "/driver/ride/"<>rideId<>"/uploadOdometer"

getRideStatusPastDays :: String -> String
getRideStatusPastDays _ = (getBaseUrl "") <> "/coins/rideStatusPastDays"

updateAirConditioned :: String -> String
updateAirConditioned _ = (getBaseUrl "") <> "/driver/updateAirCondition"

getDriverRateCard :: Maybe String -> Maybe Int -> String
getDriverRateCard mbServiceTier mbDist = 
  (getBaseUrl "")
    <> "/driver/rateCard"
    <> case mbServiceTier , mbDist  of
        Just serviceTier, Just dist -> "?vehicleServiceTier=" <> serviceTier <> "?distance=" <> show dist
        Just serviceTier, Nothing -> "?vehicleServiceTier=" <> serviceTier
        Nothing, Just dist -> "?distance=" <> show (dist*1000)
        _ , _ -> ""

getReferralEarnings :: String -> String -> String
getReferralEarnings fromDate toDate = (getBaseUrl "") <> "/payout/referral/earnings?fromDate=" <> fromDate <> "&toDate=" <> toDate

deleteVPA :: String -> String
deleteVPA dummy = (getBaseUrl "") <> "/payout/delete/vpa"

registerPayout :: String -> String
registerPayout dummy = (getBaseUrl "") <> "/payout/registration"

getSdkToken :: String -> String -> String
getSdkToken expiry serviceName = (getBaseUrl "") <> "/driver/sdkToken?expiry=" <> expiry <> "&service=" <> serviceName

getLiveSelfie :: String -> String
getLiveSelfie status = (getBaseUrl "") <> "/driver/register/getLiveSelfie?status=" <> status

registerPAN :: String -> String
registerPAN _ = (getBaseUrl "") <> "/driver/register/pancard"

registerAadhaar ::  String -> String 
registerAadhaar _ = (getBaseUrl "") <> "/driver/register/aadhaarCard"

getDriverProfile :: Boolean -> String 
getDriverProfile isImages = (getBaseUrl "") <> "/DriverProfileQues?isImages=" <> (show isImages)

verifyUPI :: String -> String
verifyUPI dummy = (getBaseUrl "") <> "/driver/profile/verify/vpaStatus"

getCoinInfo :: String -> String
getCoinInfo _ = (getBaseUrl "") <> "/coins/info"

demandHotspots :: String -> String
demandHotspots _ = (getBaseUrl "") <> "/driver/demandHotspots"

getScheduledBookingList :: String -> String -> String -> String ->String -> String -> String -> String
getScheduledBookingList limit offset from to  tripCategory lat lon  =  (getBaseUrl "") <> "/driver/scheduledBooking/list?limit="<>limit<>"&offset="<>offset<> (if from == "null" then "" else "&from=" <> from) <> (if to == "null" then "" else "&to=" <> to)<>(if lat == "0.0" || lon == "0.0" then ""  else ("&currentLocation=" <> lat <> "," <> lon)) <> ( if tripCategory == "" then "" else "&tripCategory=" <> tripCategory)  

scheduleBookingAccept :: String -> String
scheduleBookingAccept bookingId = (getBaseUrl "") <> "/driver/accept/scheduledBooking?bookingId="<>bookingId

uploadParcelImage :: String -> String
uploadParcelImage rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/uploadDeliveryImage"

driverReachedDestination :: String -> String
driverReachedDestination rideId = (getBaseUrl "") <> "/driver/ride/" <> rideId <> "/arrived/destination"

updateHVSdkCallLog :: String -> String
updateHVSdkCallLog _ = (getBaseUrl "") <> "/driver/register/logHvSdkCall"

updateMetroWarriorInfo :: String -> String
updateMetroWarriorInfo driverId = (getBaseUrl "") <> "/updateInfo/specialLocWarrior?driverId=" <> driverId

getMetroWarriorInfo :: String -> String
getMetroWarriorInfo driverId = (getBaseUrl "") <> "/getInfo/specialLocWarrior?driverId=" <> driverId

specialLocationListCategory :: String -> String
specialLocationListCategory category = (getBaseUrl "") <> "/specialLocation/list/category?category=" <> category

arrivedStop :: String -> String -> String
arrivedStop rideId stopId = (getBaseUrl "") <> "/driver/ride/"<> rideId <>"/arrived/" <> stopId <> "/stop"

departedStop :: String -> String -> String
departedStop rideId stopId = (getBaseUrl "") <> "/driver/ride/"<> rideId <>"/departed/" <> stopId <> "/stop"
