module Services.EndPoints where

import Prelude ((<>))
import Services.Config (getBaseUrl)


triggerOTP :: String -> String
triggerOTP  dummy = (getBaseUrl "" ) <> "/auth"

verifyToken :: String -> String
verifyToken token = (getBaseUrl "") <> "/auth/"<>token<>"/verify"

resendOTP :: String -> String
resendOTP token = (getBaseUrl "") <> "/auth/otp/"<>token<>"/resend"

driverActiveInactive :: String -> String
driverActiveInactive status = (getBaseUrl "") <> "/driver/setActivity?active="<> status

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

getRideHistory :: String -> String -> String -> String
getRideHistory limit offset isActive = (getBaseUrl "") <> "/driver/ride/list?limit="<>limit<>"&offset="<>offset<>"&onlyActive="<>isActive

offerRide :: String -> String 
offerRide dummyString = (getBaseUrl "") <> "/driver/searchRequest/quote/offer"

updateDriverInfo :: String -> String 
updateDriverInfo dummyString = (getBaseUrl "") <> "/driver/profile"

listCancelReason :: String -> String 
listCancelReason dummyString = (getBaseUrl "") <> "/cancellationReason/list"

getRoute :: String -> String
getRoute dummyString = (getBaseUrl "") <> "/route"

registerDriverRC :: String -> String 
registerDriverRC dummyString = (getBaseUrl "") <> "/driver/register/rc"

registerDriverDL :: String -> String 
registerDriverDL dummyString = (getBaseUrl "") <> "/driver/register/dl"

driverRegistrationStatus :: String -> String
driverRegistrationStatus dummyString = (getBaseUrl "") <> "/driver/register/status"

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
