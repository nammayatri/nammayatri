{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.EndPoints where

import Prelude ((<>))
import Services.Config (getBaseUrl)
import Prelude(show, (==))
import Data.Maybe


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

getRideHistory :: String -> String -> String -> Maybe String -> String
getRideHistory limit offset isActive status = do
    case status of
        Nothing -> (getBaseUrl "") <> "/driver/ride/list?limit="<>limit<>"&offset="<>offset<>"&onlyActive="<>isActive
        Just status -> (getBaseUrl "") <> "/driver/ride/list?limit="<>limit<>"&offset="<>offset<>"&onlyActive="<>isActive <> "&status="<> show status

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
