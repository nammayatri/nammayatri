{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.EndPoint where

import Prelude ((<>),show, (==))
import Data.Maybe (maybe, Maybe(..))
import Services.Config (getBaseUrl)


triggerOTP :: String -> String
triggerOTP dummyString = (getBaseUrl "dummy1") <> "/auth"

triggerSignatureOTP :: String -> String
triggerSignatureOTP dummyString = (getBaseUrl "dummy1") <> "/auth/signature"

resendOTP :: String -> String
resendOTP token = (getBaseUrl "2") <> "/auth/otp/"<>token<>"/resend"

verifyToken :: String -> String
verifyToken token = (getBaseUrl "3") <> "/auth/"<>token<>"/verify"

searchReq :: String -> String
searchReq dummyString = (getBaseUrl "4") <> "/rideSearch"

logout :: String -> String
logout dummyString = (getBaseUrl "5") <> "/auth/logout"

serviceability :: String -> String
serviceability dummyString = (getBaseUrl "6") <> "/serviceability/ride"

serviceabilityOrigin :: String -> String
serviceabilityOrigin dummyString = (getBaseUrl "7") <> "/serviceability/origin"

serviceabilityDest :: String -> String
serviceabilityDest dummyString = (getBaseUrl "8") <> "/serviceability/destination"

sendIssue :: String -> String
sendIssue dummyString = (getBaseUrl "9") <> "/support/sendIssue"

getRoute :: String -> String
getRoute routeType = (getBaseUrl "10") <> if routeType == "trip" then "/trip/route" else "/pickup/route"

driverFeedBack :: String -> String
driverFeedBack dummyString = (getBaseUrl "12") <> "/feedback/rateRide"

confirmRide :: String  ->  String
confirmRide quoteId = (getBaseUrl "13") <> "/rideSearch/quotes/" <>  quoteId <> "/confirm"

getQuotes :: String -> String
getQuotes searchId = (getBaseUrl "14") <> "/rideSearch/"<> searchId <> "/results"

selectEstimate :: String -> String 
selectEstimate estimateId = (getBaseUrl "15") <> "/estimate/"<> estimateId <> "/select2"

selectList :: String -> String
selectList estimateId = (getBaseUrl "15") <> "/estimate/"<> estimateId <> "/results"

rideBookingList :: String -> String -> String -> Maybe String -> String
rideBookingList limit offset isActive status = 
  maybe 
    ((getBaseUrl "16") <> "/rideBooking/list?limit="<> limit <>"&offset="<> offset <>"&onlyActive=" <> isActive)
    (\rideStatus -> ((getBaseUrl "41") <> "/rideBooking/list?limit="<> limit <>"&offset="<> offset <>"&onlyActive=false" <>"&status=" <> show rideStatus))
    status

ridebooking :: String ->  String
ridebooking bookingId  = (getBaseUrl "17") <> "/rideBooking/"<> bookingId

currentListCaseProduct :: String ->  String
currentListCaseProduct dummyString = (getBaseUrl "18") <> "/productInstance?limit=1&type=\"RIDEORDER\""

cancelRide :: String ->  String
cancelRide bookingId = (getBaseUrl "19") <> "/rideBooking/" <> bookingId <> "/cancel"

updatePerson :: String ->  String
updatePerson dummyString = (getBaseUrl "20") <> "/person/update/"

listCancelReason :: String -> String
listCancelReason cancellationStage = (getBaseUrl "21") <> "/cancellationReason/list?cancellationStage=\"" <> cancellationStage <> "\""

getCurrentLocation :: String -> String
getCurrentLocation caseId = (getBaseUrl "22") <> "/ride/" <> caseId <> "/driver/location"

autoComplete :: String -> String
autoComplete dummyString = (getBaseUrl "23") <> "/maps/autoComplete"

placeDetails :: String -> String
placeDetails dummyString =  (getBaseUrl "24") <> "/maps/getPlaceDetails"

callCustomerToDriver :: String ->  String
callCustomerToDriver  rideId =  (getBaseUrl "25") <> "/ride/" <> rideId <> "/call/driver"

getPlaceName :: String -> String 
getPlaceName dummyString = (getBaseUrl "26") <> "/maps/getPlaceName"

feedback :: String -> String
feedback dummy = (getBaseUrl "27") <> "/feedback/rateRide"

profile :: String -> String
profile _ = (getBaseUrl "28") <> "/profile"

addLocation :: String -> String 
addLocation _ = (getBaseUrl "29") <> "/savedLocation"

savedLocation :: String -> String 
savedLocation _ = (getBaseUrl "30") <> "/savedLocation/list"

deleteLocation :: String -> String 
deleteLocation tag = (getBaseUrl "31") <> "/savedLocation/" <> tag

getCoordinates :: String -> String -> String
getCoordinates id  language = (getBaseUrl "32") <> "/googleMaps/getCoordinates?placeId=" <> id <> "&language=" <> language

flowStatus :: String -> String 
flowStatus dummy = (getBaseUrl "33") <> "/frontend/flowStatus"

notifyFlowEvent :: String -> String 
notifyFlowEvent dummy = (getBaseUrl "34") <> "/frontend/notifyEvent"

cancelEstimate :: String -> String 
cancelEstimate estimateId = (getBaseUrl "35") <> "/estimate/" <> estimateId <> "/cancel"

emergencyContacts :: String -> String
emergencyContacts dummy = (getBaseUrl "36") <> "/profile/defaultEmergencyNumbers"
userSos :: String -> String
userSos _ = (getBaseUrl "36") <> "/sos/create"

userSosStatus :: String -> String
userSosStatus sosId = (getBaseUrl "37") <> "/sos/" <> sosId <>"/status"

onCall :: String -> String
onCall _ = (getBaseUrl "38") <> "/callEvent"

callbackRequest :: String -> String
callbackRequest dummy = (getBaseUrl "38") <> "/support/callbackRequest"

bookingFeedback :: String -> String
bookingFeedback dummy = (getBaseUrl "39") <> "/feedback/submit"

disabilityList :: String -> String
disabilityList dummy = (getBaseUrl "40") <> "/disability/list"

personStats :: String -> String
personStats dummy = (getBaseUrl "41") <> "/personStats"

ticketPlaces :: String -> String
ticketPlaces dummy = (getBaseUrl "42") <> "/ticket/places"

ticketPlaceServices :: String -> String
ticketPlaceServices placeId = (getBaseUrl "43") <> "/ticket/places/" <> placeId <> "/services"

ticketPlaceBook :: String -> String
ticketPlaceBook placeId = (getBaseUrl "43") <> "/ticket/places/" <> placeId <> "/book"

getAllBookings :: String -> String
getAllBookings status = (getBaseUrl "41") <> "/ticket/bookings?status=" <> status -- <> "&limit=10&offset=1"


ticketStatus :: String -> String
ticketStatus shortId = (getBaseUrl "41") <> "/ticket/bookings/" <> shortId <> "/status"

ticketBookingDetails :: String -> String
ticketBookingDetails shortid = (getBaseUrl "41") <> "/ticket/bookings/" <> shortid <> "/details"

getCategories :: String -> String
getCategories language = (getBaseUrl "40") <> "/issue/category?language=" <> language

postIssue :: String -> String
postIssue language = (getBaseUrl "41") <> "/issue?language=" <> language

uploadFile :: String -> String
uploadFile dummy = (getBaseUrl "42") <> "/issue/upload"

getOptions :: String -> String -> String -> String -> String
getOptions categoryId optionId issueReportId language = 
  if (optionId == "") 
    then (getBaseUrl "43") <> "/issue/option?categoryId=" <> categoryId <> "&language=" <> language
    else if (issueReportId == "")
      then (getBaseUrl "43") <> "/issue/option?categoryId=" <> categoryId <> "&optionId=" <> optionId <> "&language=" <> language
      else (getBaseUrl "43") <> "/issue/option?categoryId=" <> categoryId <> "&optionId=" <> optionId <> "&issueReportId=" <> issueReportId <> "&language=" <> language

issueInfo :: String -> String -> String
issueInfo issueId language = (getBaseUrl "44") <> "/issue/" <> issueId <> "/info?language=" <> language

updateIssue :: String -> String -> String
updateIssue issueId language = (getBaseUrl "45") <> "/issue/" <> issueId <> "/updateStatus" <> "?language=" <> language

fetchIssueList :: String -> String
fetchIssueList language = (getBaseUrl "46") <> "/issue/list?language=" <> language

followRide :: String -> String
followRide _ = (getBaseUrl "47") <> "/follow/ride"
