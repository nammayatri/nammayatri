{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.EndPoint where

import Prelude ((<>),show, (==),(&&),(/=))
import Data.Maybe (maybe, Maybe(..), fromMaybe)
import Services.Config (getBaseUrl)
import Data.String


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

locServiceability :: String -> String
locServiceability serviceabilityType = (getBaseUrl "7") <> "/serviceability/" <> serviceabilityType

serviceabilityDest :: String -> String
serviceabilityDest dummyString = (getBaseUrl "8") <> "/serviceability/destination"

sendIssue :: String -> String
sendIssue dummyString = (getBaseUrl "9") <> "/support/sendIssue"

getRoute :: String -> String
getRoute routeType = (getBaseUrl "10") <> case routeType of
                                                "trip" -> "/trip/route"
                                                "intercity" -> "/route"
                                                _ -> "/pickup/route" 

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

rideBookingList :: String -> String -> String -> Maybe String -> Maybe String -> String
rideBookingList limit offset isActive status clientId = 
  maybe 
    ((getBaseUrl "16") <> "/rideBooking/list?limit="<> limit <>"&offset="<> offset <>"&onlyActive=" <> isActive)
    (\rideStatus ->
      let clientIdStr = (fromMaybe "" clientId)
      in
        if null clientIdStr
          then (getBaseUrl "41") <> "/rideBooking/list?limit=" <> limit <> "&offset=" <> offset <> "&onlyActive=false" <> "&status=" <> show rideStatus
          else (getBaseUrl "41") <> "/rideBooking/list?limit=" <> limit <> "&offset=" <> offset <> "&onlyActive=false" <> "&status=" <> show rideStatus <> "&clientId=" <> clientIdStr)
    status
ridebooking :: String ->  String
ridebooking bookingId  = (getBaseUrl "17") <> "/rideBooking/"<> bookingId

ridebookingStatus :: String ->  String
ridebookingStatus bookingId  = (getBaseUrl "17") <> "/rideBooking/v2/"<> bookingId

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

voipCall :: String -> String
voipCall _ = (getBaseUrl "55") <> "/call/voip"

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

ticketPlaceServices :: String -> String -> String
ticketPlaceServices placeId date = (getBaseUrl "43") <> "/ticket/places/" <> placeId <> "/services?date=" <> date

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

getOptions :: String -> String -> String -> String -> String -> String
getOptions categoryId optionId rideId issueReportId language = 
  getBaseUrl "43" <> "/issue/option?categoryId=" <> categoryId <> "&language=" <> language <> rideIdQueryParam <> optionIdQueryParam
  where
    rideIdQueryParam = if null rideId then "" else "&rideId=" <> rideId
    optionIdQueryParam = 
      if null optionId then "" 
      else "&optionId=" <> optionId <> issueReportIdQueryParam
    issueReportIdQueryParam = 
      if null issueReportId then "" 
      else "&issueReportId=" <> issueReportId

issueInfo :: String -> String -> String
issueInfo issueId language = (getBaseUrl "44") <> "/issue/" <> issueId <> "/info?language=" <> language

updateIssue :: String -> String -> String
updateIssue issueId language = (getBaseUrl "45") <> "/issue/" <> issueId <> "/updateStatus" <> "?language=" <> language

fetchIssueList :: String -> String
fetchIssueList language = (getBaseUrl "46") <> "/issue/list?language=" <> language

getEmergencySettings :: String -> String
getEmergencySettings dummy = (getBaseUrl "47") <> "/profile/getEmergencySettings"

updateEmergencySettings :: String -> String
updateEmergencySettings dummy = (getBaseUrl "48") <> "/profile/updateEmergencySettings"

updateSafeRide :: String -> String
updateSafeRide sosId = (getBaseUrl "49") <> "/sos/markRideAsSafe/" <> sosId

updateSosMedia :: String -> String
updateSosMedia sosId = (getBaseUrl "50") <> "/sos/" <> sosId <> "/upload"

getSosDetails :: String -> String
getSosDetails rideId = (getBaseUrl "51") <> "/sos/getDetails/" <> rideId

safetySupport :: String -> String
safetySupport dummy = (getBaseUrl "52") <> "/support/safetyCheckSupport"

createMockSos :: String -> String
createMockSos dummy = (getBaseUrl "53") <> "/sos/createMockSos"

shareRide :: String -> String
shareRide dummy = (getBaseUrl "54") <> "/share/ride"

followRide :: String -> String
followRide _ = (getBaseUrl "47") <> "/follow/ride"

getMetroStations :: String -> String -> String -> String -> String -> String
getMetroStations vehicleType city routeCode endStationCode location = (getBaseUrl "47") <> "/frfs/stations?vehicleType=\"" <> vehicleType <> "\"&city=" <> city <> (if (vehicleType == "BUS" && routeCode /= "") then "&routeCode=" <> routeCode else "") <> (if (vehicleType == "BUS" && endStationCode/= "") then "&endStationCode=" <> endStationCode else "")<> "&location=" <> location

frfsSearch :: String -> String
frfsSearch vehicleType = (getBaseUrl "48") <> "/frfs/search?vehicleType=\"" <> vehicleType <> "\""

getBusRoutes :: String -> String -> String -> String
getBusRoutes city startStationCode endStationCode = (getBaseUrl "47") <> "/frfs/routes?vehicleType=\"BUS\"" <> "&city=" <> city <> "&startStationCode=" <> startStationCode <> "&endStationCode=" <> endStationCode

frfsQuotes :: String -> String
frfsQuotes searchId = (getBaseUrl "49") <> "/frfs/search/" <> searchId <> "/quote"

confirmMetroQuote :: String -> String
confirmMetroQuote quoteId = (getBaseUrl "50") <> "/frfs/quote/" <> quoteId <> "/confirm"

getMetroBookingStatus :: String -> String
getMetroBookingStatus bookingId = (getBaseUrl "51") <> "/frfs/booking/" <> bookingId <> "/status"

getMetroBookingList :: String -> Maybe String -> Maybe String -> String
getMetroBookingList vehicleType limit offset = 
  (getBaseUrl "52")
    <>  (case limit, offset of
          Just limit', Just offset' -> "/frfs/booking/list?vehicleType=\"" <> vehicleType <> "\"" <> "&limit=" <> limit' <> "&offset=" <> offset'
          _, _ -> "/frfs/booking/list?vehicleType=\"" <> vehicleType <> "\""
        )

retryMetrTicketPayment :: String -> String
retryMetrTicketPayment quoteId = (getBaseUrl "53") <> "/frfs/quote/" <> quoteId <> "/payment/retry"

metroBookingSoftCancel :: String -> String
metroBookingSoftCancel bookingId = (getBaseUrl "54") <> "/frfs/booking/" <> bookingId <> "/canCancel"

getMetroBookingSoftCancelStatus :: String -> String
getMetroBookingSoftCancelStatus bookingId = (getBaseUrl "55") <> "/frfs/booking/" <> bookingId <> "/canCancel/status"

metroBookingHardCancel :: String -> String
metroBookingHardCancel bookingId = (getBaseUrl "56") <> "/frfs/booking/" <> bookingId <> "/cancel"

getMetroBookingHardCancelStatus :: String -> String
getMetroBookingHardCancelStatus bookingId = (getBaseUrl "57") <> "/frfs/booking/cancel/" <> bookingId <> "/status"

pushSDKEvents :: String -> String
pushSDKEvents _ =  (getBaseUrl "") <> "/sdk/events"

addStop :: String -> String
addStop rideBookingId = (getBaseUrl "47") <> "/rideBooking/" <> rideBookingId <> "/addStop"

editStop :: String -> String
editStop rideBookingId = (getBaseUrl "48") <> "/rideBooking/" <> rideBookingId <> "/editStop"

rentalSearch :: String -> String
rentalSearch dummy = (getBaseUrl "49") <> "/rental/search"

addOrEditStop :: Boolean -> String -> String 
addOrEditStop isEdit rideBookingId = (getBaseUrl "47") <> "/rideBooking/" <> rideBookingId <> if isEdit then "/editStop" else "/addStop" 

editLocation :: String -> String
editLocation rideId = (getBaseUrl "58") <> "/ride/" <> rideId <> "/edit/location"

getEditLocResult :: String -> String
getEditLocResult bookingUpdateRequestId = (getBaseUrl "59") <> "/edit/" <> bookingUpdateRequestId <>  "/result"

confirmEditLocResult :: String -> String
confirmEditLocResult bookingUpdateRequestId = (getBaseUrl "60") <> "/edit/result/" <> bookingUpdateRequestId <> "/confirm"


getFRFSBookingConfig :: String -> String
getFRFSBookingConfig city = (getBaseUrl "58") <> "/frfs/config?city=" <> city

getEmergencyContactsTrackingStatus :: String -> String
getEmergencyContactsTrackingStatus rideId = (getBaseUrl "59") <> "/followRide/ECStatus/" <> rideId

getManuallySharedRideDetails :: String -> String
getManuallySharedRideDetails rideId = (getBaseUrl "60") <> "/followRide/" <> rideId <> "/customerDetails"

multiChat :: String -> String 
multiChat _ = (getBaseUrl "61") <> "/triggerFCM/message"

getFavouriteDriverList :: String
getFavouriteDriverList = ((getBaseUrl "58") <> "/driver/favorites") 

getFavouriteDriverTrips :: String -> String -> String -> Maybe String -> Maybe String -> String
getFavouriteDriverTrips limit offset isActive status clientId = 
  maybe 
    ((getBaseUrl "16") <> "/rideBooking/favourites/list?limit="<> limit <>"&offset="<> offset <>"&onlyActive=" <> isActive)
    (\rideStatus ->
      let clientIdStr = (fromMaybe "" clientId)
      in
        if null clientIdStr
          then (getBaseUrl "41") <> "/rideBooking/favourites/list?limit=" <> limit <> "&offset=" <> offset <> "&onlyActive=false" <> "&status=" <> show rideStatus
          else (getBaseUrl "41") <> "/rideBooking/favourites/list?limit=" <> limit <> "&offset=" <> offset <> "&onlyActive=false" <> "&status=" <> show rideStatus <> "&clientId=" <> clientIdStr)
    status

removeFavouriteDriver :: String -> String
removeFavouriteDriver id = ((getBaseUrl "59") <> "/favorites/" <> id <> "/remove") 

getDeliveryImage :: String -> String
getDeliveryImage rideId = (getBaseUrl "61") <> "/ride/" <> rideId <> "/deliveryImage"

busAutoComplete :: String -> String -> String -> Maybe String -> String -> Maybe String -> String
busAutoComplete vehicleType city location input limit offset = 
  (getBaseUrl "48") <> "/frfs/autocomplete?vehicleType=\"" <> vehicleType <> "\"&city=" <> city <> "&location=" <> location <> 
  "&limit=" <> limit <> "&offset=" <> (fromMaybe "0" offset) <>
  maybe "" (\i -> "&input=" <> i) input

trackRouteBus :: String -> String 
trackRouteBus route = (getBaseUrl "61") <> "/track/"<>route <> "/vehicles"

triggerAadhaarOTP :: String -> String
triggerAadhaarOTP _ = (getBaseUrl "") <> "/verifyAadhaar/generateOtp"

verifyAadhaarOTP :: String -> String
verifyAadhaarOTP _ = (getBaseUrl "") <> "/verifyAadhaar/verifyOtp"

-- unVerifiedAadhaarData :: String -> String
-- unVerifiedAadhaarData _ = (getBaseUrl "") <> "/driver/register/unVerifiedAadhaarData"

frfsRoute :: String -> String -> String -> String
frfsRoute routeCode city vehicleType = (getBaseUrl "61") <> "/frfs/route/" <> routeCode <> "?vehicleType=" <> show vehicleType <> "&city=" <> city

confirmMetroQuoteV2 :: String -> String
confirmMetroQuoteV2 quoteId = (getBaseUrl "50") <> "/frfs/quote/v2/" <> quoteId <> "/confirm"
