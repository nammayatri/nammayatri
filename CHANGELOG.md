# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [14.11.0] - 14-12-2022

Changes

- Add actual distance filter in driver pool filtering

- Persist UI version in BAP/BPP DB

- Make merchantId mandatory for bpp person

- various bug fixes

- Send search expiry to the frontend

## [14.10.1] - 10-12-2022

Changes

- Remove distance matrix based driver pool filtering in select API

- various bug fixes

## [14.10.0] - 9-12-2022

Changes

- Intelligent driver pool computation

- Persist UI version in BAP/BPP DB

- Add DriverLocation to SearchRequestForDriver

- Store all cancellation reasons in case of re-allocation

- Send estimate range to BAP in search API

- various bug fixes

- Validate merchantId in auth API

## [14.9.1] - 3-12-2022

Changes

- Handle deletion of drivers and vehicles

- various bug fixes

- Add force enable and add vehicle api for driver

- Visualize ride cli helper should output csv as another option

## [14.9.0] - 1-12-2022

Changes

- various bug fixes

- DL to be verified as valid for the below DL Vehicle classes

- Add change driver's phone number and vehicle apis

- Integrate OSRM match API as alternative for google snap-to-road API

- Move GET APIs to lookup from read replica DB

- Block driver if he had already active offer

- Fix timeout error in driver list api if not provide limit

- Skipping check for image if date of issue field is there

- Rename Organization to Merchant at BPP

- Remove join queries from search, quote, booking and estimate tables of app-backend

- Re arrange cancellation reasons on basis of priority

- Upload image in chunks for validateImage API

- Update fare recomputation logic

## [14.8.0] - 18-11-2022

Changes

- various bug fixes

- remove postgresConfig from configs

- change the request body for web engage

- Add Karnataka geofence for NammaYatri application

## [14.7.0] - 15-11-2022

Changes

- update area value in regional language

- added google translate api

## [14.6.0] - 14-11-2022

Changes

- make dl valid class of vehicles as config

- Update fare recomputation logic

- Parse geocoding address based on outer `type` sublocality_level_2

- Add rate limit in driver location updates

- Expose endpoint for SMS campaign - WebEngage

- Changing dashboard admin and users paths

- Create person API by dashboard admin in dashboard

- list role api by using search string in dashboard with limit and offset

- Send notification to other blocked drivers who have given quote when customer accepts any offered quote

- Driver Onboarding Dashboard APIs

- Add Swagger for dashboard APIs

- Auth on basis of merchant instead of server

- Change user login flow

- Filter list API response based on booking status

- various bug fixes

## [14.5.0] - 4-11-2022

Changes

- Remove distance matrix API for on_search estimates in dobpp

- Add Passenger/3WN classes also as valid COV for RC

## [14.4.0] - 1-11-2022

Changes

- Limit max drivers in driver pool and randomise driver selection

- Add option to black list user

- Rate limit search requests per user

## [14.3.0] - 29-10-2022

Changes

- Limit max trip distance which BPP can offer quotes for

- Integrate Google maps Geocode API and expose it proxy to UI

- Use session_id in proxies google maps API

- Driver Onboarding Dashboard APIs

- various bug fixes

## [14.2.0] - 22-10-2022

Changes

- various bug fixes

- Add dateOfIssue to register DL API request and dateOfRegistration in register RC API.

- Add dateOnDoc, encryptedDocData and imageExtractionValidation columns to IdfyVerification table.

## [14.1.2] - 12-10-2022

Changes

- add more fields in rc table and idfy response dump type

## [14.1.1] - 11-10-2022

Changes

- moving from idfy rc_basic to rc_plus

## [14.1.0] - 11-10-2022

Changes

- get driver docs images

- improve dashboard flow

- Various bug fixes

## [14.0.0] - 7-10-2022

Changes

- Update dhall configs of sandbox to latest

- Recomputation actual distance when
    ride start location is not within threshold (<500m) radius of pickup location and of
    or ride end location is not within threshold (<500m) radius of destination location

- Handled IDFY errors properly

- Simplify application of night fare charges

- Added slack alert for driver onboard failure

- Rearranging Address, for fcm notification type NEW_RIDE_AVAILABLE

- Added referral code for driver

- Driver profile API should return vehicle as optional field in Driver-offer-bpp

- Add image validation

- Pickup and drop location shown to the driver should be in native languge selected by the driver

- Move AccessMatrix and Roles to DB for Dashboard ops

- Adding access matrix in auth module

- Setting up Dashboard Server

- Update Redis endpoint for master/sandbox endpoints

- Add Idfy API Request and Response Type's

- store ride rating on bap side in feedback api and show it in rideBookingList

- Add S3 service

- Rider name should be shared with the BPP and displayed to the driver for them to easily find each other

- Various bug fixes

## [13.0.0] - 14-08-2022 (partial), 13-09-2022

Task list for these changes are large. Listing notable changes here

- Customer saved Locations

- Modified Beckn ACL layer to comply with updated mobility implementation guide

- Added Rental fare product for static offer BPP

- Added driver offer one way fare product BPP

- Added AUTO_RICKSHAW vehicle class for driver offer BPP

- Various bug fixes

## [12.1.1] - 22-04-2022

### Fixed

#1423: [BKN-1240] fix distance unit conversion errors by introducing Meter type

## [12.1.0] - 19-04-2022

### Added

#1358: [BKN-1113] New driver pool calculation algorithm

#1373: [BKN-1196] De-dup search requests and on-search quotes

#1347: Feature/BKN-1184/integrate snap to roads API

#1338: [BKN-1185] implement google distance matrix api

#1369: [BKN-1144] Added dup check while creating driver.

### Changed

#1366: [BKN-1206] Refactor Metrics

#1374: Update Stack LTS in dockerfile

#1370: Upgraded GHC version

### Fixed

#1355: [BKN-1202]return driver information with deleted vehicle

### Removed

#1384: [BKN-1211] removed config field from AppEnv

#1381: [BKN-1132] Remove beam from beckn-transport

#1375: Updated Ernakulam district in BPP

#1372: Added geofencing to BPP

## [12.0.0] - 17-03-2022

### Added

#1348: added real signatures to mocks

#1337: [BKN-593]Unit Test Allocation Service

#1318: [BKN-1167] Add driver tracking healthcheck service

#1256: [BKN-1079] simple mock for dunzo

#1330: [BKN-1180] PublicTransportSearchConsumer App

#1326: [BKN-1155] Reallocation features

#1246: add swagger to fmd

#1203: [BKN-1036] Domain specific types for FMD-wrapper

#1235: [BKN-1017] mock parking bpp

#1307: [BKN-1158] Search result aggregator

#1269: [BKN-1117] public transport status api

#1294: [BKN-1116] public transport confirm

#1311: Added requests for Dunzo

#1281: Add Beckn EPs in transporter swagger

#1181: [BKN-981] Add organization whitelisting in taxi BAP

#1000: [BKN-923] Reallocate rides if cancelled by driver

#770: [BKN-712] Implement Exotel status callbacks

#1015: [BKN-908] Integrate kafka

#1243: [BKN-1119] mock public transport bpp

### Changed

#1345: [BKN-1188]Return ride list with-deleted entries

#1357: Changed driver ride list API to return rides rather than ride bookings

#1352: BKN-1193 modified driver health check notification

#1350: [BKN-712]: Changed the StatusCallbackEvents

#1344: move public transport message generation into fork

#1328: Refactor/public transport

#1327: [BKN-1160]: Removed API key from organization table

#1335: [BKN-1183] Change contains of driver pool on reallocation

#1297: BKN-1115 ACL v2

#1211: [BKN-1089]: Throwing 400 Error when Ride not longer trackable

#1293: Updated registry requests

#1285: Update Dockerfile with librdkafka-dev and Makefile to run docker builds locally

#970: [BKN-861] Add stage to BAP cancellation reason table

### Fixed

#1364: [BKN-1205] fix notification visibility

#1365: fix: added temp default values for deleted driver and vehicles

#1362: fix: driver ride notification route

#1336: [BKN-1164] Cancel rides by ride booking id rather than quote id

#1349: [BKN-712]:Removed the extra Space in url

#1340: [BKN-1189] Fix Kafka log messages

#1334: [BKN-712]: Fixed exotel callback url

#1332: [BKN-1155] Fixed bug in allocation service

#1323: [BKN-1169] public transport acl fixes

#1316: Fixed bpp id in context

#1317: fix: return unmasked driver number for coordinator driver list API, get driver profile and update driver profile APIs

#1288: Fixed build with gcc11

### Removed

#1258: [BKN-1131] Remove beam from BAP Taxi

#1298: Removed DB-related code from gateway

#1291: Removed DB from gateway

#1219: Removed key and ttl fields from Context

## [11.0.0] - 16-02-2022

### Fixed

#1317: fix: return unmasked driver number for coordinator driver list API, get driver profile and update driver profile APIs

#1319: fix: persist cancellation reason at transporter

#1211: [BKN-1089]: Handled 400 error for Invalid Ride track

## [10.0.0] - 10-02-2022

### Added

#1250: [BKN-1122] Rider details table

#1126: [BKN-1011] Add proxy auth to FMD search

#1249: Add expiration time to caching interface

#1177: [BKN-981] Add registry and caching interfaces

#1129: Add subscriber_url to mock-registry lookup result

#1231: [BKN-1076] Add swagger to pinpark

#1237: [BKN-1106] Search/on_search events logging in taxi-bap

#1210: [BKN-1091] Add storage types to pub transport bap

#1202: [BKN-1087] Boilerplate for public-transport-bap

#1227: Enabled Exotel in transporter

#1033: [BKN-942] Share customer phone with bpp

#1222: Added driver notification when a ride is cancelled by customer

#1182: Added Kochi region

#1185: [BKN-1063] & [BKN-1064] : Added requestor name in quote confirm API

#1190: [BKN-1066] Add location info in notificationData API

#1155: [BKN-1022] Save location from Search

#1175: BKN-1050: Store payment details

#1159: Added REST Client requests for auth and ride booking

#1110: Added fare policy for sedan and hatchback

#1115: Added vehicle variant to the quote API type

#1147: add body-hash middleware to parking-bap

#1132: [BKN-993] Add auth EP to mobility bap

#1125: [BKN-996] Parking confirm and booking status EPs

#1117: [BKN-995] Search flow for pinpark

#1116: [BKN-993] Add token auth to parking BAP

#1087: [33] Add discount and estimatedTotalFare to BPP RideBookingStatusRes

#1086: [32] Added city name as required by NSDL gateway

#1084: [30] Add registry support to gateway

#1065: [BKN-918] [15] Initial metro support at BAP

#1064: [BKN-933] [14] registry integration for Mobility

#1063: [BKN-902] [13] Implement discounts

#1061: [BKN-841] [11] on_subscribe API implementation

#1060: [10] Add unique check for perExtraKmRateList

#1053: [BKN-896] [4] Offers per vehicle type

#1040: [BKN-904, BKN-964] [1] Chargable distance

#1103: [BKN-994] Pinpark storage

#1101: [BKN-993] Add pinpark BAP skeleton

### Changed

#1025: [BKN-920] Move encoding salt to secrets

#1268: [BKN-1139] Unique keys per agency in taxi bpp

#1151: [BKN-1037] Replace Proxy-Authorization -> X-Gateway-Authorization

#1225: [BKN-1098]: Changed the Driver ride list api as per documentation

#971: [BKN-868] Update error counter for all errors

#1229: [BKN-1112] Last name should be optional

#1195: [BKN-1074] Restore removed address at BPP

#1199: replaced read with readMaybe

#1164: [BKN-1045] Return computedPrice in listRideBookings EP

#1201: Update driver info on customer cancellation

#1192: Made variant, model, color fields required

#1191: [BKN-1024] Desc sort for listRB API

#1173: [BKN 990] Fallback to estimates on missing location updates

#1178: [BKN-1042] Updated on_confirm location type to conform to network

policy
#1160: Changed the BookingID method to Post and getBookigDetail issue

#1150: [BKN-1016] Filter out CONFIRMED rides from listRides EP

#1118: 0.9.3 mobility spec migration

#1085: [BKN-970] [31] Show future schedule

#1078: [BKN-959] [25] gateway proxy raw body

#1077: [23] Use unique_key_id for subscriber lookup

#1067: [BKN-918] [17] Split search in BAP

#1043: [BKN-893] [2] Update fare policy

#920: Change data model

#967: [BKN-853] API V2 BPP

#968: [BKN-854] API V2 BAP

### Removed

#1271: revert making `maskedDeviceToken` non-maybe type in response of APIs

#1262: [BKN-965] Remove redundant registry call

#1239: [BKN-965] Remove callback url from org table

#1176: [BKN-981] Remove organization from taxi BAP

#1111: Removed api_key and callback_api_key values

#1144: [BKN-1034] delete unused fmd endpoints

#1143: Remove on_cancel and on_confirm

#1080: [27] Removed on_subscribe APIs

#1062: [BKN-929] [12] Remove auth from lookup API

### Fixed

#1146: Improve esqueleto

#1252: app-backend: fix search retries

#1214: parking-bap: fix Amount type in PaymentParams

#1095: [BKN-946] Cancellation race condition in BAP

#1140: [BKN-1031] Return 401 in parking BAP on auth fail

## [9.0.0] - 25-11-2021

### Changed

- #1106: [BKN 990] Fallback to estimates on missing location updates. Fix DB lookup

- #1097: [BKN-990] Fallback to estimates on missing location updates

## [8.0.0] - 15-11-2021

### Added

- #1039: [BKN-953] Add driver and vehicle to list rides EP

- #1037: Added registry support to gateway

- #1004: [WIP][BKN-918] Metro BPP support at app-backend

- #1007: [BKN-933] Registry integration for Mobility

### Changed

- #1068: added more logs to btm

- #1052: Added city name as required by NSDL gateway

- #1047: Show future times in the schedule

- #1035: Added hostName config setting

- #1031: Using callback_url instead of bap_uri in the callback

- #1029: [BKN-959] Make gateway proxy raw body

- #1017: Registry: lookup subscribers by unique_key_id

- #1026: Use bap_uri from the context in gateway

- #1021: Refactor gateway selection

- #1013: [BKN-918] Split on_search in BAP

- #1014: update: signature validation tests

### Removed

- #1032: Removed on_subscribe APIs

- #1020: Remove selfId from transporter

### Fixed

- #1054: Don't update status in on_update

- #1048: Fixed station order

- #1041: [BKN-964] Save BPP ChargeableDistance in db

- #1036: Report signature verification error if the subscriber is unknown

- #1022: Fix/BKN-958 metro domain

- #1018: Fix mobility domain variant in spec

## [7.0.0] - 21-10-2021

### Added

- #982: [BKN-841] onSubscribe API implementation

- #993: [BKN-902] Implement discounts

- #1010: Add discount and estimated total fare to ride info

### Changed

- #1009: [BKN-938] Change discount bounds working

### Removed

- #999: [BKN-929] Remove auth from lookup API

### Fixed

## [6.0.0] - 15-10-2021

### Added

- #981: [BKN-890] Add mock-fcm

- #986: [BKN-904] Added chargable distance to product instance

- #984: [BKN-896] add offers per vehicle type

- #1003: Add unique check for perExtraKmRateList

### Changed

- #966: [BKN-864] Revert back to linear distance calculation

- #978: [BKN-852] Refactor and cleanup mobility integration tests

- #947: [BKN-680] registry integration in FMD

- #983: [BKN-893] Update fare policy ExtraKmRate

- #990: Tmp update jenkinsfile

- #996: Broadcast notifications

- #1001: [BKN-913] Dissallow overlapping migration indices


### Removed

- #965: [BKN-858] remove role from login request in Mobility

- #969: [BKN-870] Remove Status and Track EPs

### Fixed

- #994: Fixed incorrect spelling

- #995: Fix vehicle variant to PI migration

- #997: Fix: driver info not being sent in on_update on allocation

- #998: Fixed allocation service unit tests

## [5.0.0] - 20-09-2021

### Added

- #973: add `Invalid Receiver` error case to sms response

- #835: dev: add promtheus and grafana monitoring configs

- #946: Add run and stop apps scripts

- #912: [BKN-803,BKN-804] Add metrics

- #963: Add driver enabled field in driver info api entity

- #935: [BKN-816] added updating confluent public key in docker build

- #924: Added actor to front-end facing APIs

- #960: [BKN-834] added constructor with text field to Domain type

- #908: [BKN-809] Activate|Deactivate driver

### Changed

- #942: [BKN-838] Return 400 on bad number in registration APIs

- #932: [BKN-832] Relax person API address validation and simplify it

- #783: [BKN-713] Return error on Exotel error

- #910: [BKN-815] Split location table

- #886: [BKN-710] Refactor FlowR

- #939: Updated Ernakulam shape file

- #945: [BKN-840] Check for verified token

### Fixed

- #975: app-backend: fix getLocation API lookup based on `status` instead of `id`s

### Removed

- #964: [BKN-867] remove fetching case on confirm in FMD

- #921: Remove DriverToken, AdminTokenAuth result changed to Id Organization type

- #720: [BKN-675] Remove listPerson EP

- #936: Removed Cron APIs

- #943: [BKN-839] Remove "create transporter API"

## [4.0.0] - 26-08-2021

### Added

- #953: fcm: add personId in fcm error log

- #938: [BKN-833] Cancellation reasons tables

- #927: [BKN-821] Recompute fare after ride completion and send it to BAP with distance

- #918: [BKN-681] registry service lookup implementation

- #926: [BKN-829] Add rating and registeredAt to getDriversEPs

- #925: [BKN-807] Add types for duration and distance

- #915: [BKN-798] Cache successful auth

- #913: [BKN-801, BKN-802] Add searchString to ListDrivers EP to search by name and mobileNum

- #916: [BKN-805] Add metrix to fmd-wrapper

- #501: [BKN-484] Add distance calculation and route update via graphhopper

- #907: [BKN-811] Add mock sms service

- #879: [BKN-702] Add CallStack

### Changed

- #954: INFRA-1174 Added annotation excludeOutboundPorts for redis ports

- #955: Improved cancellation text

- #950: transporter: update ratings table to have driverId to make avg ratings calculation simpler

- #949: [BKN-842] Add pagination and ordering for list rides EP

- #937: [BKN-835] Configurable updateLocation refresh period

- #934: Case-insensitive driver list

- #922: [BKN-795] changed error when bpp disabled

- #931: Change Person.rating db type

- #919: [BKN-826] response status validation in Google Maps responses

- #923: [BKN-753] replacing redundant strip prefixes JSON instances with deriving

- #917: [BKN-808] Restrict registration as Admin

- #914: Refactor VerificationResult to PersonId

- #903: BKN-793 latests spec changes implementation

- #911: BKN-817 disabled init API in FMD

- #842: Refactor error classes

- #898: Combine RequestInfo and ResponseInfo into one log line

- #857: [BKN-780] Move core db types

- #834: Refactor measuring durations

- #868: merging domain versions into single data type

- #901: Feature/BKN-792 rate limit start ride

### Fixed

- #951: Fix/cancellation reason api

- #948: [BKN-843] fix: use RIDEORDER PI ids to rate the ride

- #944: [BKN-821] Fix BAP actualDistance and actualPrice update

- #929: [BKN-830] Fixed driver response processing

- #928: Fix metrics time

- #909: BKN-794 FMD complete flow test and flow fixes

- #906: [BKN-797] Fix exotel

- #905: [BKN-796] Fix search failure counter

### Removed

- #890: [BKN-788] Remove metrics typeclasses

- #891: [BKN-785] Remove Transform Typeclasses

## [3.0.0] - 16-07-2021

### Added

- #894: add additional input validations

- #884: [BKN-786] Add registration number check in createDriver

- #881: [BKN-784] Link vehicle in createDriver

- #858: [BKN-764] Add retries to search flow

- #854: [BKN-764] Add configurable http calls timout

- #847: [BKN-762] Order the quotes based on nearest available driver first

### Changed

- #892: update: input validations and baseFare range to be 0 - 500

- #826: Simplify Migration.Gps parser

- #844: [BKN-745] Refactor validation framework to hlists

- #782: [BKN-724] Create vehicle inside createDriver API

- #781: [BKN-725] Restrict vehicle unlinking

- #722: [BKN-677] Replace linkEntity EP by linkVehicle

- #871: Payment schema latest spec changes implementation

- #864: [BKN-734] Allocator optimization

- #870: new json domain values implementation

- #867: filling bpp_id in FMD

### Fixed

- #896: fix: wrong org name in cancellation by organization notification

- #889: fix: btm shards config for sandbox and production

- #887: [BKN-789] Fix login EP call limit

- #882: Fix hardcoded vehicle org id

- #876: [BKN-783] fix nightShiftEnd validation

- #873: fixing tags field parsing in fulfillment from json instance

- #872: init API parsing fix

## [2.0.0] - 10-07-2021

### Added

- #822: [BKN-745] Add validation framework and some of the validations

- #823: Adding driver responses to the RideRequest table

- #831: [BKN-752] Add arn to sesConfig

- #838: [BKN-754] Added ISO-8859-1 instance for PlainText

- #652: [BKN-533] Show different cancellation notifications

- #855: BKN-419 FMD tests

- #856: log cancellation reasons for analytics

- #860: adding local retail domain for gateway

- #862: enabling food and beverage and local retail domains

- #863: add additional log tags in gateway + add requestId in log


### Changed

- #800: [BKN-735] Do not cancel ride if there are not attempted drivers

- #820: [BKN-727] FMD migration to 0.9.1

- #827: Refactor BTM env

- #829: Remove Flow and replace it by constraints over m

- #837: Bring back type family in Person type

- #839: dhall-config: update master gateway selector

- #840: BKN-756 updating info on repeated call init endpoint for FMD

- #841: BKN-757 making confirm usable without preceding API calls

- #843: BKN-759 using Dunzo task id as order id

- #845: [BKN-754] Moved PlainText_ISO_8859_1 to Beckn.Types.Servant

- #848: [BKN-763] Order fare policy list

- #865: dhall-configs: update sms template

- #866: dhall-config: update sandbox NSDL BG url


### Removed

- #821: [BKN-746] Remove encryption from Person type signature

- #824: [BKN-750]/Deleted Delhivery Code From FMD

- #833: [BKN-699, BKN-706] Remove models layer and remove fromMaybeM from queries

- #830: [BKN-751] Remove Types.API.Common

- #849: Remove Inventory type

### Fixed

- #807: [BKN-729] Fix signature auth (use raw request)

- #832: [BKN-749] Fixed Myvaluefirst sms error recognition

- #836: Fix returned Person

- #846: [BKN-755] notifyOnRegistration only on very first login

- #850: Fix ExotelResponse parsing

- #861: Beckn Domain fixes

## [1.0.0] - 21-06-2021

- Go-live production release

[Unreleased]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/compare/commits?targetBranch=refs%2Ftags%2Fv13.0.0&sourceBranch=refs%2Fheads%2Fmaster&targetRepoId=762

[14.11.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.11.0&merges=include
[14.10.1]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.10.1&merges=include
[14.10.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.10.0&merges=include
[14.9.1]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2F14.9.1&merges=include
[14.9.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2F14.9.0&merges=include
[14.8.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.8.0&merges=include
[14.7.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.7.0&merges=include
[14.6.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.6.0&merges=include
[14.5.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.5.0&merges=include
[14.4.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.4.0&merges=include
[14.3.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.3.0&merges=include
[14.2.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2F14.2.0&merges=include
[14.1.2]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.1.2&merges=include
[14.1.1]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.1.1&merges=include
[14.1.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.1.0&merges=include
[14.0.0]: https://bitbucket.juspay.net/projects/BEC/repos/beckn/commits?until=refs%2Ftags%2Fv14.0.0&merges=include
[13.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv13.0.0&merges=include
[12.1.1]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv12.1.1&merges=include
[12.1.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv12.1.0&merges=include
[12.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv12.0.0&merges=include
[11.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv11.0.0&merges=include
[10.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv10.0.0&merges=include
[9.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv9.0.0&merges=include
[8.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv8.0.0&merges=include
[7.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv7.0.0&merges=include
[6.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv6.0.0&merges=include
[5.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv5.0.0&merges=include
[4.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv4.0.0&merges=include
[3.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv3.0.0&merges=include
[2.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv2.0.0&merges=include
[1.0.0]: https://bitbucket.juspay.net/projects/JUSPAY/repos/beckn/commits?until=refs%2Ftags%2Fv1.0.0&merges=include