# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://bitbucket.org/juspay/beckn/branches/compare/release-version-7..v7.0.0
[7.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v7.0.0
[6.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v6.0.0
[5.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v5.0.0
[4.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v4.0.0
[3.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v3.0.0
[2.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v2.0.0
[1.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v1.0.0