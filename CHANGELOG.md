# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://bitbucket.org/juspay/beckn/branches/compare/master..v3.0.0
[3.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v3.0.0
[2.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v2.0.0
[1.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v1.0.0