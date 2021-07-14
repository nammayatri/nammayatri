# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://bitbucket.org/juspay/beckn/branches/compare/master..v2.0.0
[2.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v2.0.0
[1.0.0]: https://bitbucket.org/juspay/beckn/commits/tag/v1.0.0