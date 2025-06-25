# Task Tracker

## Current Task
Implementing updates to the codebase:
1.  **Schema Changes**: Remove `GTFSFeedInfo` table and use `IntegratedBppConfigId` as `feedId`.
2.  **API Changes**: Add `IntegratedBppConfigId` as an optional field to `/frfs/routes`, `/frfs/stations`, `/frfs/route/{routeCode}`, and `/frfs/search` endpoints.
3.  **Flow Changes**: Move `GTFSFeedInfo` queries to `IntegratedBPPConfig` queries in `OTPRest.hs` and other files. Replace `GTFSFeedInfo` usage with `IntegratedBPPConfig` everywhere.

## Sub-tasks:
- [x] Update Memory Bank (task-tracker.md, activeContext.md, progress.md)
- [x] Schema Removal:
    - [x] Identify and remove `GTFSFeedInfo` table definition.
    - [x] Delete or refactor `Backend/app/rider-platform/rider-app/Main/src/Domain/Types/GTFSFeedInfo.hs`. (Will be auto-cleaned)
    - [ ] Remove `Backend/app/rider-platform/rider-app/Main/src-read-only/Storage/Queries/GTFSFeedInfo.hs` (Will be auto-cleaned) and `Backend/app/rider-platform/rider-app/Main/src/Storage/CachedQueries/GTFSFeedInfo.hs`.
- [ ] API Modifications:
    - [ ] Modify `Backend/app/rider-platform/rider-app/Main/spec/API/FrfsTicket.yaml`.
    - [ ] Update API handler functions in `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/FRFSTicketService.hs`.
- [ ] Code Refactoring (GTFSFeedInfo to IntegratedBPPConfig):
    - [ ] Modify `Backend/app/rider-platform/rider-app/Main/src/Storage/CachedQueries/OTPRest/OTPRest.hs`.
    - [ ] Search for and update other files using `GTFSFeedInfo`.
    - [ ] Ensure consistent use of `IntegratedBppConfigId` as `feedId`.
    - [ ] Adapt logic for multiple `IntegratedBPPConfig` per `PlatformType`, `MerchantID`, `MerchantOperatingCityID` for `VehicleType`.
- [ ] Verification and Cleanup:
    - [ ] Compile and Test.
    - [ ] Code Cleanup.

## Affected Files (Confirmed List):
- `memory-bank/task-tracker.md`
- `memory-bank/activeContext.md`
- `memory-bank/progress.md`
- `memory-bank/systemPatterns.md`
- `memory-bank/techContext.md`
- `.clinerules`
- `Backend/app/rider-platform/rider-app/Main/src/Storage/CachedQueries/OTPRest/OTPRest.hs`
- `Backend/app/rider-platform/rider-app/Main/spec/API/FrfsTicket.yaml`
- `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/FRFSTicketService.hs`
- `Backend/app/rider-platform/rider-app/Main/src-read-only/Storage/Queries/GTFSFeedInfo.hs`
- `Backend/app/rider-platform/rider-app/Main/src/Storage/CachedQueries/GTFSFeedInfo.hs`
- `Backend/app/rider-platform/rider-app/Main/src/Domain/Types/GTFSFeedInfo.hs`
- `Backend/app/rider-platform/rider-app/Main/src/Storage/Beam/GTFSFeedInfo.hs` (or similar, to be confirmed by reading `package.yaml` or `rider-app.cabal` for schema definitions)
- Other files using `GTFSFeedInfo` (to be identified via search)
