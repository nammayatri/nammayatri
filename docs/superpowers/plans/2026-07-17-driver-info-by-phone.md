# Driver Info by Phone (multi-city search) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a dashboard endpoint that finds a driver by phone number across all cities the caller has access to, returning the exact `DriverInfoRes` payload the existing `/driver/info` endpoint returns.

**Architecture:** A hand-written Servant module in `provider-dashboard`, mounted into the shared `API'` group so it appears on both the V1 (no-city) and V2 (city-in-path) mount points. Its handler ignores the path city and instead loops over the caller's granted cities, calling the existing per-city `getDriverInfo` driver-app client and short-circuiting on the first hit. No changes to `dynamic-offer-driver-app`.

**Tech Stack:** Haskell, Servant, EulerHS `Flow`, NammaDSL-generated clients, `hspec` for the one new unit test-suite.

## Global Constraints

- NEVER edit files under `src-read-only/` — they are generated. (CLAUDE.md rule 1)
- Project builds with `-Werror`: every GHC warning (unused import, dodgy import) is a compile error. (CLAUDE.md rule 3)
- Verify with `cabal build provider-dashboard` after each code task. (CLAUDE.md rule 2)
- If a `.hs` file is added or deleted, run `, hpack` so the `.cabal` picks it up. (CLAUDE.md conventions)
- Endpoint paths are camelCase. (CLAUDE.md DSL rules)
- Commit format: `<sub-project>/<type>: <summary>`, e.g. `dashboard/feat: ...`. End commit bodies with the `Co-Authored-By` trailer.
- All build/hpack commands run from `Backend/` inside the nix shell (`, <cmd>`).

## Verified facts this plan relies on (do not re-derive)

- Existing dashboard handler: `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/Driver.hs:107-129` (`getDriverInfo`).
- Mount point: `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DynamicOfferDriver.hs` — type `API'` (lines 57-63) is shared by `handler` (V1, line 66) and `handlerV2` (line 82); both call each group as `<Group>.handler merchantId city`.
- Per-city client call (copy the argument order verbatim):
  `Client.callRideBookingAPI checkedMerchantId city (.driverDSL.getDriverInfo) apiTokenInfo.personId.getId mbFleet mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber mbEmail mbPersonId mbWalletId`
  — `Client` = `API.Client.ProviderPlatform.RideBooking`.
- `ApiTokenInfo` fields (`Backend/app/dashboard/Lib/src/Tools/Auth/Api.hs:54-60`): `personId :: Id Person`, `merchant :: DM.Merchant`, `city :: City.City`, `person`, `userActionType`.
- `MerchantAccess` (`Backend/app/dashboard/Lib/src/Domain/Types/MerchantAccess.hs:23-31`): fields include `merchantId :: Id DMerchant.Merchant` and `operatingCity :: City.City`.
- City-list query: `QMerchantAccess.findAllMerchantAccessByPersonId :: Id Person -> m [MerchantAccess]` (`Backend/app/dashboard/Lib/src/Storage/Queries/MerchantAccess.hs:63`).
- Auth combinator: `ApiAuth 'DRIVER_OFFER_BPP 'DSL <uat> :> ...` yields `ApiTokenInfo` as a handler argument (`Backend/app/dashboard/Lib/src/Tools/Auth/Api.hs:48`). Reuse existing userActionType `GET_DRIVER_INFO` (spec decision — no new enum/migration).
- Error surfacing (the crux): the dashboard client uses `callApiUnwrappingApiError (identity @Error)` (`Backend/app/dashboard/Lib/src/Tools/Client.hs:73`). A remote **domain** error is re-thrown as `Tools.Error.Error` (`Backend/app/dashboard/Lib/src/Tools/Error.hs:30-46`) with `contents.errorCode :: Text`. A connection/raw failure is thrown as the *separate* type `ExternalAPICallError`, which `catch @Tools.Error.Error` does NOT catch. Confirmed idiom: `Backend/app/dashboard/Lib/src/SharedLogic/Transaction.hs:143-151` catches `(err :: E.Error)` and re-throws.
- Error code for driver-not-found is `"PERSON_DOES_NOT_EXIST"` (`shared-kernel/lib/mobility-core/src/Kernel/Types/Error.hs:191`, HTTP 400). `MerchantOperatingCityNotFound` = `"MERCHANT_OPERATING_CITY_NOT_FOUND"` (HTTP 500) — treated as a miss defensively but should not occur for granted cities.
- `Flow = FlowR AppEnv` derives `MonadCatch`/`MonadThrow` (`shared-kernel/.../Kernel/Types/Flow.hs:55-56`); `catch`/`handle` come from `Kernel.Prelude`.
- `firstJustM :: (a -> m (Maybe b)) -> [a] -> m (Maybe b)` from `Control.Monad.Extra` (used at `.../RideExtra.hs:6`).
- `PersonDoesNotExist` constructor is in scope at the dashboard via `Tools.Error` (re-exports `Kernel.Types.Error` except `MerchantError`).
- Dashboard has **no** existing unit-test suite (`grep test-suite` over `app/dashboard/**` is empty). Task 1 introduces one, scoped to the pure helpers only.

## File Structure

| File | Responsibility |
|---|---|
| `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/DriverInfoByPhone.hs` (new) | Domain logic: pure city-selection helpers + `getDriverInfoByPhone` loop + `tryCity` |
| `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DriverInfoByPhone.hs` (new) | Servant route type + auth + `withFlowHandlerAPI'` handler |
| `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DynamicOfferDriver.hs` (modify) | Mount new route into `API'` and both handlers |
| `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/Driver.hs` (modify) | Extract `getRequestorFleetFlag` helper; reuse it in existing `getDriverInfo` |
| `Backend/app/dashboard/provider-dashboard/test/Spec.hs` + `.../test/DriverInfoByPhoneSpec.hs` (new) | Unit tests for pure city-selection helpers |
| `Backend/app/dashboard/provider-dashboard/package.yaml` (modify) | Add `tests:` stanza |

---

### Task 1: Pure city-selection helpers + unit tests

Introduces the only pure, edge-case-carrying logic (dedup, token-city-first ordering, cross-merchant filtering) and its test-suite. This is the one piece of new test infrastructure for this package; keep the suite scoped to pure functions so it links cheaply.

**Files:**
- Create: `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/DriverInfoByPhone.hs`
- Create: `Backend/app/dashboard/provider-dashboard/test/Spec.hs`
- Create: `Backend/app/dashboard/provider-dashboard/test/DriverInfoByPhoneSpec.hs`
- Modify: `Backend/app/dashboard/provider-dashboard/package.yaml`

**Interfaces:**
- Produces:
  - `orderCitiesForSearch :: Kernel.Types.Beckn.City.City -> [Kernel.Types.Beckn.City.City] -> [Kernel.Types.Beckn.City.City]` — dedup, then token city first, order otherwise stable.
  - `selectSearchCities :: Kernel.Types.Id.Id DMerchant.Merchant -> Kernel.Types.Beckn.City.City -> [DAccess.MerchantAccess] -> [Kernel.Types.Beckn.City.City]` — filter accesses to the token merchant, project `operatingCity`, then `orderCitiesForSearch`.

- [ ] **Step 1: Create the module with the two pure helpers**

Create `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/DriverInfoByPhone.hs`:

```haskell
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone
  ( getDriverInfoByPhone,
    orderCitiesForSearch,
    selectSearchCities,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver as Common
import qualified API.Client.ProviderPlatform.RideBooking as Client
import Control.Monad.Extra (firstJustM)
import Data.List (sortOn)
import qualified Domain.Action.ProviderPlatform.RideBooking.Driver as RBDriver
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import "lib-dashboard" Environment
import EulerHS.Prelude hiding (id, sortOn)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified "lib-dashboard" Tools.Error as E
import Tools.Auth.Api (ApiTokenInfo (..))
import Tools.Auth.Merchant (CheckedShortId, skipMerchantCityAccessCheck)

orderCitiesForSearch :: City.City -> [City.City] -> [City.City]
orderCitiesForSearch tokenCity = sortOn (/= tokenCity) . nub

selectSearchCities :: Id DM.Merchant -> City.City -> [DAccess.MerchantAccess] -> [City.City]
selectSearchCities tokenMerchantId tokenCity accesses =
  orderCitiesForSearch tokenCity [ma.operatingCity | ma <- accesses, ma.merchantId == tokenMerchantId]
```

Note: `nub` (from `EulerHS.Prelude`) is O(n²) but city lists are tiny. `sortOn (/= tokenCity)` is a stable sort keying `False < True`, so the token city (key `False`) sorts first and the rest keep their order. `getDriverInfoByPhone`, `tryCity`, and the fleet-guard are added in Task 3 — this task compiles with just the two pure functions plus the imports they need. To avoid `-Werror` unused-import failures in this task, trim the import list to only what the two pure functions use (`City`, `Id`, `DM`, `DAccess`, `sortOn`); re-add the rest in Task 3. Simplest: in this task include only:

```haskell
module Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone
  ( orderCitiesForSearch,
    selectSearchCities,
  )
where

import Data.List (sortOn)
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import EulerHS.Prelude hiding (id, sortOn)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id

orderCitiesForSearch :: City.City -> [City.City] -> [City.City]
orderCitiesForSearch tokenCity = sortOn (/= tokenCity) . nub

selectSearchCities :: Id DM.Merchant -> City.City -> [DAccess.MerchantAccess] -> [City.City]
selectSearchCities tokenMerchantId tokenCity accesses =
  orderCitiesForSearch tokenCity [ma.operatingCity | ma <- accesses, ma.merchantId == tokenMerchantId]
```

- [ ] **Step 2: Add the test-suite stanza to package.yaml**

In `Backend/app/dashboard/provider-dashboard/package.yaml`, after the `executables:` block, add:

```yaml
tests:
  provider-dashboard-test:
    main: Spec.hs
    source-dirs: test
    ghc-options:
      - -threaded
      - -rtsopts
      - '"-with-rtsopts=-N -T"'
    dependencies:
      - provider-dashboard
      - lib-dashboard
      - mobility-core
      - hspec
```

- [ ] **Step 3: Write the failing test**

Create `Backend/app/dashboard/provider-dashboard/test/Spec.hs`:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Create `Backend/app/dashboard/provider-dashboard/test/DriverInfoByPhoneSpec.hs`:

```haskell
module DriverInfoByPhoneSpec (spec) where

import Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone (orderCitiesForSearch)
import qualified Kernel.Types.Beckn.City as City
import Test.Hspec

spec :: Spec
spec = describe "orderCitiesForSearch" $ do
  let blr = City.City "Bangalore"
      maa = City.City "Chennai"
      koc = City.City "Kochi"

  it "puts the token city first" $
    orderCitiesForSearch maa [blr, maa, koc] `shouldBe` [maa, blr, koc]

  it "deduplicates cities" $
    orderCitiesForSearch blr [blr, blr, maa] `shouldBe` [blr, maa]

  it "returns empty for no cities" $
    orderCitiesForSearch blr [] `shouldBe` []

  it "keeps non-token order stable when token city absent" $
    orderCitiesForSearch (City.City "Delhi") [blr, maa, koc] `shouldBe` [blr, maa, koc]
```

- [ ] **Step 4: Regenerate cabal and run the test to verify it fails to compile (function-not-yet-exported / suite-new)**

Run:
```bash
, hpack
, cabal build provider-dashboard-test 2>&1 | tail -20
```
Expected: the module compiles the pure helper; if you scoped Step 1 to only the two functions, the test should now **pass** on first run (the helpers already exist). If instead you see a compile error about a missing export, fix the export list in Step 1. (This is a pure function with no I/O; a red-then-green cycle here is the compile itself.)

- [ ] **Step 5: Run the test to verify it passes**

Run:
```bash
, cabal test provider-dashboard-test 2>&1 | tail -30
```
Expected: `4 examples, 0 failures`.

- [ ] **Step 6: Commit**

```bash
git add Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/DriverInfoByPhone.hs \
        Backend/app/dashboard/provider-dashboard/test/Spec.hs \
        Backend/app/dashboard/provider-dashboard/test/DriverInfoByPhoneSpec.hs \
        Backend/app/dashboard/provider-dashboard/package.yaml \
        Backend/app/dashboard/provider-dashboard/provider-dashboard.cabal
git commit -m "dashboard/feat: add pure city-selection helpers for driver infoByPhone

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Extract `getRequestorFleetFlag` helper

Removes the role→fleet duplication so both endpoints share one copy. Behaviour of the existing `getDriverInfo` must not change.

**Files:**
- Modify: `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/Driver.hs`

**Interfaces:**
- Consumes: `ApiTokenInfo` (already imported in this module).
- Produces: `getRequestorFleetFlag :: ApiTokenInfo -> Flow Bool` — `True` iff the caller's role is `FLEET_OWNER` or `RENTAL_FLEET_OWNER`. Export it from the module.

- [ ] **Step 1: Add the helper and export it**

In `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/Driver.hs`, add `getRequestorFleetFlag` to the module export list (alongside `getDriverInfo`), and define it near `getDriverInfo`:

```haskell
getRequestorFleetFlag :: ApiTokenInfo -> Flow Bool
getRequestorFleetFlag apiTokenInfo = do
  encPerson <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  role <- QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  pure $ role.dashboardAccessType == DRole.FLEET_OWNER || role.dashboardAccessType == DRole.RENTAL_FLEET_OWNER
```

- [ ] **Step 2: Rewrite `getDriverInfo` to use the helper**

Replace lines 126-128 of the existing `getDriverInfo` (the `encPerson`/`role`/`let mbFleet` block) with:

```haskell
  mbFleet <- getRequestorFleetFlag apiTokenInfo
```

Leave the surrounding validation and the final `Client.callRideBookingAPI ...` line unchanged.

- [ ] **Step 3: Build to verify no behaviour/compile change**

Run:
```bash
, cabal build provider-dashboard 2>&1 | tail -20
```
Expected: builds clean (no `-Werror` warnings). `QP`, `QRole`, `DRole` imports are still used (by the new helper), so no unused-import errors.

- [ ] **Step 4: Commit**

```bash
git add Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/Driver.hs
git commit -m "dashboard/refactor: extract getRequestorFleetFlag from getDriverInfo

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: Domain action `getDriverInfoByPhone` + `tryCity` loop

The effectful core: merchant-auth check, fleet-owner rejection, city selection, short-circuit loop, and the narrow error matcher that distinguishes "not found in this city" from a real failure.

**Files:**
- Modify: `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/DriverInfoByPhone.hs` (from Task 1)

**Interfaces:**
- Consumes: `RBDriver.getRequestorFleetFlag` (Task 2); `selectSearchCities` (Task 1); `Client.callRideBookingAPI`.
- Produces: `getDriverInfoByPhone :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Flow Common.DriverInfoRes` (args: merchantShortId, path-city [ignored], token info, mobileNumber, mobileCountryCode).

- [ ] **Step 1: Expand the module to full imports and add the loop + tryCity**

Replace the Task-1 module header/exports with the full version, and append the two functions. Final module:

```haskell
module Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone
  ( getDriverInfoByPhone,
    orderCitiesForSearch,
    selectSearchCities,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver as Common
import qualified API.Client.ProviderPlatform.RideBooking as Client
import Control.Monad.Extra (firstJustM)
import Data.List (sortOn)
import qualified Domain.Action.ProviderPlatform.RideBooking.Driver as RBDriver
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import "lib-dashboard" Environment
import EulerHS.Prelude hiding (id, sortOn)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified "lib-dashboard" Tools.Error as E
import Tools.Auth.Api (ApiTokenInfo (..))
import Tools.Auth.Merchant (CheckedShortId, skipMerchantCityAccessCheck)

orderCitiesForSearch :: City.City -> [City.City] -> [City.City]
orderCitiesForSearch tokenCity = sortOn (/= tokenCity) . nub

selectSearchCities :: Id DM.Merchant -> City.City -> [DAccess.MerchantAccess] -> [City.City]
selectSearchCities tokenMerchantId tokenCity accesses =
  orderCitiesForSearch tokenCity [ma.operatingCity | ma <- accesses, ma.merchantId == tokenMerchantId]

getDriverInfoByPhone ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.DriverInfoRes
getDriverInfoByPhone merchantShortId _pathCity apiTokenInfo mbMobileNumber mbMobileCountryCode = do
  unless (merchantShortId == apiTokenInfo.merchant.shortId) $ throwError AccessDenied
  mobileNumber <- fromMaybeM (InvalidRequest "\"mobileNumber\" is required") mbMobileNumber
  let mobileCountryCode = fromMaybe "+91" mbMobileCountryCode
  mbFleet <- RBDriver.getRequestorFleetFlag apiTokenInfo
  when mbFleet $ throwError (InvalidRequest "Fleet Owner can only search with vehicle Number, personId or walletId")
  accesses <- QMerchantAccess.findAllMerchantAccessByPersonId apiTokenInfo.personId
  let cities = selectSearchCities apiTokenInfo.merchant.id apiTokenInfo.city accesses
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  logInfo $ "getDriverInfoByPhone: searching " <> show (length cities) <> " cities for merchant " <> merchantShortId.getShortId
  firstJustM (tryCity checkedMerchantId apiTokenInfo mbFleet mobileNumber mbMobileCountryCode) cities
    >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)

tryCity ::
  CheckedShortId DM.Merchant ->
  ApiTokenInfo ->
  Bool ->
  Text ->
  Maybe Text ->
  City.City ->
  Flow (Maybe Common.DriverInfoRes)
tryCity checkedMerchantId apiTokenInfo mbFleet mobileNumber mbMobileCountryCode city =
  ( Just
      <$> Client.callRideBookingAPI
        checkedMerchantId
        city
        (.driverDSL.getDriverInfo)
        apiTokenInfo.personId.getId
        mbFleet
        (Just mobileNumber)
        mbMobileCountryCode
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
  )
    `catch` \(err :: E.Error) ->
      if err.contents.errorCode `elem` ["PERSON_DOES_NOT_EXIST", "MERCHANT_OPERATING_CITY_NOT_FOUND"]
        then do
          logInfo $ "getDriverInfoByPhone: driver not found in city " <> show city
          pure Nothing
        else throwError err
```

Why this matcher is correct: `catch @E.Error` only catches errors re-thrown from a remote **domain** error (via `identity @Error` in `Tools.Client`). Connection failures / non-API responses are thrown as `ExternalAPICallError`, a different type, so they propagate untouched — a city outage never masquerades as "not found". Only the two whitelisted error codes are swallowed.

- [ ] **Step 2: Build**

Run:
```bash
, cabal build provider-dashboard 2>&1 | tail -30
```
Expected: clean build. If GHC reports an argument-count/type mismatch on `Client.callRideBookingAPI`, diff the argument order against the reference call site in `Driver.hs:129` and match it exactly (the same 10 trailing `Maybe`/`Bool`/`Text` args).

- [ ] **Step 3: Confirm the test-suite still passes (helpers unchanged)**

Run:
```bash
, cabal test provider-dashboard-test 2>&1 | tail -10
```
Expected: `4 examples, 0 failures`.

- [ ] **Step 4: Commit**

```bash
git add Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/DriverInfoByPhone.hs
git commit -m "dashboard/feat: add getDriverInfoByPhone multi-city driver lookup

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: Servant route module + mount

Exposes the domain action over HTTP with auth, and wires it onto both the V1 and V2 mount points.

**Files:**
- Create: `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DriverInfoByPhone.hs`
- Modify: `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DynamicOfferDriver.hs`

**Interfaces:**
- Consumes: `DIByPhoneAction.getDriverInfoByPhone` (Task 3).
- Produces: `API.ProviderPlatform.DriverInfoByPhone.API` and `handler :: ShortId DM.Merchant -> City.City -> FlowServer API`.

- [ ] **Step 1: Create the route module**

Create `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DriverInfoByPhone.hs`:

```haskell
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DriverInfoByPhone
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver as Common
import qualified API.Types.Dashboard.RideBooking
import qualified Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone as DIByPhone
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import Tools.Auth.Api

type API =
  "driver"
    :> "infoByPhone"
    :> ApiAuth
         'DRIVER_OFFER_BPP
         'DSL
         ('API.Types.Dashboard.RideBooking.PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.GET_DRIVER_INFO)
    :> QueryParam "mobileNumber" Text
    :> QueryParam "mobileCountryCode" Text
    :> Get '[JSON] Common.DriverInfoRes

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city apiTokenInfo mbMobileNumber mbMobileCountryCode =
  withFlowHandlerAPI' $
    DIByPhone.getDriverInfoByPhone merchantId city apiTokenInfo mbMobileNumber mbMobileCountryCode
```

Note on the userActionType path: it must be the same three-part promoted type used by the generated `GetDriverInfo` (`Backend/app/dashboard/provider-dashboard/src-read-only/API/Action/ProviderPlatform/RideBooking/Driver.hs:81-87`). If the qualified promoted constructors above do not resolve, open that generated file, copy its exact `ApiAuth 'DRIVER_OFFER_BPP 'DSL (...)` block verbatim, and adjust only the module qualifiers to match this module's imports. Do not invent a new `GET_DRIVER_INFO_BY_PHONE` constructor — reuse `GET_DRIVER_INFO` (spec decision: avoids an enum + role-seeding migration).

- [ ] **Step 2: Mount into `DynamicOfferDriver.hs`**

In `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DynamicOfferDriver.hs`:

Add the import (after line 30, `import qualified API.Action.ProviderPlatform.RideBooking as RideBookingDSL`):
```haskell
import qualified API.ProviderPlatform.DriverInfoByPhone as DriverInfoByPhoneDSL
```

Add to `API'` (extend the alias at lines 57-63):
```haskell
type API' =
  FleetDSL.API
    :<|> AppManagementDSL.API
    :<|> ManagementDSL.API
    :<|> IssueManagementDSL.API
    :<|> RideBookingDSL.API
    :<|> OperatorDSL.API
    :<|> DriverInfoByPhoneDSL.API
```

Add to `handler` (the V1 body, lines 69-74) as the last alternative:
```haskell
    :<|> OperatorDSL.handler merchantId city
    :<|> DriverInfoByPhoneDSL.handler merchantId city
```

Add the identical last alternative to `handlerV2` (lines 84-89):
```haskell
    :<|> OperatorDSL.handler merchantId city
    :<|> DriverInfoByPhoneDSL.handler merchantId city
```

The order of `:<|>` alternatives in `API'` and both handler bodies must match exactly, or Servant will fail to typecheck the server against the API.

- [ ] **Step 3: Regenerate cabal and build**

Run:
```bash
, hpack
, cabal build provider-dashboard 2>&1 | tail -40
```
Expected: clean build. Common failure: mismatch between `API'` alternative order and handler order → a large Servant type error; fix by aligning the two `:<|>` chains.

- [ ] **Step 4: Commit**

```bash
git add Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DriverInfoByPhone.hs \
        Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DynamicOfferDriver.hs \
        Backend/app/dashboard/provider-dashboard/provider-dashboard.cabal
git commit -m "dashboard/feat: expose and mount driver infoByPhone endpoint

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Full build + integration verification

Confirms the whole dashboard compiles and the endpoint behaves end-to-end.

**Files:** none (verification only).

- [ ] **Step 1: Build everything the change touches**

Run:
```bash
, cabal build provider-dashboard provider-dashboard-exe 2>&1 | tail -20
```
Expected: clean, no warnings (`-Werror`).

- [ ] **Step 2: Run the unit suite once more**

Run:
```bash
, cabal test provider-dashboard-test 2>&1 | tail -10
```
Expected: `4 examples, 0 failures`.

- [ ] **Step 3: Manual integration check (requires a running stack + a valid dashboard token)**

Start services if not already running (`, run-mobility-stack-dev`). Then, with a dashboard auth token for a user whose grants include a non-default city and a driver known to exist in that non-default city:

```bash
# Should return the same DriverInfoRes shape as /driver/info, with merchantOperatingCity = the non-default city
curl -s "http://localhost:8018/api/bpp/driver-offer/NAMMA_YATRI_PARTNER/driver/infoByPhone?mobileNumber=<PHONE>&mobileCountryCode=%2B91" \
  -H "token: <DASHBOARD_TOKEN>" | jq '{driverId, mobileNumber, merchantOperatingCity}'
```
(Confirm the provider-dashboard port from `, run-mobility-stack-dev` output; `8018` is the common default.)

Expected results to verify:
- Driver in the token's own city → found (single hop; check logs show 1 city searched or first-hit).
- Driver in a granted non-default city → found; `merchantOperatingCity` reports that city.
- Unknown phone → HTTP 400 with `errorCode: "PERSON_DOES_NOT_EXIST"` — identical to `/driver/info`.
- A fleet-owner token → `InvalidRequest` "Fleet Owner can only search..." before any driver-app call.

- [ ] **Step 4: Final commit (only if any fixups were needed)**

```bash
git add -A
git commit -m "dashboard/feat: driver infoByPhone endpoint verification fixups

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review

**Spec coverage:**
- New endpoint accepting merchant + phone → Task 4 (route), Task 3 (logic). ✓
- Fetch merchant cities / iterate / short-circuit → Task 3 (`selectSearchCities` + `firstJustM`). ✓
- Same not-found error as `/driver/info` → Task 3 (`fromMaybeM (PersonDoesNotExist ...)`). ✓
- Response type identical → Task 4 returns `Common.DriverInfoRes` verbatim from the reused client call. ✓
- Authorization = caller's granted cities only → Task 3 (`selectSearchCities` filters to `apiTokenInfo.merchant.id`; merchant-match check; `skipMerchantCityAccessCheck`). ✓
- Reuse existing logic, avoid duplication → Task 2 (`getRequestorFleetFlag`), Task 3 (reuses `callRideBookingAPI`). ✓
- Linear in #cities, short-circuit → `firstJustM` (lazy). ✓
- Error handling + logging → Task 3 (narrow `catch @E.Error`, `logInfo` per miss and on search start). ✓
- Fleet-owner restriction preserved → Task 3 (reject before loop). ✓
- Sequential vs parallel rationale → captured in spec; no parallelism implemented (correct). ✓
- No `dynamic-offer-driver-app` / `src-read-only` changes → all edits are in `provider-dashboard/src`, `test`, `package.yaml`. ✓

**Placeholder scan:** No TBD/TODO; all code blocks are complete; the one genuinely environment-dependent value (dashboard port, token, test phone) is flagged as such in Task 5 Step 3 because it cannot be known statically.

**Type consistency:** `getRequestorFleetFlag :: ApiTokenInfo -> Flow Bool` (Task 2) is consumed with that exact signature in Task 3. `selectSearchCities`/`orderCitiesForSearch` signatures match between Task 1 and Task 3. `getDriverInfoByPhone`'s 5-arg signature (Task 3) matches the handler call in Task 4. `tryCity`'s argument list matches the `callRideBookingAPI` reference call site.

**Known residual risk:** The `ApiAuth` promoted-type path in Task 4 Step 1 is the one construct that may need the "copy verbatim from the generated file" fallback — called out explicitly in that step.
