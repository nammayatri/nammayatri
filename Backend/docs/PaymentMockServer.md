
# Payment Routing Implementation Plan

## Goal Description
Route payment requests to either the mock payment server or the real payment gateway based on an `isMockPayment` flag. This allows testing the payment flow with a mock server while keeping the production flow intact.

## Proposed Changes

### Database Migrations
#### [rider-app]
#### [rider-app]
- [NEW] Update Storage YAML files to define the new column. The system will generate the SQL migration.
    - [MODIFY] `Backend/app/rider-platform/rider-app/Main/spec/Storage/FrfsTicket.yaml`: Add `isMockPayment` to `FRFSTicketBooking`.
    - [MODIFY] `Backend/lib/payment/spec/Storage/Payments.yaml`: Add `isMockPayment` to `PaymentOrder`.
    - [DELETE] Remove the manually created SQL file if it exists.

### Haskell Domain Models

#### [Shared Services / Rider App Types]
- [MODIFY] `FRFSTicketBooking.hs` (Location to be confirmed, likely `lib/shared-services` or `src/Domain/Types`):
    - Add `isMockPayment :: Maybe Bool` to `FRFSTicketBooking` record.

#### [Payment Library]
- [MODIFY] `Backend/lib/payment/src/Lib/Payment/Domain/Types/PaymentOrder.hs`:
    - Add `isMockPayment :: Maybe Bool` to `PaymentOrder` record.

### Business Logic

#### [API Handling]
- [MODIFY] `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/FRFSTicketService.hs`:
    - Update `postFrfsQuoteV2Confirm` and `postFrfsQuoteConfirm` to accept `isMockPayment` query parameter.
    - Pass this flag to `postFrfsQuoteV2ConfirmUtil`.
    - In `postFrfsQuoteV2ConfirmUtil`, pass `isMockPayment` to `confirmAndUpsertBooking`.

#### [Booking Creation]
- [MODIFY] `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSConfirm.hs`:
    - Update `confirmAndUpsertBooking` signature to accept `isMockPayment`.
    - Update `buildAndCreateBooking` to include `isMockPayment` when creating `FRFSTicketBooking`.

#### [Payment Creation]
- [MODIFY] `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFS/OnInit.hs`:
    - In `onInit`, ensure `isMockPayment` is preserved or passed.
    - In `createPayments`, read `isMockPayment` from `FRFSTicketBooking`.
    - Pass `isMockPayment` to `JourneyUtils.postMultimodalPaymentUpdateOrderUtil` or `createPaymentOrder`.

- [MODIFY] `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFS/Common.hs` (or wherever `createPaymentOrder` helper is):
    - Update `createPaymentOrder` (if it exists here or in `OnInit`) to set `isMockPayment` in `PaymentOrder`.

#### [Payment Execution]
- [MODIFY] `Backend/app/rider-platform/rider-app/Main/src/Tools/Payment.hs`:
    - Modify `runWithServiceConfigAndServiceName` (and `runWithServiceConfig*` helpers if needed) to:
        - Check if `isMockPayment` is `False`.
        - If `False`, overwrite `vsc.mockUrl` (or equivalent config) to `Nothing` to force real payment.
        - If `True`, leave it as is (assuming `vsc` has mock URL configured or distinct config handles it). (Or as per User: "use it to overwrite vsc config's mockUrl to Nothing if it is False.")

## Verification Plan

### Automated Tests
- Run `cabal build` to ensure type checks pass.
- Verify migration scripts syntax.

### Manual Verification
- Deploy to staging (User task).
- Test flow with `isMockPayment=true`: Should hit mock server.
- Test flow with `isMockPayment=false` (or omitted): Should hit real server.
