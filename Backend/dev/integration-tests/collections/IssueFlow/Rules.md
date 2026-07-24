# IssueFlow

Integration test for the rider-app (BAP) and dynamic-offer-driver-app (BPP)
Issue / IssueCategory APIs introduced under the shared
`Backend/lib/shared-services/IssueManagement` module. Exercises the dashboard
admin CRUD surface, the BAP/BPP UI consumption surface, and the in-app chat
loop (UI + dashboard sides).

## Scope of one run

A single pass through `01-IssueLifecycle.json` does:

1. **BAP path** (rider-app + rider-dashboard)
   - Rider OTP login (`baseUrl_app`).
   - Dashboard creates an `IssueCategory` (with an inline `IssueMessage`).
   - Dashboard fetches the category detail and captures the inline message's id.
   - Rider lists categories and asserts the new one is visible.
   - Dashboard creates an `IssueOption` keyed by the captured message id.
   - Rider creates an `IssueReport` referencing the new category/option.
   - Two-way chat: rider posts, dashboard operator posts, rider re-lists, rider marks read.
   - **Config update PATCH regression** (the bug fixed at
     `shared-services/src/IssueManagement/Domain/Action/Dashboard/Issue.hs:1553`):
     write `merchantName` + `supportEmail`, verify both persist, then write
     only `supportEmail`, verify the previously-set `merchantName` is preserved.
   - Cleanup: delete option then category.

2. **BPP path** (dynamic-offer-driver-app + provider-dashboard)
   - Same shape, driver-side: OTP login on `baseURL_namma_P`, category +
     option creation on `/issue/...` under `bpp/driver-offer/...`, driver UI
     consumption + chat round-trip, config update + read-back, cleanup.

## Endpoints exercised (by side)

| Step                             | BAP path                                                          | BPP path                                                              |
|----------------------------------|-------------------------------------------------------------------|-----------------------------------------------------------------------|
| Create category (dashboard)      | `POST /bap/:m/:c/issueV2/category/create`                         | `POST /bpp/driver-offer/:m/:c/issue/category/create`                  |
| Category detail (dashboard)      | `GET /bap/:m/:c/issueV2/category/{id}/detail`                     | `GET /bpp/driver-offer/:m/:c/issue/category/{id}/detail`              |
| List categories (UI)             | `GET /issue/category` on rider-app                                | `GET /issue/category` on driver-app                                   |
| Create option (dashboard)        | `POST /bap/:m/:c/issueV2/option/create?issueCategoryId&issueMessageId` | `POST /bpp/driver-offer/:m/:c/issue/option/create?issueCategoryId&issueMessageId` |
| Create issue report (UI)         | `POST /issue` on rider-app                                        | `POST /issue` on driver-app                                           |
| Chat send (UI)                   | `POST /issue/{id}/chat/message` on rider-app                      | `POST /issue/{id}/chat/message` on driver-app                         |
| Chat send (dashboard)            | `POST /bap/:m/:c/issueV2/{id}/chat/message`                       | `POST /bpp/driver-offer/:m/:c/issue/{id}/chat/message`                |
| Chat list (UI)                   | `GET /issue/{id}/chat/messages` on rider-app                      | `GET /issue/{id}/chat/messages` on driver-app                         |
| Chat mark read (UI)              | `POST /issue/{id}/chat/read` on rider-app                         | (not exercised on driver — same handler)                              |
| Config get/update                | `GET /bap/:m/:c/issueV2/config`, `POST /bap/:m/:c/issueV2/config/update` | `GET /bpp/driver-offer/:m/:c/issue/config`, `POST .../issue/config/update` |
| Delete category/option           | `DELETE /bap/:m/:c/issueV2/{category|option}/{id}/delete`         | `DELETE /bpp/driver-offer/:m/:c/issue/{category|option}/{id}/delete`  |

## Idempotency

- Per-run rider phone number is generated in the collection-level prerequest
  (`_test_rider_number = "8" + 9 random digits`).
- All test artefacts (category label, option label, merchantName,
  supportEmail) are suffixed with `_test_run_suffix` so concurrent or repeated
  runs do not collide on label uniqueness.
- Cleanup steps at the end delete the option then the category created by the
  same run.

## What this collection does **not** cover

- `POST /message/upsert` — multipart-form endpoint; messages are exercised
  inline via the `messages: [...]` field on `category/create`.
- `POST /category/copy{,All,AllDefault}` — these require pre-seeded source
  data in a different operating city; covered separately if needed.
- `POST /translation/bulk` and `GET /translation/list` — translations are
  exercised inline via `translations:` on category/option create.
- `POST /category/{id}/preview` — covered by category detail.
- `POST /category/reorder`, `option/reorder`, `message/reorder` — possible
  follow-up `02-IssueReorderFlow.json` if priority is asserted explicitly.
- FCM push assertions for chat — `mbSendChatNotification` on both sides logs
  to the local FCM client config; no mock-server hook is registered. The
  collection asserts the chat-send API returns 200; the push side-effect is
  not asserted.

## Adding a new city

Drop a `Local/Local_<PREFIX>_<City>.postman_environment.json` mirroring
`Local_NY_Bangalore.postman_environment.json`, swapping `city`,
`bap_merchant_short_id`, `bpp_merchant_short_id`, and the
`*_dashboard_token` values for the target city's seeded test data.
