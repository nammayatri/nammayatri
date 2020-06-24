# API Authorization

## Backend and transporter APIs

For most API calls, the token needs to be passed as the contents of the `token` header. It is then verified against the `token` field in the `registration_token` table of the `atlas_app` database (backend APIs) and `atlas_transporter` database (transporter APIs). The token is returned by the call to the `/token/verify` endpoint of the registration flow.

## Callbacks

Callbacks (`on_search`, `on_track`, `on_confirm`, `on_cancel`, `on_status`) do not currently use any authorization.

## Special cases

* Create Gateway handler checks the token against the `api_key` field of the `organization` table of the `atlas_transporter` database.