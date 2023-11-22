# Alchemist

1. Spec definitions
2. Parser: Parse Spec to build a state
3. Generator:
    Haskell :: To build .hs files
      - BeamTable
      - BeamQueries
      - Servant API
      - Domain Types
    SQL DDL :: To build .sql files
      - New tables
      - Module which check for existing migrations and only apply changeset

    In future we can add Rust too.
4. Utils

API Spec Example
```
data TicketServiceVerificationResp = {}

type API = [apiexp|
  GET /org/driver/{placeId:(Id DTB.TicketPlace)}/services?*status:DTB.BookingStatus&limit:Int&offset:Int
  AUTH TokenAuth
  RESP DTB.TicketPlace
  ---
  POST /org/driver/{placeId:(Id DTB.TicketPlace)}/services?*status:DTB.BookingStatus&limit:Int&offset:Int
  AUTH TokenAuth
  H mId (Id Merchant)
  H vt VehicleType
  REQ DTB.TicketBody
  RESP DTB.TicketServiceVerificationResp
|]
```