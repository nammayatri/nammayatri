# Namma Yatri Backend -- Complete API Reference

> Auto-generated from source code analysis. Covers rider-app (port 8013) and dynamic-offer-driver-app (port 8016).

---

# RIDER-APP SERVICE (port 8013)

## 1. Registration Domain

---

### [POST] /v2/auth
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** NoAuth

**Request body:**
```haskell
data AuthReq = AuthReq
  { mobileNumber                :: Maybe Text
  , mobileCountryCode           :: Maybe Text
  , identifierType              :: Maybe IdentifierType    -- MOBILENUMBER | EMAIL | AADHAAR
  , merchantId                  :: ShortId Merchant
  , deviceToken                 :: Maybe Text
  , notificationToken           :: Maybe Text              -- JSON key: "userId"
  , whatsappNotificationEnroll  :: Maybe OptApiMethods
  , firstName                   :: Maybe Text
  , middleName                  :: Maybe Text
  , lastName                    :: Maybe Text
  , email                       :: Maybe Text
  , businessEmail               :: Maybe Text
  , language                    :: Maybe Language
  , gender                      :: Maybe Gender
  , otpChannel                  :: Maybe OTPChannel        -- SMS | WHATSAPP | EMAIL
  , registrationLat             :: Maybe Double
  , registrationLon             :: Maybe Double
  , enableOtpLessRide           :: Maybe Bool
  , allowBlockedUserLogin       :: Maybe Bool
  , isOperatorReq               :: Maybe Bool
  , reuseToken                  :: Maybe Bool
  }
```

**Response body:**
```haskell
data AuthRes = AuthRes
  { authId          :: Id RegistrationToken
  , attempts        :: Int
  , authType        :: LoginType              -- OTP | PASSWORD | DIRECT | OAUTH
  , token           :: Maybe Text
  , person          :: Maybe PersonAPIEntity
  , isPersonBlocked :: Bool
  , depotCode       :: Maybe (Id Depot)
  , isDepotAdmin    :: Maybe Bool
  }
```

**Query params:** None

**Business logic summary:** Initiates OTP-based authentication. Validates the request, looks up or creates the person by mobile/email, generates an OTP, creates a RegistrationToken, and sends the OTP via SMS/WhatsApp/email. Performs IP-based fraud detection in a background fork. If the user is blocked, the OTP send is skipped but the token is still returned.

**DB tables touched:** `person`, `person_stats`, `registration_token`, `merchant`, `rider_config`, `merchant_config`, `depot_manager`, `person_default_emergency_number`

**Side effects:** Redis sliding window rate limit; OTP sent via SMS/WhatsApp/Email; Yudhishthira NammaTags computed for new users

---

### [POST] /v2/auth/signature
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** SignatureAuth (RSA signature in `x-sdk-authorization` header)

**Request body:** Same `AuthReq` as above
**Response body:** Same `AuthRes` -- but `token` and `person` are populated immediately (no OTP step)

**Query params:** None

**Business logic summary:** SDK-based signature authentication. Decrypts mobile number and notification token using merchant AES cipher. Finds or creates person, creates a registration token marked as DIRECT auth (verified immediately), updates personal info, runs the verify flow. No OTP is sent.

**DB tables touched:** `person`, `person_stats`, `registration_token`, `merchant`, `person_disability`, `safety_settings`

**Side effects:** Redis token cache cleanup; WhatsApp opt-in API (forked); REGISTRATION_APPROVED push notification for new persons

---

### [POST] /v2/auth/password
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** NoAuth

**Request body:**
```haskell
data PasswordAuthReq = PasswordAuthReq
  { userEmail         :: Maybe Text
  , userMobileNumber  :: Maybe Text
  , userCountryCode   :: Maybe Text
  , userMerchantId    :: Id Merchant
  , userPassword      :: Text
  , isOperatorReq     :: Maybe Bool
  }
```

**Response body:** Same `AuthRes` with token populated

**Query params:** None

**Business logic summary:** Password-based auth. Looks up person by mobile or email, verifies password hash, creates a verified registration token, deletes old tokens. Supports operator/depot manager flows.

**DB tables touched:** `person`, `registration_token`, `depot_manager`

**Side effects:** Redis sliding window rate limit

---

### [POST] /v2/auth/{authId}/verify
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** NoAuth

**Request body:**
```haskell
data AuthVerifyReq = AuthVerifyReq
  { otp                        :: Text         -- exactly 4 digits
  , deviceToken                :: Text
  , whatsappNotificationEnroll :: Maybe OptApiMethods
  , userPasswordString         :: Maybe Text
  }
```

**Response body:**
```haskell
data AuthVerifyRes = AuthVerifyRes
  { token  :: Text
  , person :: PersonAPIEntity
  }
```
Where `PersonAPIEntity` contains: `id`, `firstName`, `middleName`, `lastName`, `email`, `maskedMobileNumber`, `maskedDeviceToken`, `hasTakenRide`, `hasTakenValidRide`, `referralCode`, `whatsappNotificationEnrollStatus`, `language`, `hasDisability`, `disability`, `gender`, `businessEmail`, `businessProfileVerified`, `hasCompletedSafetySetup`, `hasCompletedMockSafetyDrill`, `bundleVersion`, `clientVersion`, `followsRide`, `isSafetyCenterDisabled`, `customerTags`

**Query params:** None

**Business logic summary:** Verifies OTP, marks token as verified, updates device token, runs verify flow (delete old tokens, WhatsApp opt-in, notify new users, build PersonAPIEntity). Optionally sets a password and generates a customer referral code if missing. Performs fraud detection on device token matching.

**DB tables touched:** `registration_token`, `person`, `person_disability`, `safety_settings`, `merchant_service_usage_config`

**Side effects:** Redis rate limit + token cache cleanup; REGISTRATION_APPROVED push notification; WhatsApp opt-in/out (forked); fraud detection on device token

---

### [POST] /v2/auth/otp/{authId}/resend
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** NoAuth

**Request body:** None
**Response body:** `AuthRes` (token and person are Nothing)
**Query params:** None

**Business logic summary:** Resends the OTP for an existing registration token. Retrieves stored OTP, re-sends via cached channel, decrements attempts counter.

**DB tables touched:** `registration_token`, `person`, `rider_config`

**Side effects:** SMS/WhatsApp/Email OTP resend; Redis OTP channel read

---

### [POST] /v2/auth/get-token
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** NoAuth

**Request body:**
```haskell
data GetTokenReq = GetTokenReq
  { appSecretCode :: Text
  , userMobileNo  :: Text
  }
```

**Response body:** `AuthRes`
**Query params:** None

**Business logic summary:** Retrieves auth token using a temporary app secret code (from generate-temp-app-code). Looks up personId from Redis, finds their registration token. Rate-limited to 3 attempts/hour.

**DB tables touched:** `person`, `registration_token`

**Side effects:** Redis read/rate limit

---

### [POST] /v2/auth/generate-temp-app-code
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** TokenAuth

**Request body:** None
**Response body:** `TempCodeRes { tempCode :: Text }`
**Query params:** None

**Business logic summary:** Generates a short-lived (2 min) temporary numeric code for WebView-to-native app handoff. Code stored in Redis keyed by mobile hash + code -> personId.

**DB tables touched:** `person`

**Side effects:** Redis counter + key store with 120s TTL

---

### [POST] /v2/auth/business-email/send-verification
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** None
**Response body:** `APISuccess`

**Business logic summary:** Sends a verification email with OTP + magic link to the person's businessEmail. Creates a RegistrationToken with the OTP hash.

**DB tables touched:** `person`, `rider_config`, `registration_token`
**Side effects:** Redis rate limit; Email (SES) verification email

---

### [POST] /v2/auth/business-email/verify
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `VerifyBusinessEmailReq { token :: Maybe Text, otp :: Maybe Text }`
**Response body:** `VerifyBusinessEmailRes { message :: Text, verified :: Bool }`

**Business logic summary:** Verifies business email via OTP. Marks token verified and sets businessProfileVerified on person.

**DB tables touched:** `registration_token`, `person`
**Side effects:** Redis rate limit

---

### [POST] /v2/auth/makeSignature
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** None
**Response body:** `SignedResponse CustomerSignatureRes` containing customerId, customerPhoneNumber, customerName, signature, timestamp, merchantId

**Business logic summary:** Signs customer identity data with merchant RSA private key.

**DB tables touched:** `person`, `merchant`
**Side effects:** None

---

### [POST] /v2/auth/logout
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** None
**Response body:** `APISuccess`

**Business logic summary:** Clears all cached auth tokens, clears device token, deletes all registration tokens.

**DB tables touched:** `registration_token`, `person`
**Side effects:** Redis auth token cache deletion

---

## 2. Search Domain

---

### [POST] /rideSearch
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Search.hs:154`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs`
**Auth:** TokenAuth

**Request body:** `SearchReq` -- tagged union with `fareProduct` discriminator:
| fareProduct | Inner type |
|---|---|
| `ONE_WAY` | `OneWaySearchReq` |
| `RENTAL` | `RentalSearchReq` |
| `INTER_CITY` | `InterCitySearchReq` |
| `AMBULANCE` | `OneWaySearchReq` |
| `DELIVERY` | `OneWaySearchReq` |
| `PTSearch` | `PublicTransportSearchReq` |
| `FIXED_ROUTE` | `FixedRouteSearchReq` |

Key nested type `OneWaySearchReq`: `origin :: SearchReqLocation`, `destination :: Maybe SearchReqLocation`, `stops :: Maybe [SearchReqLocation]`, `startTime :: Maybe UTCTime`, `isReallocationEnabled :: Maybe Bool`, `quotesUnifiedFlow :: Maybe Bool`, `sessionToken :: Maybe Text`, `driverIdentifier :: Maybe DriverIdentifier`, `isMeterRideSearch :: Maybe Bool`, `doMultimodalSearch :: Maybe Bool`

Where `SearchReqLocation { gps :: LatLong, address :: LocationAddress }` and `LocationAddress { street, door, city, state, country, building, areaCode, area, ward, placeId, instructions, title, extras :: Maybe Text }`

**Response body:**
```haskell
data SearchResp = SearchResp
  { searchId     :: Id SearchRequest
  , searchExpiry :: UTCTime
  , routeInfo    :: Maybe RouteInfo   -- duration, distance, points, snappedWaypoints
  }
```

**Query params:** `filterServiceAndJrnyType :: Maybe Bool`

**Business logic summary:** Enforces per-person sliding-window rate limit. Cancels any previous active search/booking. Creates a SearchRequest record, computes routes via Maps APIs (Google/OSRM). Forks a BECKN v2 search request to the gateway. If multimodal is enabled, forks multi-modal transit route computation.

**DB tables touched:** `search_request`, `location`, `person`, `person_disability`, `merchant`, `merchant_operating_city`, `rider_config`, `merchant_config`, `person_flow_status`, `saved_req_location`, `hot_spot_config`, `special_location`, `estimate`, `booking`, `ride`

**Side effects:** Redis rate limit + hotspot frequency updates; Kafka autocomplete/route events; Maps routing APIs; BECKN gateway search; Slack alert on rate limit exceeded; search metrics

---

### [POST] /multimodalSearch
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Search.hs:138`
**Handler:** `app/rider-platform/rider-app/Main/src/API/UI/Search.hs:283`
**Auth:** TokenAuth

**Request body:** Same `SearchReq`
**Response body:**
```haskell
data MultimodalSearchResp = MultimodalSearchResp
  { searchId               :: Id SearchRequest
  , searchExpiry           :: UTCTime
  , journeys               :: [JourneyData]
  , firstJourney           :: Maybe JourneyData
  , firstJourneyInfo       :: Maybe JourneyInfoResp
  , showMultimodalWarning  :: Bool
  , multimodalWarning      :: Maybe MultimodalWarning
  , crisSdkToken           :: Maybe Text
  , viaRoutes              :: [ViaRouteDetails]
  }
```

**Query params:** `filterServiceAndJrnyType :: Maybe Bool`

**Business logic summary:** Creates search request, queries multi-modal transit routes (bus/metro/subway + taxi legs) via Google Transit/OTP. Creates Journey and JourneyLeg DB records. If `initateJourney` header is true, the first journey is immediately initiated with fare quotes.

**DB tables touched:** All from /rideSearch plus `journey`, `journey_leg`, `multimodal_preferences`, `integrated_bpp_config`, `merchant_service_config`

**Side effects:** All from /rideSearch plus CRIS SDK token retrieval, OTP Rest station lookups, FRFS search for public transport legs

---

## 3. Select Domain

---

### [POST] /estimate/{estimateId}/select
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Select.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs`
**Auth:** TokenAuth

**Request body:**
```haskell
data DSelectReq = DSelectReq
  { customerExtraFee             :: Maybe Money
  , customerExtraFeeWithCurrency :: Maybe PriceAPIEntity
  , autoAssignEnabled            :: Bool
  , autoAssignEnabledV2          :: Maybe Bool
  , isPetRide                    :: Maybe Bool
  , paymentMethodId              :: Maybe PaymentMethodId
  , paymentInstrument            :: Maybe PaymentInstrument
  , otherSelectedEstimates       :: Maybe [Id Estimate]
  , isAdvancedBookingEnabled     :: Maybe Bool
  , deliveryDetails              :: Maybe DeliveryDetails
  , disabilityDisable            :: Maybe Bool
  , billingCategory              :: Maybe BillingCategory
  , preferSafetyPlus             :: Maybe Bool
  }
```

**Response body:** `DSelectResultRes { selectTtl :: Int }`

**Query params:** None

**Business logic summary:** BECKN `select` flow. Acquires Redis lock per customer. Validates estimate, payment instrument, business profile. Updates search request with auto-assign settings. Updates person flow status to WAITING_FOR_DRIVER_OFFERS. Marks estimate as DRIVER_QUOTE_REQUESTED. Sends BECKN select V2 to BPP.

**DB tables touched:** `estimate`, `person`, `merchant`, `search_request`, `person_flow_status`, `driver_offer`, `journey`, `journey_leg`, `rider_config`, `beckn_config`, `search_request_parties_link`, `parcel_details`

**Side effects:** Redis lock (60s); BECKN select V2 to BPP; payment method default update

---

### [GET] /estimate/{estimateId}/results
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** None
**Response body:** `QuotesResultResponse { selectedQuotes :: Maybe SelectListRes, bookingId :: Maybe (Id Booking), batchConfig :: Maybe BatchConfig, bookingIdV2 :: Maybe (Id Booking) }`
**Query params:** None

**Business logic summary:** Primary polling endpoint after select. Checks if a booking was created for this estimate's search request. If not, returns active quotes.

**DB tables touched:** `estimate`, `booking`, `quote`, `bpp_details`
**Side effects:** Redis read for batch config

---

## 4. Booking & Confirm Domain

---

### [POST] /rideSearch/quotes/{quoteId}/confirm
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Confirm.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Confirm.hs` -> `SharedLogic.Confirm`
**Auth:** TokenAuth

**Request body:** None

**Response body:** `ConfirmRes { bookingId :: Id Booking, confirmTtl :: Int }`

**Query params:** `paymentMethodId :: Maybe PaymentMethodId`, `paymentInstrument :: Maybe PaymentInstrument`, `isAdvancedBookingEnabled :: Maybe Bool`

**Business logic summary:** Triggers BECKN `init` (not `confirm`). Acquires Redis lock, checks for active bookings, creates Booking record with status NEW, generates displayBookingId, determines insurance eligibility. For delivery: creates BookingPartiesLink. For scheduled rides: creates scheduler job. Sends BECKN init to BPP. On failure: cancels booking and sends notification.

**DB tables touched:** `quote`, `merchant`, `search_request`, `person`, `booking`, `booking_parties_link`, `booking_cancellation_reason`, `person_flow_status`, `estimate`, `exophone`, `merchant_operating_city`, `beckn_config`, `merchant_payment_method`, `insurance_config`

**Side effects:** Redis lock (10s); BECKN init to BPP; Kafka BookingCreatedEvent; scheduler job for scheduled rides; push notification on error

---

### [POST] /rideBooking/{rideBookingId}
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Booking.hs:47`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Booking.hs`
**Auth:** TokenAuth

**Request body:** None
**Response body:** `BookingAPIEntity` -- 50+ fields including: `id`, `status` (NEW|CONFIRMED|AWAITING_REASSIGNMENT|REALLOCATED|COMPLETED|CANCELLED|TRIP_ASSIGNED), `estimatedFareWithCurrency`, `fromLocation`, `rideList :: [RideAPIEntity]`, `fareBreakup :: [FareBreakupAPIEntity]`, `bookingDetails` (variant: OneWay|Rental|DriverOffer|InterCity|Ambulance|Delivery|MeterRide), `tripCategory`, `vehicleCategory`, `paymentInstrument`, `sosStatus`, `billingCategory`, `isInsured`, `mbJourneyId`
**Query params:** None

**Business logic summary:** Booking status v1. Fetches booking, forks stale-booking resync via BECKN status. Auto-cancels if confirm TTL expired. Caches emergency contact SOS data in Redis.

**DB tables touched:** `booking`, `ride`, `fare_breakup`, `exophone`, `bpp_details`, `sos`, `person`, `booking_cancellation_reason`, `beckn_config`, `rider_config`, `journey_leg`, `booking_parties_link`, `location_mapping`

**Side effects:** Redis SOS cache (24h TTL); BECKN status/cancel calls if stale

---

### [GET] /rideBooking/v2/{rideBookingId}
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `BookingStatusAPIEntity` -- lightweight: `id`, `bookingStatus`, `rideStatus`, `talkedWithDriver`, `estimatedEndTimeRange`, `driverArrivalTime`, `sosStatus`, `stopInfo`, `batchConfig`

**Business logic summary:** Lightweight polling endpoint for frequent status checks during booking lifecycle.

---

### [GET] /rideBooking/list
**Service:** rider-app
**Auth:** TokenAuth
**Query params:** `limit`, `offset`, `onlyActive`, `status`, `clientId`, `fromDate`, `toDate`, `rideStatus`
**Response body:** `BookingListRes { list :: [BookingAPIEntity] }`

---

### [GET] /rideBooking/listV2
**Service:** rider-app
**Auth:** TokenAuth
**Query params:** `limit`, `offset`, `bookingOffset`, `journeyOffset`, `passOffset`, `fromDate`, `toDate`, `billingCategory`, `rideType`, `rideStatus`, `journeyStatus`, `isPaymentSuccess`, `bookingRequestType`, `sendEligiblePassIfAvailable`, `passTypes`
**Response body:** `BookingListResV2 { list :: [BookingAPIEntityV2], bookingOffset, journeyOffset, passOffset, hasMoreData }` where `BookingAPIEntityV2` = `Ride BookingAPIEntity | MultiModalRide JourneyInfoResp | MultiModalPass PurchasedPassAPIEntity`

**Business logic summary:** Primary "My Rides" endpoint. Merges bookings, journeys, and passes by timestamp into a time-ordered list.

---

### [POST] /rideBooking/{rideBookingId}/addStop
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `StopReq { gps :: LatLong, address :: LocationAddress }`
**Response body:** `APISuccess`
**Business logic summary:** Adds stop to Rental booking. Creates Location + LocationMapping. Sends BECKN update (AddStop) to BPP.

---

### [POST] /rideBooking/{rideBookingId}/editStop
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `StopReq`
**Response body:** `APISuccess`
**Business logic summary:** Edits existing stop. Sends BECKN update (EditStop) to BPP.

---

### [POST] /rideBooking/invoice/generate
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `GenerateInvoiceReq { startDate, endDate, rideTypes, billingCategories, email, bookingId }`
**Response body:** `GenerateInvoiceRes { invoiceId, totalBookings, totalAmount, bookingAPIEntities, status, message }`

---

### [GET] /rideBooking/{rideBookingId}/paymentStatus
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `PaymentStatusResp` -- full payment order status with bank errors, refunds, VPA, card info

**Business logic summary:** For payment-before-confirm bookings. Syncs order status with Juspay payment gateway.

---

## 5. Ride Domain

---

### [POST] /ride/{rideId}/driver/location
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Ride.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Ride.hs`
**Auth:** TokenAuth

**Request body:** None
**Response body:** `GetDriverLocResp { lat :: Double, lon :: Double, lastUpdate :: UTCTime, pickupEtaInMinutes :: Maybe Int, pickupDist :: Meters }`
**Query params:** None

**Business logic summary:** Fetches real-time driver location. For value-add NPs, calls BPP tracking URL directly. For ONDC providers, calls BECKN track API. Computes pickup ETA from cached/fresh route.

**DB tables touched:** `ride`, `booking`
**Side effects:** Redis pickup route cache; external BPP tracking call; OSRM route fetch on cache miss

---

### [GET] /ride/{rideId}/status
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `GetRideStatusResp { fromLocation, toLocation, ride :: RideAPIEntity, customer :: PersonAPIEntity, driverPosition :: Maybe LatLong }`

**Business logic summary:** Full ride status view with driver position, customer info, and complete ride entity (50+ fields).

---

### [POST] /ride/{rideId}/edit/location
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `EditLocationReq { origin :: Maybe EditLocation, destination :: Maybe EditLocation }` where `EditLocation { gps :: LatLong, address :: LocationAddress }`
**Response body:** `EditLocationResp { bookingUpdateRequestId :: Maybe (Id BookingUpdateRequest), result :: Text }`

**Business logic summary:** Pickup edit: validates NEW status, distance threshold, driver not too close. Destination edit: creates BookingUpdateRequest with SOFT status. Both send BECKN update to BPP.

**DB tables touched:** `ride`, `booking`, `person`, `merchant`, `location`, `location_mapping`, `booking_update_request`
**Side effects:** BPP driver location call; BECKN update to BPP

---

### [GET] /ride/{rideId}/deliveryImage
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `Text` (Base64 image)
**Business logic summary:** Retrieves delivery proof image from BPP internal API.

---

### [GET] /ride/driver/photo/media
**Service:** rider-app
**Auth:** TokenAuth
**Query params:** `filePath :: Text` (mandatory)
**Response body:** `Text` (Base64 image)
**Business logic summary:** Fetches driver profile photo from S3.

---

## 6. Cancel Domain

---

### [POST] /rideBooking/{rideBookingId}/softCancel
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** None
**Response body:** `APISuccess`

**Business logic summary:** First phase of two-phase cancellation. Sends BECKN cancel with SOFT_CANCEL to BPP. No local state changes.

**Side effects:** BECKN cancel V2 to BPP

---

### [GET] /rideBooking/cancellationDues
**Service:** rider-app
**Auth:** TokenAuth
**Query params:** `rideBookingId :: Maybe (Id Booking)`
**Response body:** `CancellationDuesDetailsRes { cancellationDues :: Maybe PriceAPIEntity, disputeChancesUsed :: Maybe Int, canBlockCustomer :: Maybe Bool }`

**Business logic summary:** Retrieves cancellation dues. If ride has cancellationFeeIfCancelled, returns directly. Otherwise calls BPP internal API.

---

### [POST] /rideBooking/{rideBookingId}/cancel
**Service:** rider-app
**Auth:** TokenAuth

**Request body:**
```haskell
data CancelReq = CancelReq
  { reasonCode                :: CancellationReasonCode
  , reasonStage               :: CancellationStage    -- OnSearch | OnInit | OnConfirm | OnAssign
  , additionalInfo            :: Maybe Text
  , reallocate                :: Maybe Bool
  , blockOnCancellationRate   :: Maybe Bool
  }
```

**Response body:** `APISuccess`

**Business logic summary:** Full booking cancellation. Validates cancellability, computes driver distance to pickup, creates BookingCancellationReason, acquires shared cancellation lock (30s), sends BECKN cancel CONFIRM_CANCEL to BPP.

**DB tables touched:** `booking`, `ride`, `merchant`, `merchant_operating_city`, `booking_cancellation_reason`
**Side effects:** BPP driver location; Maps API distance; Redis cancellation lock + optional blocking key; BECKN cancel V2

---

## 7. Rating Domain

---

### [POST] /feedback/rateRide
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Rating.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Feedback.hs`
**Auth:** TokenAuth

**Request body:**
```haskell
data FeedbackReq = FeedbackReq
  { rideId            :: Id Ride
  , rating            :: Int                   -- 1-5
  , feedbackDetails   :: Maybe Text
  , wasRideSafe       :: Maybe Bool
  , shouldFavDriver   :: Maybe Bool
  , wasOfferedAssistance :: Maybe Bool
  , mbAudio           :: Maybe Text            -- base64 audio
  , feedbackAnswers   :: Maybe [FeedbackAnswer]
  }
```

**Response body:** `APISuccess`

**Business logic summary:** Validates rating 1-5 and ride COMPLETED. Uploads audio to S3 if provided. Updates ride rating/safety. Sends BECKN rating V2 to BPP (forked). For low ratings with sensitive words, creates Kapture ticket + Slack notification.

**DB tables touched:** `ride`, `booking`, `merchant`, `rider_config`, `person`, `person_flow_status`, `issue`, `feedback_form`
**Side effects:** S3 audio upload; BECKN rating V2 (forked); BPP feedbackForm call (forked); Kapture ticket; Slack notification

---

### [GET] /knowYourDriver/{rideId}
**Service:** rider-app
**Auth:** TokenAuth
**Query params:** `isImages :: Maybe Bool`
**Response body:** `DriverProfileResponse { response :: Maybe DriverProfileRes }`
**Business logic summary:** Gets driver profile from BPP internal API. Null for non-value-add NPs.

---

## 8. Payment Domain

---

### [POST] /payment/{rideId}/createOrder
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Payment.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Payment.hs`
**Auth:** TokenAuth

**Response body:** `CreateOrderResp` (Juspay SDK payload with payment_links, clientAuthToken)

**Business logic summary:** Creates Juspay payment order for a COMPLETED ride's totalFare. Optionally creates wallet in external service.

**DB tables touched:** `ride`, `person`, `payment_order`, `merchant_service_config`
**Side effects:** Juspay createOrder API; optional wallet creation

---

### [GET] /payment/{orderId}/status
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `PaymentStatusResp` with fields: `orderId`, `status`, `bankErrorMessage`, `bankErrorCode`, `refunds`, `payerVpa`, `card`, `txnId`, `effectAmount`, `offers`, `amount`, `validTill`
**Business logic summary:** Syncs payment order status with gateway. Handles grouped orders.
**Side effects:** Payment gateway status API; Redis lock

---

### [GET] /payment?orderId={orderId}
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `PaymentOrderAPIEntity` with decrypted clientAuthToken

---

### [POST] /payment/wallet/recharge
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `WalletRechargeReq { pointsAmount :: Int }`
**Response body:** `APISuccess`

---

### [GET] /payment/wallet/balance
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `WalletBalanceData` (point balance, cash balance, expired balance)

---

### [GET] /payment/methods
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `PaymentMethodsResponse { list :: CustomerCardListResp, defaultPaymentMethodId :: Maybe PaymentMethodId }`
**Business logic summary:** Gets saved cards from Stripe. Auto-updates default if current default no longer exists.

---

### [POST] /payment/methods/{paymentMethodId}/makeDefault
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `APISuccess`

---

### [GET] /payment/intent/setup
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `SetupIntentResponse { setupIntentClientSecret, customerId, ephemeralKey }`
**Business logic summary:** Creates Stripe ephemeral key + setup intent for adding new cards.

---

### [DELETE] /payment/methods/{paymentMethodId}/delete
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `APISuccess`
**Side effects:** Stripe deleteCard API

---

### [POST] /payment/{rideId}/addTip
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `AddTipRequest { amount :: PriceAPIEntity }`
**Response body:** `APISuccess`
**Business logic summary:** Adds tip to completed online-payment ride. Creates separate payment intent or updates existing one. Calls BPP internal API to populate tip on driver side.
**Side effects:** Redis lock; Stripe API; BPP internal API

---

### [POST] /payment/{rideId}/refundRequest/create
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `RefundRequestReq { code, description, evidence, fileType, requestedAmount }`
**Response body:** `APISuccess`
**Business logic summary:** Creates refund request for completed/cancelled online-payment ride. Validates amount. Uploads evidence to S3.

---

### [POST] /s2s/payment/paytm/edc/callback
**Service:** rider-app
**Auth:** NoAuth (S2S callback)
**Request body:** `PaytmEdcCallbackReq` (checksum + resultInfo + merchantReferenceNo)
**Response body:** `AckResponse`
**Business logic summary:** Paytm EDC terminal payment callback. For CHARGED RideBooking orders, triggers BECKN confirm to BPP.

---

## 9. Profile Domain

---

### [GET] /profile
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Profile.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Profile.hs`
**Auth:** TokenAuth

**Response body:** `ProfileRes` -- 40+ fields including: `id`, `firstName`, `middleName`, `lastName`, `email`, `maskedMobileNumber`, `isBlocked`, `hasTakenRide`, `hasTakenValidRide`, `businessProfileVerified`, `referralCode`, `language`, `gender`, `hasCompletedSafetySetup`, `frontendConfigHash`, `cancellationRate`, `isMultimodalRider`, `customerTags`, `paymentMode`, `blockedUntil`

**Query params:** `toss`, `tenant`, `context`, `includeProfileImage`

**Business logic summary:** Full rider profile. Decrypts PII, computes cancellation rate, checks disability, updates version fields, loads frontend configs from CAC, checks multimodal eligibility. Forks: unblock check, cancellation rate nudge.

**DB tables touched:** `person`, `person_stats`, `person_disability`, `safety_settings`, `client_person_info`, `rider_config`, `payout_config`, `integrated_bpp_config`

---

### [POST] /profile
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `UpdateProfileReq` -- 25+ optional fields: `firstName`, `lastName`, `email`, `language`, `gender`, `disability`, `hasDisability`, `deviceId`, `profilePicture`, `mbMobileNumber`, `paymentMode`, etc.
**Response body:** `APISuccess`
**Business logic summary:** Updates personal info, applies referral code, handles disability upsert.
**Side effects:** Kafka marketing params event (forked)

---

### [PUT] /profile/updateEmergencySettings
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `UpdateEmergencySettingsReq` -- 15 fields: `shareEmergencyContacts`, `nightSafetyChecks`, `hasCompletedSafetySetup`, `autoCallDefaultContact`, `enablePostRideSafetyCheck`, `shakeToActivate`, etc.
**Response body:** `APISuccess`

---

### [GET] /profile/getEmergencySettings
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `EmergencySettingsRes` -- all safety settings + emergency contact list + police support info

---

### [POST] /profile/defaultEmergencyNumbers
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `UpdateProfileDefaultEmergencyNumbersReq { defaultEmergencyNumbers :: [PersonDefaultEmergencyNumber] }` where each has: `name`, `mobileCountryCode`, `mobileNumber`, `priority`, `enableForFollowing`, `shareTripWithEmergencyContactOption`
**Response body:** `APISuccess`
**Business logic summary:** Replaces all emergency contacts. Encrypts numbers, links to registered persons. Sends SMS + push to newly added contacts.

---

### [GET] /profile/defaultEmergencyNumbers
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `GetProfileDefaultEmergencyNumbersResp { defaultEmergencyNumbers :: [PersonDefaultEmergencyNumberAPIEntity] }`

---

### [POST] /profile/updateAuthData/triggerOTP
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `TriggerUpdateAuthOTPReq { identifier, email, mobileNumber, mobileCountryCode }`
**Response body:** `APISuccess`
**Business logic summary:** Sends OTP for changing mobile number or email. Validates not already registered.

---

### [POST] /profile/updateAuthData/verifyOTP
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `VerifyUpdateAuthOTPReq { identifier, otp }`
**Response body:** `APISuccess`
**Business logic summary:** Verifies OTP and updates person's contact info.

---

## 10. Maps Domain

---

### [POST] /maps/autoComplete
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/src/API/UI/Maps.hs`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Maps.hs`
**Auth:** TokenAuth
**Request body:** `AutoCompleteReq { input :: Text, sessionToken, location :: Text, radius :: Integer, language :: Language, strictbounds, origin, autoCompleteType }`
**Response body:** `AutoCompleteResp` (list of place predictions)
**Business logic summary:** Delegates to Google Maps. Forks analytics: appends input to Redis session data, triggers Kafka autocomplete event.
**Side effects:** Google Maps AutoComplete API; Redis analytics (300s TTL); Kafka event

---

### [POST] /maps/getPlaceDetails
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `GetPlaceDetailsReq { placeId, sessionToken, language }`
**Response body:** `GetPlaceDetailsResp`
**Side effects:** Google Maps Place Details API

---

### [POST] /maps/getPlaceName
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `GetPlaceNameReq { getBy :: ByLatLong LatLong | ByPlaceId Text, sessionToken, language }`
**Response body:** `[PlaceName]` with `formattedAddress`, `addressComponents`, `location`, `placeId`
**Business logic summary:** Caching layer over maps API. Checks PlaceNameCache by geohash or placeId. On miss, calls external API and caches result.
**DB tables touched:** `place_name_cache`, `merchant`, `merchant_service_config`, `rider_config`
**Side effects:** Google Maps Geocoding API on cache miss; DB cache insert

---

## 11. SOS Domain

---

### [GET] /sos/getDetails/{rideId}
**Service:** rider-app
**File:** `app/rider-platform/rider-app/Main/spec/API/sos.yaml`
**Handler:** `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Sos.hs`
**Auth:** TokenAuth
**Response body:** `SosDetailsRes { sos :: Maybe Sos }` where Sos has: `id`, `flow` (Police|CustomerCare|EmergencyContact|SafetyFlow|CSAlertSosTicket|AudioRecording), `status` (Resolved|NotResolved|Pending|MockPending|MockResolved), `sosState` (LiveTracking|SosActive), `entityType` (Ride|NonRide), `personId`, `rideId`, `ticketId`, `mediaFiles`, `trackingExpiresAt`
**Business logic summary:** Looks up SOS by ride ID from Redis cache. Falls back to mock drill entity if real SOS doesn't exist.

---

### [POST] /sos/create
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `SosReq { flow :: SosType, rideId :: Maybe (Id Ride), isRideEnded, sendPNOnPostRideSOS, notifyAllContacts, customerLocation :: Maybe LatLong }`
**Response body:** `SosRes { sosId :: Id Sos }`
**Business logic summary:** Creates SOS alert. For ride-based: creates Kapture ticket, enables share-ride for emergency contacts, sends push + SMS. For non-ride: creates standalone SOS with 8h tracking expiry.
**Side effects:** Redis SOS cache; Kapture ticket; push notifications; SMS; follow-ride counter updates

---

### [POST] /sos/{sosId}/status
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `SosUpdateReq { status :: SosStatus, comment :: Maybe Text }`
**Response body:** `APISuccess`

---

### [POST] /sos/markRideAsSafe/{sosId}
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `MarkAsSafeReq { isMock, isRideEnded, isEndLiveTracking, contacts :: Maybe [Text] }`
**Response body:** `APISuccess`
**Business logic summary:** Marks SOS resolved. For mock drills: updates Redis. For real: updates Kapture ticket, transitions to LiveTracking if not ending tracking.

---

### [POST] /sos/callPolice
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `CallPoliceAPI { rideId :: Id Ride }`
**Response body:** `APISuccess`
**Business logic summary:** Initiates police incident report via ERSS API. Schedules follow-up CallPoliceApi scheduler job. Sets ride safety status to PoliceMonitoring.
**Side effects:** ERSS incident report API; scheduler job; Redis rate-limit key

---

### [POST] /sos/{sosId}/upload
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** Multipart: `SOSVideoUploadReq { payload :: FilePath, fileType :: FileType }`
**Response body:** `AddSosVideoRes { fileUrl :: Text }`
**Business logic summary:** Uploads SOS audio/video to S3. Appends media file to SOS record. Creates Kapture ticket for ride-based SOS.

---

### [POST] /sos/startTracking
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `StartTrackingReq { contacts :: [Text], durationInMinutes :: Int, customerLocation, sosId }`
**Response body:** `StartTrackingRes { sosId, trackingUrl }`
**Business logic summary:** Starts non-ride live location tracking (1-1440 min). Creates or extends SOS, builds tracking URL, notifies contacts.

---

### [GET] /sos/{sosId}/tracking
**Service:** rider-app
**Auth:** NoAuth (public tracking endpoint)
**Response body:** `SosTrackingRes { currentLocation, sosState, status }`
**Business logic summary:** Public endpoint for emergency contacts to track SOS location in real time. Rate-limited per SOS ID.

---

### [POST] /sos/{sosId}/updateLocation
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `SosLocationUpdateReq { lat, lon, accuracy }`
**Response body:** `APISuccess`
**Business logic summary:** Updates real-time SOS location in Redis.

---

## 12. Support & Issue Domain

---

### [POST] /support/sendIssue
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `SendIssueReq { contactEmail, issue :: Issue { reason, description }, rideBookingId, nightSafety }`
**Response body:** `APISuccess`
**Business logic summary:** Deprecated. Used for delete account requests. Creates Issue record + Kapture ticket.

---

### [POST] /support/callbackRequest
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `APISuccess`
**Business logic summary:** Creates callback request for customer support.

---

### [POST] /support/safetyCheckSupport
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `SafetyCheckSupportReq { bookingId, isSafe :: Bool, description }`
**Response body:** `APISuccess`
**Business logic summary:** Handles safety check response. If unsafe: creates "Code Red" Kapture ticket, sets ride to CSAlerted. If safe: marks ride as Safe.

---

### [POST] /issue
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `IssueReportReq { rideId, mediaFiles, optionId, categoryId, description, chats, createTicket, ticketBookingId }`
**Response body:** `IssueReportRes { issueReportId, issueReportShortId, messages }`
**Business logic summary:** Creates issue report. If IGM enabled and non-value-add NP, creates BECKN IGM issue and sends to BPP.

---

### [GET] /issue/list
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `IssueReportListRes { issues :: [IssueReportListItem] }` -- each with issueReportId, status (OPEN|PENDING_INTERNAL|PENDING_EXTERNAL|RESOLVED|CLOSED|REOPENED), category, rideId, createdAt

---

### [GET] /issue/category
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `IssueCategoryListRes { categories :: [IssueCategoryRes] }` -- each with label, logoUrl, categoryType (Category|FAQ), isRideRequired, maxAllowedRideAge

---

### [GET] /issue/{issueId}/info
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `IssueInfoRes` -- full issue with chat history, media files, status, options

---

### [PUT] /issue/{issueId}/updateStatus
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `IssueStatusUpdateReq { status :: IssueStatus, customerResponse :: Maybe CustomerResponse, customerRating :: Maybe CustomerRating }`
**Business logic summary:** Updates issue status. On CLOSED/RESOLVED: also resolves associated IGM issue via BECKN.

---

## 13. FollowRide Domain

---

### [GET] /follow/ride
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `[Followers]` -- each with `name`, `bookingId`, `mobileNumber`, `priority`, `personId`
**Business logic summary:** Returns people whose ride the current user is following (via emergency contact sharing).

---

### [POST] /share/ride
**Service:** rider-app
**Auth:** TokenAuth
**Request body:** `ShareRideReq { emergencyContactNumbers :: [Text] }`
**Response body:** `APISuccess`
**Business logic summary:** Shares current ride with specified emergency contacts. Updates Redis follow-ride list, sends SHARE_RIDE push notifications.

---

### [GET] /followRide/ECStatus/{rideId}
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `EmergencyContactsStatusRes { details :: [ContactsDetail { personId, updateTime }] }`
**Business logic summary:** Returns which emergency contacts have viewed the SOS tracking.

---

### [GET] /followRide/{rideId}/customerDetails
**Service:** rider-app
**Auth:** TokenAuth
**Response body:** `FollowRideCustomerDetailsRes { bookingId, customerName, customerPhone }`

---

# DYNAMIC-OFFER-DRIVER-APP SERVICE (port 8016)

## 14. Driver Registration Domain

---

### [POST] /auth
**Service:** dynamic-offer-driver-app
**File:** `app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/Registration.hs`
**Handler:** `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Registration.hs`
**Auth:** NoAuth

**Request body:**
```haskell
data AuthReq = AuthReq
  { mobileNumber            :: Maybe Text
  , mobileCountryCode       :: Maybe Text
  , merchantId              :: Text
  , merchantOperatingCity   :: Maybe Context.City
  , email                   :: Maybe Text
  , name                    :: Maybe Text
  , identifierType          :: Maybe IdentifierType
  , registrationLat         :: Maybe Double
  , registrationLon         :: Maybe Double
  , otpChannel              :: Maybe OTPChannel
  }
```

**Response body:** `AuthRes { authId, attempts, token :: Maybe Text, person :: Maybe PersonAPIEntity }`

**Business logic summary:** Validates mobile/email, looks up or creates driver Person + DriverInformation + DriverStats. Generates OTP, creates RegistrationToken, sends OTP via SMS/WhatsApp/email.

**DB tables touched:** `person`, `driver_information`, `driver_stats`, `registration_token`, `driver_license`, `merchant`, `merchant_operating_city`, `transporter_config`
**Side effects:** SMS/WhatsApp/email OTP; Redis rate limit + OTP channel cache

---

### [POST] /auth/signature
**Service:** dynamic-offer-driver-app
**Auth:** SignatureAuth
**Business logic summary:** Same as /auth but verifies immediately via SDK signature. Returns token directly.

---

### [POST] /auth/{authId}/verify
**Service:** dynamic-offer-driver-app
**Auth:** NoAuth

**Request body:** `AuthVerifyReq { otp :: Text, deviceToken :: FCMRecipientToken, whatsappNotificationEnroll :: Maybe OptApiMethods }`
**Response body:** `AuthVerifyRes { token :: Text, person :: PersonAPIEntity }` where PersonAPIEntity has: `id`, `firstName`, `middleName`, `lastName`, `maskedMobileNumber`, `maskedDeviceToken`, `whatsappNotificationEnrollStatus`, `role`, `language`

**Business logic summary:** Validates OTP, marks token verified, generates driver referral code (forked). For new drivers, fires NEW_SIGNUP Firebase marketing event.

**DB tables touched:** `registration_token`, `person`, `driver_referral`
**Side effects:** Redis rate limit + token cache; Firebase marketing notification; WhatsApp opt-in

---

### [POST] /auth/otp/{authId}/resend
**Service:** dynamic-offer-driver-app
**Auth:** NoAuth
**Business logic summary:** Resends OTP, decrements attempt counter.

---

### [POST] /auth/logout
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Business logic summary:** Clears tokens, device token. Sets driver mode to OFFLINE.
**DB tables touched:** `person`, `registration_token`, `driver_information`

---

## 15. Driver Ride Domain

---

### [POST] /driver/otpRide/start
**Service:** dynamic-offer-driver-app
**File:** `app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/Ride.hs`
**Handler:** `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride.hs`
**Auth:** TokenAuth

**Request body:** `OTPRideReq { specialZoneOtpCode :: Text, point :: LatLong, odometer :: Maybe OdometerReading }`
**Response body:** `DriverRideRes` (50+ fields -- see below)

**Business logic summary:** Combined create + start for special zone OTP rides. Finds booking by OTP, validates driver subscription, checks GPS within restriction radius of pickup, creates ride, assigns driver, starts ride. Schedules payout if enabled.

**DB tables touched:** `booking`, `driver_information`, `transporter_config`, `ride`, `ride_details`, `vehicle`, `fleet_driver_association`, `person`, `location`, `location_mapping`
**Side effects:** BECKN on_confirm callback; location tracking service init; Redis keys; Kafka ride event; FCM notification; scheduler payout job

---

### [GET] /driver/ride/list
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Query params:** `limit`, `offset`, `onlyActive`, `status` (UPCOMING|NEW|INPROGRESS|COMPLETED|CANCELLED), `day`, `numOfDay`
**Response body:** `DriverRideListRes { list :: [DriverRideRes] }`

`DriverRideRes` key fields: `id`, `shortRideId`, `status`, `fromLocation`, `toLocation`, `driverName`, `driverNumber`, `vehicleNumber`, `vehicleModel`, `rideOtp`, `endOtp`, `rideStartTime`, `rideEndTime`, `computedFareWithCurrency`, `chargeableDistanceWithUnit`, `rideRating`, `riderName`, `tripCategory`, `billingCategory`, `tipAmount`, `tollCharges`, `isPetRide`, `isInsured`, `paymentInstrument`

---

### [POST] /driver/ride/{rideId}/arrived/pickup
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `LatLong`
**Response body:** `APISuccess`
**Business logic summary:** Marks driver arrived at pickup. Validates distance within arrivedPickupThreshold. Sends BECKN on_update (driver arrival) to BAP.

---

### [POST] /driver/ride/{rideId}/start
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth

**Request body:** `StartRideReq { rideOtp :: Text, point :: LatLong, odometer :: Maybe OdometerReading }`
**Response body:** `APISuccess`

**Business logic summary:** Validates OTP (unless otpLess), validates ride status NEW/UPCOMING. Records trip start, initializes distance calculation, generates end-ride OTP if needed. Notifies BAP, sends push to driver, schedules payout and notification reminders.

**DB tables touched:** `ride`, `booking`, `person`, `driver_information`, `transporter_config`, `ride_related_notification_config`, `payout_request`
**Side effects:** Redis start key (60s TTL); location tracking init; BECKN on_update; FCM; Kafka event; scheduler jobs

---

### [POST] /driver/ride/{rideId}/end
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth

**Request body:**
```haskell
data EndRideReq = EndRideReq
  { endRideOtp                               :: Maybe Text
  , point                                    :: LatLong
  , uiDistanceCalculationWithAccuracy        :: Maybe Int
  , uiDistanceCalculationWithoutAccuracy     :: Maybe Int
  , odometer                                 :: Maybe OdometerReading
  , driverGpsTurnedOff                       :: Maybe Bool
  }
```

**Response body:** `EndRideResp { result :: Text, homeLocationReached :: Maybe Bool, driverRideRes :: Maybe DriverRideRes }`

**Business logic summary:** Most complex endpoint. Validates INPROGRESS + end-OTP. Final distance calculation from GPS interpolation. Fare calculation (distance/time/tolls/parking/discounts). Updates ride with final fare + COMPLETED. Handles go-home deactivation. Sends BECKN on_update to BAP. Awards driver coins. Triggers Yudhishthira rule evaluation.

**DB tables touched:** `ride`, `booking`, `person`, `driver_information`, `transporter_config`, `fare_parameters`, `fare_policy`, `driver_stats`, `driver_go_home_request`, `go_home_config`, `ride_details`, `stop_information`, `merchant_payment_method`, `rider_details`
**Side effects:** BECKN on_update; Redis interpolated points + toll cache; Kafka events; location tracking final distance; FCM; coins reward; scheduler stats update; ClickHouse toll analysis

---

### [POST] /driver/ride/{rideId}/cancel
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth

**Request body:** `CancelRideReq { reasonCode :: CancellationReasonCode, additionalInfo :: Maybe Text, doCancellationRateBasedBlocking :: Maybe Bool }`
**Response body:** `CancelRideResp { result :: Text, goHomeCancellationCount :: Maybe Int, isGoHomeDisabled :: Maybe Bool }`

**Business logic summary:** Validates NEW/UPCOMING. Handles go-home cancellation counter. Creates BookingCancellationReason. Acquires shared cancellation lock. Triggers reallocation or sends on_cancel to BAP.

**DB tables touched:** `ride`, `booking`, `person`, `driver_information`, `booking_cancellation_reason`, `driver_go_home_request`, `go_home_config`
**Side effects:** BECKN on_cancel/on_update; Redis cancellation lock; Kafka event; driver coins deduction

---

### [POST] /driver/ride/{rideId}/arrived/stop
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `LatLong`
**Response body:** `APISuccess`
**Business logic summary:** For multi-stop rental rides. Validates distance to stop. Sends BECKN stop-arrival update.

---

### [POST] /driver/ride/{rideId}/arrived/destination
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `LatLong`
**Response body:** `APISuccess`
**Business logic summary:** Marks driver arrived at drop-off. Records destinationReachedAt. Sends BECKN update to BAP.

---

### [POST] /driver/ride/{rideId}/uploadOdometer
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** Multipart: `file` + `fileType`
**Response body:** `UploadOdometerResp { fileId :: Id MediaFile }`
**Business logic summary:** Uploads odometer image to S3.

---

### [POST] /driver/ride/{rideId}/uploadDeliveryImage
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** Multipart: `file` (image only)
**Response body:** `APISuccess`
**Business logic summary:** Uploads delivery verification image. Appends to ride's deliveryFileIds. Notifies BAP.

---

## 16. Driver Profile Domain

---

### [GET] /driver/profile
**Service:** dynamic-offer-driver-app
**File:** `app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/Driver.hs`
**Handler:** `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Driver.hs`
**Auth:** TokenAuth

**Response body:** `DriverInformationRes` -- 100+ fields including: `id`, `firstName`, `operatingCity`, `numberOfRides`, `mobileNumber`, `linkedVehicle`, `rating`, `active`, `onRide`, `verified`, `enabled`, `blocked`, `subscribed`, `paymentPending`, `referralCode`, `mode`, `autoPayStatus`, `gender`, `isGoHomeEnabled`, `driverGoHomeInfo`, `freeTrialDaysLeft`, `currentDues`, `manualDues`, `isPayoutEnabled`, `cancellationRateInWindow`, `safetyScore`, `qrUrl`, `driverTags`, `fleetOwnerId`, `operatorId`

**Business logic summary:** Primary "get driver state" endpoint. Aggregates person, driver info, vehicle, stats, referrals, subscription status, go-home info, fleet associations, bank details, payout status, QR code.

**DB tables touched:** `person`, `driver_information`, `vehicle`, `driver_stats`, `driver_referral`, `driver_home_location`, `driver_go_home_request`, `go_home_config`, `driver_plan`, `subscription_config`, `driver_fee`, `merchant`, `merchant_operating_city`, `transporter_config`, `fleet_driver_association`, `fleet_owner_information`, `driver_bank_account`, `vehicle_registration_certificate`, `payout_config`

---

### [POST] /driver/profile
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `UpdateDriverReq` -- 25+ optional fields: `firstName`, `language`, `canDowngradeToSedan`, `canSwitchToRental`, `isPetModeEnabled`, `gender`, `vehicleName`, `tripDistanceMaxThreshold`, `maxPickupRadius`, `isSilentModeEnabled`, `rideRequestVolume`, etc.
**Response body:** `DriverInformationRes`

---

### [GET] /driver/profile/stats
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Query params:** `day :: Day` (mandatory)
**Response body:** `DriverStatsRes { totalRidesOfDay, totalEarningsOfDay, bonusEarning, coinBalance, totalValidRidesOfDay, tipsEarning }`

---

### [POST] /driver/setActivity
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Query params:** `active :: Bool` (mandatory), `mode :: DriverMode` (ONLINE|OFFLINE|SILENT)
**Response body:** `APISuccess`
**Business logic summary:** Sets driver active/inactive status and mode.

---

### [GET] /driver/nearbyRideRequest
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Query params:** `searchTryId :: Maybe (Id SearchTry)`
**Response body:** `GetNearbySearchRequestsRes { searchRequestsForDriver :: [SearchRequestForDriverAPIEntity] }`
**Business logic summary:** Returns pending ride requests near the driver.

---

### [POST] /driver/searchRequest/quote/respond
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `DriverRespondReq { offeredFareWithCurrency, searchTryId, response :: ACCEPT | REJECT, notificationSource, renderedAt, respondedAt }`
**Response body:** `APISuccess`
**Business logic summary:** Driver accepts/rejects ride request. On accept: creates driver quote or directly assigns ride. Sends BECKN on_select to BAP.

---

### [POST] /driver/goHome/activate
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Query params:** `homeLocationId` (mandatory), `currentLocation` (mandatory)
**Response body:** `APISuccess`

### [POST] /driver/goHome/deactivate
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Response body:** `APISuccess`

### [POST] /driver/goHome/add
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `AddHomeLocationReq { position :: LatLong, address :: Text, tag :: Text }`
**Response body:** `APISuccess`

---

## 17. Driver Payment Domain

---

### [POST] /payment/{invoiceId}/createOrder
**Service:** dynamic-offer-driver-app
**File:** `app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/Payment.hs`
**Handler:** `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Payment.hs`
**Auth:** TokenAuth

**Response body:** `CreateOrderResp` (Juspay SDK payload)

**Business logic summary:** Creates Juspay order for driver fee invoices. Aggregates all invoices, determines subscription service, handles vendor split if enabled.

**DB tables touched:** `invoice`, `driver_fee`, `subscription_config`, `vendor_fee`, `payment_order`
**Side effects:** Juspay createOrder API

---

### [GET] /payment/{orderId}/status
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Response body:** `PaymentStatusResp` (PaymentStatus | MandatePaymentStatus | PDNNotificationStatusResp)

**Business logic summary:** Core payment status check. Optimizes for terminal CHARGED status. On CHARGED: marks driver fees CLEARED, updates invoice SUCCESS, updates subscription status, sends FCM. Processes mandate lifecycle (ACTIVE/REVOKED/PAUSED/EXPIRED). Processes autopay notifications (SUCCESS -> schedule execution).

**DB tables touched:** `payment_order`, `invoice`, `driver_fee`, `subscription_purchase`, `notification`, `driver_information`, `driver_plan`, `mandate`, `transporter_config`, `subscription_config`, `person`
**Side effects:** Juspay orderStatus API; Redis processing locks + notification dedup; FCM notifications; Kafka autoPayStatusChange; scheduler ExpireSubscriptionPurchase

---

### [GET] /payment?orderId={orderId}
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Response body:** `PaymentOrderAPIEntity` with decrypted clientAuthToken

---

### [GET] /payment/{notificationId}/notification
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Response body:** `NotificationStatusResp`
**Business logic summary:** Checks Juspay mandate notification acknowledgment. On SUCCESS: marks invoice active, schedules execution. On FAILURE: retries or reverts to manual.

---

## 18. Driver Plan (Subscription) Domain

---

### [GET] /plan/list
**Service:** dynamic-offer-driver-app
**File:** `app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/Plan.hs`
**Handler:** `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Plan.hs`
**Auth:** TokenAuth

**Query params:** `limit`, `offset`, `vehicleVariant`, `serviceName` (YATRI_SUBSCRIPTION|PREPAID_SUBSCRIPTION|YATRI_RENTAL|DASHCAM_RENTAL)

**Response body:** `PlanListAPIRes { list :: [PlanEntity], subscriptionStartTime, isLocalized }` where PlanEntity has: `id`, `name`, `description`, `planFareBreakup`, `freeRideCount`, `frequency`, `offers`, `paymentMode`, `totalPlanCreditLimit`, `currentDues`, `autopayDues`, `validInDays`, `dues :: [DriverDuesEntity]`, `coinEntity`, `bankErrors`

**Business logic summary:** Fetches available subscription plans for the driver's service and operating city. Enriches with current dues, applied offers, coin discounts, bank errors.

**DB tables touched:** `person`, `driver_information`, `driver_plan`, `subscription_purchase`, `subscription_config`, `plan`, `plan_translation`, `transporter_config`, `driver_fee`, `invoice`, `vendor_fee`, `driver_stats`

---

### [GET] /plan/currentPlan
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Query params:** `serviceName`
**Response body:** `CurrentPlanRes { currentPlanDetails :: Maybe PlanEntity, mandateDetails :: Maybe MandateDetailsEntity, orderId, lastPaymentType, autoPayStatus, subscribed, planRegistrationDate, latestAutopayPaymentDate, latestManualPaymentDate, validity, subscriptionPurchaseStatus, payoutVpa, askForPlanSwitchByCity, askForPlanSwitchByVehicle }`

---

### [POST] /plan/{planId}/subscribe
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Query params:** `serviceName`
**Response body:** `PlanSubscribeRes { orderId, orderResp }`

**Business logic summary:** For PREPAID: creates subscription purchase + Juspay order. For others: handles autopay status (resume if SUSPENDED, revoke old mandate if PAUSED_PSP). Creates mandate-setup invoice and payment order with registration fee + outstanding dues.

**DB tables touched:** `person`, `driver_information`, `driver_plan`, `subscription_config`, `plan`, `driver_fee`, `invoice`, `payment_order`, `subscription_purchase`
**Side effects:** Juspay createOrder + mandateRevoke; Redis locks; FCM/WhatsApp subscription link

---

### [PUT] /plan/suspend
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Response body:** `APISuccess`
**Business logic summary:** Deactivates mandate, switches to MANUAL payment, converts autopay invoices to manual overdue.

---

### [PUT] /plan/resume
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Response body:** `APISuccess`
**Business logic summary:** Reactivates mandate, switches back to AUTOPAY.

---

### [PUT] /plan/{planId}/select
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Response body:** `APISuccess`
**Business logic summary:** Switches driver to a different plan of the same payment mode.

---

## 19. Driver Maps Domain

---

### [POST] /maps/autoComplete
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `AutoCompleteReq { input, sessionToken, location, radius, language, strictbounds, origin }`
**Response body:** `AutoCompleteResp`
**Side effects:** External Maps API (Google Places)

---

### [POST] /maps/getPlaceName
**Service:** dynamic-offer-driver-app
**Auth:** TokenAuth
**Request body:** `GetPlaceNameReq { getBy :: ByLatLong LatLong | ByPlaceId Text }`
**Response body:** `[PlaceName]`
**Business logic summary:** Caching layer over maps provider. Checks PlaceNameCache by geohash/placeId. On miss: calls external API, caches result.
**DB tables touched:** `place_name_cache`, `merchant`, `transporter_config`

---

## 20. Driver Onboarding Domain (NammaDSL-generated)

All use TokenAuth PROVIDER_TYPE.

---

### [GET] /onboarding/configs
**Query params:** `onlyVehicle :: Bool`, `makeSelfieAadhaarPanMandatory :: Bool`
**Response:** `DocumentVerificationConfigList`

### [GET] /driver/rateCard
**Query params:** `vehicleServiceTier`, `tripCategory`, `distance`, `durationInMin`
**Response:** `[RateCardResp]`

### [GET] /driver/vehicleServiceTiers
**Response:** `DriverVehicleServiceTiers`

### [POST] /driver/updateServiceTiers
**Request:** `DriverVehicleServiceTiers`
**Response:** `APISuccess`

### [POST] /driver/register/ssn
**Request:** `SSNReq`
**Response:** `APISuccess`

### [POST] /driver/verify/bankAccount
**Request:** `VerifyBankAccReq`
**Response:** `VerifyAsyncResp`

### [POST] /driver/backgroundVerification
**Response:** `APISuccess`

### [POST] /driver/register/pancard
**Request:** `DriverPanReq`
**Response:** `APISuccess`

### [GET] /driver/register/bankAccount/link
**Query params:** `paymentMode`
**Response:** `BankAccountLinkResp`

### [GET] /driver/register/bankAccount/status
**Response:** `BankAccountResp`

### [POST] /driver/register/aadhaarCard
**Request:** `AadhaarCardReq`
**Response:** `APISuccess`

### [POST] /driver/register/commonDocument
**Request:** `CommonDocumentReq`
**Response:** `APISuccess`

### [POST] /driver/digilocker/initiate
**Request:** `DigiLockerInitiateReq`
**Response:** `DigiLockerInitiateResp`

### [POST] /driver/digilocker/pullDocuments
**Request:** `PullDocumentReq`
**Response:** `APISuccess`

---

## 21. Driver Wallet Domain (NammaDSL-generated)

---

### [GET] /wallet/transactions
**Auth:** TokenAuth PROVIDER_TYPE
**Query params:** `fromDate :: Maybe UTCTime`, `toDate :: Maybe UTCTime`
**Response:** `WalletSummaryResponse { currentBalance, redeemableBalance, nonRedeemableBalance, additions :: WalletItemGroup, deductions :: WalletItemGroup }`

### [POST] /wallet/payout
**Auth:** TokenAuth PROVIDER_TYPE
**Response:** `APISuccess`

### [POST] /wallet/topup
**Auth:** TokenAuth PROVIDER_TYPE
**Request:** `TopUpRequest { amount :: HighPrecMoney }`
**Response:** `PlanSubscribeRes`

### [GET] /wallet/payoutHistory
**Auth:** TokenAuth PROVIDER_TYPE
**Query params:** `from`, `to`, `status`, `limit`, `offset`
**Response:** `PayoutHistoryResponse { totalPaidOut, lastPayout, totalPending, items :: [PayoutHistoryItem] }`

---

## 22. Driver LMS Domain (NammaDSL-generated)

---

### [GET] /lms/listAllModules
**Query params:** `variant`, `language`, `limit`, `offset`, `moduleSection`
**Response:** `LmsGetModuleRes`

### [GET] /lms/{moduleId}/listAllVideos
**Query params:** `language`
**Response:** `LmsGetVideosRes`

### [GET] /lms/{moduleId}/listAllQuiz
**Query params:** `language`
**Response:** `LmsGetQuizRes`

### [GET] /lms/{moduleId}/getCertificate
**Response:** `LmsCertificateRes`

### [POST] /lms/markVideoAsStarted
**Request:** `VideoUpdateAPIReq`
**Response:** `APISuccess`

### [POST] /lms/markVideoAsCompleted
**Request:** `VideoUpdateAPIReq`
**Response:** `APISuccess`

### [POST] /lms/question/confirm
**Request:** `QuestionConfirmReq`
**Response:** `QuestionConfirmRes`

---

## 23. Additional Driver Endpoints (NammaDSL-generated)

---

### [POST] /driver/call/feedback
**Request:** `CallFeedbackReq`
**Response:** `APISuccess`

### [GET] /driver/demandHotspots
**Response:** `GetDemandHotspotsResp`

### [GET] /driver/sdkToken
**Query params:** `expiry :: Int`, `service` (mandatory)
**Response:** `GetTokenRes`

### [POST] /driver/profile/updateAuthData/triggerOTP
**Request:** `TriggerUpdateAuthOTPReq`
**Response:** `APISuccess`

### [POST] /driver/profile/updateAuthData/verifyOTP
**Request:** `VerifyUpdateAuthOTPReq`
**Response:** `APISuccess`

### [GET] /vehicleMakes
**Response:** `VehicleMakesResp`

### [POST] /vehicleModels
**Request:** `VehicleModelsReq`
**Response:** `VehicleModelsResp`

### [GET] /specialLocation/list
**Query params:** `isOrigin :: Bool`
**Response:** `[SpecialLocationFull]`

### [POST] /social/login
**Auth:** NoAuth
**Request:** `SocialLoginReq`
**Response:** `SocialLoginRes`

### [POST] /meterRide/{rideId}/addDestination
**Request:** `MeterRideAddDestinationReq`
**Response:** `MeterRideAddDestinationResp`

### [GET] /insurance/{referenceId}
**Response:** `InsuranceAPIEntity`

### [GET] /invoice
**Query params:** `fromDate`, `toDate`, `rcNo`
**Response:** `[InvoiceRes]`

### [POST] /penalty/check
**Request:** `PenaltyCheckReq`
**Response:** `PenaltyCheckRes`

### [GET] /reels/getAllReelVideos
**Query params:** `reelsKey` (mandatory), `language`
**Response:** `ReelsResp`

### [GET] /cityConfigs
**Response:** `CityConfigs`

### [GET] /operation/getAllHubs
**Response:** `[OperationHub]`

### [POST] /operation/createRequest
**Request:** `DriverOperationHubRequest`
**Response:** `APISuccess`

### [GET] /calculateFare
**Query params:** `pickupLatLon`, `dropLatLon`, `distanceWeightage` (all mandatory)
**Response:** `FareResponse`

### [GET] /fleetOwner/list
**Query params:** `blocked`, `fleetType`, `onlyEnabled`, `limit`, `offset`
**Response:** `[FleetOwnerListItem]`

### [POST] /edit/result/{bookingUpdateRequestId}
**Request:** `EditBookingRespondAPIReq`
**Response:** `APISuccess`

---

## 24. WMB (Fleet/Bus Management) Endpoints

---

### [GET] /wmb/fleetBadges
**Query params:** `mbSearchString`, `mbBadgeType`, `limit` (mandatory), `offset` (mandatory)
**Response:** `[AvailableBadge]`

### [POST] /wmb/availableRoutes
**Request:** `AvailableRouteReq`
**Response:** `[AvailableRoute]`

### [POST] /wmb/qr/start
**Request:** `TripQrStartReq`
**Response:** `TripTransactionDetails`

### [GET] /wmb/trip/active
**Response:** `ActiveTripTransaction`

### [GET] /wmb/route/{routeCode}/details
**Response:** `RouteDetails`

### [GET] /wmb/trip/list
**Query params:** `limit`, `offset`, `status`
**Response:** `[TripTransactionDetails]`

### [POST] /wmb/trip/{tripTransactionId}/start
**Request:** `TripStartReq`
**Response:** `APISuccess`

### [POST] /wmb/trip/{tripTransactionId}/end
**Request:** `TripEndReq`
**Response:** `TripEndResp`

### [POST] /wmb/trip/{tripTransactionId}/request
**Request:** `RequestDetails`
**Response:** `AlertReqResp`

### [POST] /fleet/consent
**Response:** `APISuccess`

### [GET] /fleet/config
**Response:** `FleetConfig`

---

# SUMMARY

| Service | Domain | Endpoints |
|---|---|---|
| rider-app | Registration | 13 |
| rider-app | Search | 2 |
| rider-app | Select | 4 |
| rider-app | Booking & Confirm | 10 |
| rider-app | Ride | 5 |
| rider-app | Cancel | 3 |
| rider-app | Rating | 3 |
| rider-app | Payment | 16 |
| rider-app | Profile | 9 |
| rider-app | Maps | 3 |
| rider-app | SOS | 12 |
| rider-app | Support | 3 |
| rider-app | Issue | 11 |
| rider-app | FollowRide | 4 |
| driver-app | Registration | 6 |
| driver-app | Ride | 13 |
| driver-app | Driver Profile | 8 |
| driver-app | Payment | 4 |
| driver-app | Plan/Subscription | 6 |
| driver-app | Maps | 2 |
| driver-app | Onboarding | 14 |
| driver-app | Wallet | 4 |
| driver-app | LMS | 7 |
| driver-app | Misc UI APIs | 18 |
| driver-app | WMB/Fleet | 11 |
| **Total** | | **185** |
