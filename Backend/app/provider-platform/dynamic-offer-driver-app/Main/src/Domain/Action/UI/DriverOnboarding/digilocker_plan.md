# DigiLocker Verification Implementation Plan

## Overview

Implement DigilockerVerification table as a session-only table with JSON-based document status tracking. Enable DigiLocker verification for Namma Yatri merchant in Delhi city only, while other merchants continue using existing Idfy/HyperVerge flows.

## Key Design Decisions

1. **DigilockerVerification** = OAuth session table with JSON docStatus column
2. **Document data** = Stored in DriverLicense, PanCard, AadhaarCard tables (existing)
3. **No separate consent columns** = All document status in JSON
4. **No foreign keys** = Only `verifiedBy` field in document tables
5. **Merchant+City check** = Use TransporterConfig pattern (enable for Namma Yatri + Delhi only)

## 1. Create DigilockerVerification Table

### Database Schema

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/DriverOnboarding.yaml`

Add after IdfyVerification (around line 88):

```yaml
DigilockerVerification:
  tableName: digilocker_verification
  fields:
    id: Id DigilockerVerification
    driverId: Id Person

    # OAuth Session Fields
    stateId: Text  # NOT NULL - unique identifier for callback lookup
    codeVerifier: Text  # PKCE code verifier: Base64(random 24 bytes + timestamp*1000)
    codeChallenge: Text  # SHA256(codeVerifier) -> Base64URL
    codeMethod: Text  # Always "S256"
    authorizationCode: Maybe Text  # Code returned by DigiLocker in callback
    accessToken: Maybe Text  # OAuth access token from /token API
    accessTokenExpiresAt: Maybe UTCTime  # Token expiration timestamp
    scope: Maybe Text # The permission returned as string from /token API of digilocker

    # Document Status (JSON)
    docStatus: Text  # JSON: {"DriverLicense": {"status": "PENDING", "responseCode": "200", "responseDescription": "..."}, ...}

    # Session Status
    sessionStatus: Text  # PENDING | SUCCESS | FAILED | CONSENT_DENIED
    responseCode: Maybe Text  # Response code from /token API
    responseDescription: Maybe Text  # Response description from /token API

    # Raw Response (for debugging)
    tokenResponse: Maybe Text  # Raw /token API response

    # Metadata
    merchantId: Maybe (Id Merchant)
    merchantOperatingCityId: Maybe (Id MerchantOperatingCity)
    createdAt: UTCTime
    updatedAt: UTCTime

  constraints:
    id: PrimaryKey
    driverId: SecondaryKey
    stateId: SecondaryKey  # Unique, used for callback lookup

  queries:
    findById:
      kvFunction: findOneWithKV
      where: id
    findAllByDriverId:
      kvFunction: findAllWithKV
      where: driverId
    findByStateId:
      kvFunction: findOneWithKV
      where: stateId
    findLatestByDriverId:
      kvFunction: findAllWithOptionsKV
      where: driverId
    updateSessionStatus:
      kvFunction: updateWithKV
      params: [sessionStatus, responseCode, responseDescription]
      where: id
    updateAccessToken:
      kvFunction: updateWithKV
      params: [accessToken, accessTokenExpiresAt, authorizationCode]
      where: stateId
    updateDocStatus:
      kvFunction: updateWithKV
      params: [docStatus]
      where: id
    deleteByDriverId:
      kvFunction: deleteWithKV
      where: driverId
```

### JSON docStatus Structure

```json
{
  "DriverLicense": {
    "status": "PENDING | FAILED | PULL_REQUIRED | SUCCESS | CONSENT_DENIED",
    "responseCode": "200",
    "responseDescription": "OK"
  },
  "PanCard": {
    "status": "SUCCESS",
    "responseCode": "200",
    "responseDescription": "Document verified"
  },
  "AadhaarCard": {
    "status": "PULL_REQUIRED",
    "responseCode": null,
    "responseDescription": null
  }
}
```

## 2. Update Existing Document Tables

### Add verifiedBy Column Only

**DriverLicense, PanCard, AadhaarCard tables**:

```yaml
DriverLicense:
  fields:
    ...all existing fields...
    verifiedBy: Maybe VerificationMethod  # DIGILOCKER | IDFY | HYPERVERGE

PanCard:
  fields:
    ...all existing fields...
    verifiedBy: Maybe VerificationMethod

AadhaarCard:
  fields:
    ...all existing fields...
    verifiedBy: Maybe VerificationMethod
```

**Enum**:

```yaml
types:
  VerificationMethod:
    enum: "DIGILOCKER,IDFY,HYPERVERGE"
```

**Note**: No `digilockerSessionId` FK - correlation via driverId + timestamp if needed for audit.

## 3. Add DigiLocker Config to TransporterConfig

**Location**: Check TransporterConfig table definition

Add fields:

```yaml
TransporterConfig:
  fields:
    ...existing fields...
    digilockerEnabled: Maybe Bool  # Enable DigiLocker for this merchant+city
    digilockerClientId: Maybe Text
    digilockerClientSecret: Maybe (EncryptedHashedField e Text)
    digilockerRedirectUri: Maybe Text
```

**Configuration**: Enable only for Namma Yatri + Delhi

```sql
UPDATE transporter_config
SET digilocker_enabled = true,
    digilocker_client_id = 'CLIENT_ID',
    digilocker_client_secret = 'ENCRYPTED_SECRET',
    digilocker_redirect_uri = 'https://dobpp.nammayatri.in/driver/register/digilocker/callback'
WHERE merchant_id = 'NAMMA_YATRI_ID'
  AND merchant_operating_city_id = 'DELHI_CITY_ID';
```

## 4. Migration SQL

**Location**: `Backend/dev/migrations/dynamic-offer-driver-app/`

Create migration file (e.g., `XXXX-add-digilocker-verification.sql`):

```sql
-- Create digilocker_verification table
CREATE TABLE atlas_driver_offer_bpp.digilocker_verification (
    id character varying(36) NOT NULL PRIMARY KEY,
    driver_id character varying(36) NOT NULL,

    -- OAuth fields
    state_id text NOT NULL UNIQUE,
    code_verifier text NOT NULL,
    code_challenge text NOT NULL,
    code_method text NOT NULL DEFAULT 'S256',
    authorization_code text,
    access_token text,
    access_token_expires_at timestamp with time zone,

    -- Document status JSON
    doc_status text NOT NULL DEFAULT '{}',

    -- Session status
    session_status text NOT NULL DEFAULT 'PENDING',
    response_code text,
    response_description text,

    -- Response
    token_response text,

    -- Metadata
    merchant_id character varying(36),
    merchant_operating_city_id character varying(36),
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Indexes
CREATE INDEX idx_digilocker_verification_driver_id
  ON atlas_driver_offer_bpp.digilocker_verification(driver_id);

CREATE INDEX idx_digilocker_verification_state_id
  ON atlas_driver_offer_bpp.digilocker_verification(state_id);

CREATE INDEX idx_digilocker_verification_latest_session
  ON atlas_driver_offer_bpp.digilocker_verification(driver_id, created_at DESC);

-- Add verifiedBy to document tables
ALTER TABLE atlas_driver_offer_bpp.driver_license
  ADD COLUMN verified_by text;

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card
  ADD COLUMN verified_by text;

ALTER TABLE atlas_driver_offer_bpp.aadhaar_card
  ADD COLUMN verified_by text;

-- Add DigiLocker config to transporter_config
ALTER TABLE atlas_driver_offer_bpp.transporter_config
  ADD COLUMN digilocker_enabled boolean DEFAULT false,
  ADD COLUMN digilocker_client_id text,
  ADD COLUMN digilocker_client_secret text,
  ADD COLUMN digilocker_redirect_uri text;
```

## 5. New DigiLocker APIs

### API Endpoints

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/DriverOnboarding.hs`

```haskell
type API =
  "driver" :> "register"
    :> ( ...existing endpoints...
       :<|> "digilocker"
         :> ( "initiate"
                :> TokenAuth
                :> Post '[JSON] Digilocker.InitiateDigilockerRes
            :<|> "callback"
                :> QueryParam "code" Text
                :> QueryParam "state" Text
                :> QueryParam "error" Text
                :> Get '[JSON] APISuccess
            :<|> "pull" :> Capture "docType" DocumentType
                :> TokenAuth
                :> ReqBody '[JSON] Digilocker.PullDocumentReq
                :> Post '[JSON] APISuccess
            )
       )
```

### DigiLocker Service Module

**Location**: `Backend/lib/shared-services/src/Tools/DigilockerService.hs`

```haskell
module Tools.DigilockerService where

-- Base URL
digilockerBaseUrl :: Text
digilockerBaseUrl = "https://digilocker.meripehchaan.gov.in/public/oauth2/1/"

-- Token exchange
getAccessToken ::
  Text  -- clientId
  -> Text  -- clientSecret
  -> Text  -- authorizationCode
  -> Text  -- codeVerifier
  -> Text  -- redirectUri
  -> Flow DigilockerTokenResponse

data DigilockerTokenResponse = DigilockerTokenResponse
  { accessToken :: Text
  , tokenType :: Text
  , expiresIn :: Int
  , scope :: Maybe Text  -- Space-separated doc URIs
  }

-- Get document file
getDocumentFile ::
  Text  -- accessToken
  -> Text  -- documentUri
  -> Flow ByteString

-- Get document XML
getDocumentXml ::
  Text  -- accessToken
  -> Text  -- documentUri
  -> Flow Text

-- Pull document
pullDocument ::
  Text  -- accessToken
  -> Text  -- orgId
  -> Text  -- docType
  -> Text  -- docNumber (PAN/Aadhaar/DL number)
  -> Flow DigilockerPullResponse

data DigilockerPullResponse = DigilockerPullResponse
  { documentUri :: Text
  }
```

## 6. API Implementation

### POST /driver/register/digilocker/initiate

**Flow**:

```haskell
initiateDigilockerFlow :: (Id Person, Id Merchant, Id MerchantOperatingCity) -> Flow InitiateDigilockerRes
initiateDigilockerFlow (personId, merchantId, merchantOpCityId) = do
  -- 1. Get TransporterConfig
  transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing
    >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  -- 2. Check if DigiLocker enabled for this merchant+city
  unless (transporterConfig.digilockerEnabled == Just True) $
    throwError $ InvalidRequest "DigiLocker not enabled for this merchant"

  -- 3. Get merchant and check
  merchant <- findMerchantById merchantId
  merchantOpCity <- findById merchantOpCityId
  unless (merchant.shortId == "NAMMA_YATRI" && merchantOpCity.city == "Delhi") $
    throwError $ InvalidRequest "DigiLocker only available for Namma Yatri Delhi"

  -- 4. Check for active session
  latestSession <- findLatestByDriverId personId
  case latestSession of
    Just session -> do
      now <- getCurrentTime
      let isActive = session.sessionStatus == "PENDING" && session.accessTokenExpiresAt > Just now

      when isActive $
        throwError $ InvalidRequest "Active DigiLocker session already exists. Please wait or retry after 1 hour."

      -- Check docStatus for any PENDING docs
      let docStatusJson = parseJSON session.docStatus
      when (hasAnyPendingDocs docStatusJson) $
        throwError $ InvalidRequest "Previous session still processing documents"

      -- If failed/consent_denied/expired, allow new session
      -- Continue to create new session
    Nothing -> pure ()

  -- 5. Generate PKCE parameters
  randomBytes <- liftIO $ getRandomBytes 24
  timestamp <- liftIO $ getCurrentTime <&> (floor . (* 1000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds)
  let codeVerifier = base64Encode (randomBytes <> pack (show timestamp))
      codeChallenge = base64UrlEncode $ sha256 codeVerifier
      codeMethod = "S256"

  -- 6. Generate stateId
  stateId <- generateGUID

  -- 7. Create session record
  sessionId <- generateGUID
  now <- getCurrentTime
  create $ DigilockerVerification
    { id = sessionId
    , driverId = personId
    , stateId = stateId
    , codeVerifier = codeVerifier
    , codeChallenge = codeChallenge
    , codeMethod = codeMethod
    , authorizationCode = Nothing
    , accessToken = Nothing
    , accessTokenExpiresAt = Nothing
    , docStatus = "{}"  -- Empty JSON
    , sessionStatus = "PENDING"
    , responseCode = Nothing
    , responseDescription = Nothing
    , tokenResponse = Nothing
    , merchantId = Just merchantId
    , merchantOperatingCityId = Just merchantOpCityId
    , createdAt = now
    , updatedAt = now
    }

  -- 8. Construct DigiLocker authorization URL
  let baseUrl = fromJust transporterConfig.digilockerRedirectUri  -- Get from config
      authUrl = "https://digilocker.meripehchaan.gov.in/public/oauth2/1/authorize"
        <> "?response_type=code"
        <> "&client_id=" <> fromJust transporterConfig.digilockerClientId
        <> "&redirect_uri=" <> urlEncode baseUrl
        <> "&state=" <> stateId
        <> "&code_challenge=" <> codeChallenge
        <> "&code_challenge_method=S256"

  return InitiateDigilockerRes { authorizationUrl = authUrl, stateId = stateId }
```

### GET /driver/register/digilocker/callback

**Flow**:

```haskell
handleDigilockerCallback :: DigilockerCallbackParams -> Flow APISuccess
handleDigilockerCallback DigilockerCallbackParams{code, state, error} = do
  -- 1. Validate parameters
  when (isNothing state || isNothing code && isNothing error) $ do
    logError "DigiLocker callback: stateId or code missing"
    -- Set alert since we can't identify driver
    throwError $ InternalError "Invalid callback parameters"

  let stateId = fromJust state

  -- 2. Find session by stateId
  session <- findByStateId stateId >>= fromMaybeM (InvalidRequest "Invalid state")
  person <- Person.findById session.driverId >>= fromMaybeM (PersonDoesNotExist session.driverId.getId)

  -- 3. Check merchant+city
  transporterConfig <- findByMerchantOpCityId person.merchantOperatingCityId Nothing
  unless (transporterConfig.digilockerEnabled == Just True) $
    throwError $ InvalidRequest "DigiLocker not enabled"

  -- 4. Handle consent denied
  when (isJust error) $ do
    updateSessionStatus session.id "CONSENT_DENIED" Nothing (Just "User denied consent")
    logInfo $ "DigiLocker consent denied: " <> person.id.getId
    return Success

  -- 5. Validate code
  when (isNothing code) $ do
    updateSessionStatus session.id "FAILED" Nothing (Just "Authorization code missing")
    throwError $ InvalidRequest "Authorization code missing"

  let authCode = fromJust code

  -- 6. Check if token already exists (shouldn't happen but handle)
  when (isJust session.accessToken) $ do
    now <- getCurrentTime
    when (session.accessTokenExpiresAt > Just now) $ do
      logWarning "Token already exists and not expired"
      return Success

  -- 7. Fork background processing
  fork "process_digilocker_token" $ do
    processTokenAndDocuments session person authCode transporterConfig

  return Success

processTokenAndDocuments :: DigilockerVerification -> Person -> Text -> TransporterConfig -> Flow ()
processTokenAndDocuments session person authCode config = do
  -- 1. Call /token API
  result <- try @_ @SomeException $ DigilockerService.getAccessToken
    (fromJust config.digilockerClientId)
    (fromJust config.digilockerClientSecret)
    authCode
    session.codeVerifier
    (fromJust config.digilockerRedirectUri)

  case result of
    Left err -> do
      updateSessionStatus session.id "FAILED" (Just "500") (Just $ "Token exchange failed: " <> show err)
      logError $ "Token exchange failed: " <> show err

    Right tokenResp -> do
      -- 2. Update session with token
      now <- getCurrentTime
      let expiresAt = addUTCTime (fromIntegral tokenResp.expiresIn) now
      updateAccessToken session.stateId tokenResp.accessToken expiresAt authCode

      -- 3. Get required docs for this merchant+city
      allDocConfigs <- CQDVC.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing
      let requiredDocTypes = filter (.isMandatory) allDocConfigs <&> (.documentType)

      -- 4. Get already verified docs
      alreadyVerifiedDocs <- getAlreadyVerifiedDocs person.id
      let unavailableDocs = requiredDocTypes \\ alreadyVerifiedDocs

      -- 5. Parse scope from token response
      let scopeUris = maybe [] T.words tokenResp.scope
          issuedDocs = filter ("issued/" `T.isPrefixOf`) scopeUris
          pullDocs = filter ("pull/" `T.isPrefixOf`) scopeUris

      -- 6. Check consent for all unavailable docs
      initialDocStatus <- buildInitialDocStatus unavailableDocs issuedDocs pullDocs

      -- Check if we have consent for all required docs
      let hasAllConsent = all (\docType -> docType `elem` map fst issuedDocs || docType `elem` map fst pullDocs) unavailableDocs

      unless hasAllConsent $ do
        updateSessionStatus session.id "CONSENT_DENIED" Nothing (Just "Partial consent - not all required documents")
        updateDocStatus session.id (toJSON initialDocStatus)
        logWarning "Partial consent received"
        return ()

      -- 7. Update docStatus with PENDING/PULL_REQUIRED
      updateDocStatus session.id (toJSON initialDocStatus)
      updateSessionStatus session.id "SUCCESS" (Just "200") (Just "Token received")

      -- 8. Process each issued document
      forM_ issuedDocs $ \(docType, docUri) -> do
        verifyDocument session person tokenResp.accessToken docType docUri

      logInfo $ "DigiLocker documents processed for driver: " <> person.id.getId

verifyDocument :: DigilockerVerification -> Person -> Text -> DocumentType -> Text -> Flow ()
verifyDocument session person accessToken docType docUri = do
  result <- try @_ @SomeException $ do
    -- 1. Fetch PDF
    pdfBytes <- DigilockerService.getDocumentFile accessToken docUri

    -- 2. Upload to S3 using existing validateImage
    let base64Image = base64Encode pdfBytes
    imageResp <- Image.validateImage False (person.id, person.merchantId, person.merchantOperatingCityId) $
      ImageValidateRequest
        { image = base64Image
        , imageType = docType
        , rcNumber = Nothing
        , validationStatus = Just Domain.VALID
        , workflowTransactionId = Nothing
        , sdkFailureReason = Nothing
        , vehicleCategory = Nothing
        }

    -- 3. Fetch XML
    xmlData <- DigilockerService.getDocumentXml accessToken docUri

    -- 4. Parse and validate data
    docDetails <- parseAndValidateXml xmlData docType person

    -- 5. Store in respective table
    now <- getCurrentTime
    case docType of
      DriverLicense -> do
        dlId <- generateGUID
        DLQuery.create $ DriverLicense
          { id = dlId
          , driverId = person.id
          , licenseNumber = docDetails.licenseNumber
          , documentImageId1 = imageResp.imageId
          , verificationStatus = Documents.VALID
          , verifiedBy = Just "DIGILOCKER"
          , ...other fields...
          , createdAt = now
          }

      PanCard -> do
        panId <- generateGUID
        PanQuery.create $ PanCard
          { id = panId
          , driverId = person.id
          , panCardNumber = docDetails.panNumber
          , verificationStatus = Documents.VALID
          , verifiedBy = Just "DIGILOCKER"
          , ...
          }

      AadhaarCard -> do
        -- Similar...

    -- 6. Update docStatus to SUCCESS
    updateDocStatusForDocument session.id docType "SUCCESS" (Just "200") (Just "Document verified")

  case result of
    Left err -> do
      updateDocStatusForDocument session.id docType "FAILED" (Just "500") (Just $ show err)
      logError $ "Failed to verify " <> show docType <> ": " <> show err
    Right _ -> pure ()
```

### POST /driver/register/digilocker/pull/:docType

**Flow**:

```haskell
pullDigilockerDocument :: (Id Person, Id Merchant, Id MerchantOperatingCity) -> DocumentType -> PullDocumentReq -> Flow APISuccess
pullDigilockerDocument (personId, merchantId, merchantOpCityId) docType PullDocumentReq{docParams} = do
  -- 1. Check merchant+city
  transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing
  unless (transporterConfig.digilockerEnabled == Just True) $
    throwError $ InvalidRequest "DigiLocker not enabled"

  -- 2. Get latest session
  latestSession <- findLatestByDriverId personId >>= fromMaybeM (InvalidRequest "No DigiLocker session found")

  -- 3. Check session not expired
  now <- getCurrentTime
  when (latestSession.accessTokenExpiresAt <= Just now) $
    throwError $ InvalidRequest "DigiLocker session expired. Please restart the process."

  when (isNothing latestSession.accessToken) $
    throwError $ InvalidRequest "Access token not available"

  -- 4. Get orgId from config (merchant-specific)
  let orgId = getOrgIdForDocType docType  -- e.g., "in.gov.pan" for PAN

  -- 5. Call DigiLocker /pull API
  result <- try @_ @SomeException $ DigilockerService.pullDocument
    (fromJust latestSession.accessToken)
    orgId
    (show docType)
    (extractDocNumber docParams)  -- e.g., PAN number from params

  case result of
    Left err -> do
      updateDocStatusForDocument latestSession.id docType "FAILED" (Just "500") (Just $ show err)
      throwError $ InternalError $ "Pull document failed: " <> show err

    Right pullResp -> do
      -- 6. Update docStatus to PENDING
      updateDocStatusForDocument latestSession.id docType "PENDING" Nothing Nothing

      -- 7. Verify the pulled document (same as issued doc)
      person <- Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      verifyDocument latestSession person (fromJust latestSession.accessToken) docType pullResp.documentUri

      return Success
```

## 7. Update Status Handler API

### Add DigiLocker Status

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverOnboarding/Status.hs`

```haskell
data StatusRes = StatusRes
  { ...existing fields...
  , digilockerStatus :: Maybe DigilockerOverallStatus
  }

data DigilockerOverallStatus =
  | FAILED
  | PENDING
  | SUCCESS
  | CONSENT_DENIED

-- In statusHandler implementation
statusHandler :: ... -> Flow StatusRes
statusHandler (personId, merchantId, merchantOpCityId) ... = do
  ...existing logic...

  -- Add DigiLocker status
  digilockerStatus <- getDigilockerOverallStatus personId

  pure $ StatusRes { ...existing fields..., digilockerStatus }

getDigilockerOverallStatus :: Id Person -> Flow (Maybe DigilockerOverallStatus)
getDigilockerOverallStatus driverId = do
  latestSession <- findLatestByDriverId driverId
  case latestSession of
    Nothing -> return Nothing
    Just session -> do
      -- Check sessionStatus
      if session.sessionStatus == "CONSENT_DENIED"
        then return $ Just CONSENT_DENIED
        else do
          -- Parse docStatus JSON
          let docStatusJson = parseJSON session.docStatus
          -- Check if any doc has CONSENT_DENIED
          if hasAnyConsentDenied docStatusJson
            then return $ Just CONSENT_DENIED
            else if hasAnyPending docStatusJson
            then return $ Just PENDING
            else if hasAnyFailed docStatusJson
            then return $ Just FAILED
            else return $ Just SUCCESS
```

### Add Document Status

```haskell
-- In DocumentStatusItem
data DocumentStatusItem = DocumentStatusItem
  { documentType :: DDVC.DocumentType
  , verificationStatus :: ResponseStatus  -- Add PULL_REQUIRED, CONSENT_DENIED
  , verificationMessage :: Maybe Text
  , verificationUrl :: Maybe BaseUrl
  , verifiedBy :: Maybe Text
  , isPullRequired :: Bool
  }

-- Update ResponseStatus enum
data ResponseStatus =
  NO_DOC_AVAILABLE
  | PENDING
  | VALID
  | FAILED
  | INVALID
  | LIMIT_EXCEED
  | MANUAL_VERIFICATION_REQUIRED
  | UNAUTHORIZED
  | PULL_REQUIRED  -- NEW
  | CONSENT_DENIED  -- NEW
```

## 8. Helper Functions

### Check Already Verified Docs

```haskell
getAlreadyVerifiedDocs :: Id Person -> Flow [DocumentType]
getAlreadyVerifiedDocs driverId = do
  verifiedDocs <- []

  -- Check DL
  mbDL <- DLQuery.findByDriverId driverId
  when (isJust mbDL && (mbDL <&> (.verificationStatus)) == Just Documents.VALID) $
    verifiedDocs := DriverLicense : verifiedDocs

  -- Check PAN
  mbPan <- PanQuery.findByDriverId driverId
  when (isJust mbPan && (mbPan <&> (.verificationStatus)) == Just Documents.VALID) $
    verifiedDocs := PanCard : verifiedDocs

  -- Check Aadhaar
  mbAadhaar <- AadhaarQuery.findByDriverId driverId
  when (isJust mbAadhaar && (mbAadhaar <&> (.verificationStatus)) == Just Documents.VALID) $
    verifiedDocs := AadhaarCard : verifiedDocs

  return verifiedDocs
```

### Parse Doc Type from URI

```haskell
parseDocTypeFromUri :: Text -> Flow DocumentType
parseDocTypeFromUri uri
  | "DRVLC" `T.isInfixOf` uri = return DriverLicense
  | "PANCR" `T.isInfixOf` uri = return PanCard
  | "uidai" `T.isInfixOf` uri = return AadhaarCard
  | otherwise = throwError $ InvalidRequest "Unknown document type in URI"
```

## Summary

### New Tables:

1. **DigilockerVerification** - Session tracking with JSON docStatus

### Modified Tables:

1. **DriverLicense** - add `verifiedBy`
2. **PanCard** - add `verifiedBy`
3. **AadhaarCard** - add `verifiedBy`
4. **TransporterConfig** - add DigiLocker fields

### New APIs:

1. POST `/driver/register/digilocker/initiate`
2. GET `/driver/register/digilocker/callback`
3. POST `/driver/register/digilocker/pull/:docType`

### Modified APIs:

1. GET `/driver/register/status` - add `digilockerStatus`

### Configuration:

- Enable only for Namma Yatri + Delhi via TransporterConfig
- Store client credentials in TransporterConfig table

### Key Features:

- ✅ No Redis dependency
- ✅ JSON-based document status tracking
- ✅ Session expiry checks (1 hour)
- ✅ Support for issued and pull documents
- ✅ Merchant+city specific enablement
- ✅ Existing verification flows unchanged
- ✅ Clean audit trail via `verifiedBy` field