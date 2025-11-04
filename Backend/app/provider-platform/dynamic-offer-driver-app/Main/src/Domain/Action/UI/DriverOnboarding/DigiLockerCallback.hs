module Domain.Action.UI.DriverOnboarding.DigiLockerCallback where

import qualified Data.Text as T
import Environment
import Kernel.Prelude
import Kernel.Utils.Common hiding (Error)
import Servant hiding (throwError)
import Tools.Error

-- | DigiLocker OAuth 2.0 Callback API
-- This endpoint receives the OAuth callback from DigiLocker after user authorization.
--
-- Flow:
-- 1. App initiates OAuth flow with response_type=code
-- 2. User authorizes on DigiLocker
-- 3. DigiLocker redirects to this callback with authorization code
-- 4. We exchange the code for an access token (via tokenization API)
-- 5. Use access token to pull documents from DigiLocker
--
-- Success Parameters (response_type=code):
--   - code: Authorization code to exchange for access token
--   - state: Application-specific data (contains driverId and codeVerifier)
--
-- Error Parameters (OAuth 2.0 spec):
--   - error: Error code as per OAuth 2.0 spec
--   - error_description: Human-readable error description
--   - state: Original state (if available)
--
-- NOTE: Either 'code' OR 'error' will be present, not both
type DigiLockerCallbackAPI =
  "dobpp"
    :> "verify"
    :> "callback"
    :> "digiLocker"
    :> QueryParam "error" Text
    :> QueryParam "error_description" Text
    :> QueryParam "code" Text
    :> MandatoryQueryParam "state" Text
    :> Get '[JSON] AckResponse

-- | Handler for DigiLocker OAuth 2.0 callback
-- This is called by DigiLocker after user authorization
-- Parameters follow OAuth 2.0 Authorization Code flow
digiLockerCallbackHandler ::
  Maybe Text -> -- error (OAuth 2.0 error code)
  Maybe Text -> -- error_description
  Maybe Text -> -- code (authorization code) - only present in success case
  Text -> -- state (contains driverId and codeVerifier)
  Flow AckResponse
digiLockerCallbackHandler mbError mbErrorDescription mbCode stateParam = do
  logInfo $ "DigiLocker OAuth Callback Received - State: " <> stateParam
  logDebug $ "DigiLocker Callback - Has Code: " <> show (isJust mbCode) <> ", Has Error: " <> show (isJust mbError)

  -- Step 1: Validate state parameter (always required)
  when (T.null stateParam) $ do
    logError "DigiLocker callback - Missing required state parameter"
    throwError $ InvalidRequest "DigiLocker callback received with empty state parameter"

  -- Step 2: Handle OAuth 2.0 error cases
  -- If resource owner denies access or request fails, DigiLocker sends error parameters (no code)
  whenJust mbError $ \errorCode -> do
    let errorMsg = fromMaybe "Unknown OAuth error" mbErrorDescription
    logError $ "DigiLocker OAuth Error - Code: " <> errorCode <> ", Description: " <> errorMsg <> ", State: " <> stateParam
    -- Common OAuth 2.0 error codes: access_denied, invalid_request, unauthorized_client, etc.
    throwError $ InvalidRequest $ "DigiLocker OAuth Error: " <> errorCode <> " - " <> errorMsg

  -- Step 3: Validate authorization code (required in success case)
  code <- mbCode & fromMaybeM (InvalidRequest "DigiLocker callback - Missing authorization code in success response")
  when (T.null code) $ do
    logError "DigiLocker callback - Authorization code is empty"
    throwError $ InvalidRequest "DigiLocker callback received with empty authorization code"

  logDebug $ "DigiLocker Callback - Code: " <> (T.take 10 code) <> "..."

  -- Step 4: TODO - Implement the full OAuth callback flow
  -- The implementation should follow the HyperVerge pattern but adapted for DigiLocker:
  --
  -- 1. Fetch driver info from Redis using state parameter
  --    - State contains: driverId, codeVerifier (for PKCE)
  --    - Redis key format: mkDigiLockerStateKey stateParam
  --
  -- 2. Exchange authorization code for access token
  --    - Use DigiLocker tokenization API (already implemented in shared-kernel)
  --    - Send: code, codeVerifier, client credentials
  --    - Receive: access_token, expires_in, token_type
  --
  -- 3. Store access token in DigilockerVerification table
  --    - Link to driverId
  --    - Store token and expiry
  --
  -- 4. Optionally trigger document pull
  --    - Can pull documents immediately or wait for explicit pull request
  --    - Use Tools.Verification.pullDigiLockerDrivingLicense or getDigiLockerXml
  --
  -- 5. Update verification status
  --    - Similar to HyperVerge: call Status.statusHandler
  --    - Update driver onboarding status
  --
  -- 6. Store details in Redis for frontend polling
  --    - Key: mkDigiLockerDetailsKey driverId
  --    - Value: DigiLockerDetailsData with status

  logInfo $ "DigiLocker callback - Successfully validated OAuth callback for State: " <> stateParam
  logWarning "DigiLocker callback - Full implementation pending. Returning Ack."
  return Ack
