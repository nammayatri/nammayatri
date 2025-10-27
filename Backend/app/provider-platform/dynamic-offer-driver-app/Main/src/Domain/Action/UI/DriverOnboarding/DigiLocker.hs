{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.DigiLocker
  ( initiateDigiLocker,
    generateCodeVerifier,
    generateCodeChallenge,
    generateState,
    deleteDigiLockerSession,
    DigiLockerInitiateResp (..),
    DigiLockerStateData (..),
    DigiLockerSessionData (..),
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as Person

-- | Response type for DigiLocker initiation
data DigiLockerInitiateResp = DigiLockerInitiateResp
  { authorizationUrl :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Generate a secure random code verifier (43-128 characters, URL-safe)
generateCodeVerifier :: MonadFlow m => m Text
generateCodeVerifier = do
  -- Generate 32 random bytes (will result in 43 characters when base64url encoded)
  randomBytes <- liftIO $ getRandomBytes 32
  let verifier = TE.decodeUtf8 $ Base64URL.encode randomBytes
  -- Remove padding characters if any
  return $ T.takeWhile (/= '=') verifier

-- | Generate code challenge from verifier using SHA256 and base64url encoding
generateCodeChallenge :: Text -> Text
generateCodeChallenge verifier =
  let verifierBytes = TE.encodeUtf8 verifier
      hashed = SHA256.hash verifierBytes
      encoded = Base64URL.encode hashed
   in TE.decodeUtf8 $ BS.takeWhile (/= 61) encoded -- Remove padding (61 is ASCII '=')

-- | Generate a random state parameter
generateState :: MonadFlow m => m Text
generateState = do
  randomBytes <- liftIO $ getRandomBytes 16
  let stateValue = TE.decodeUtf8 $ Base64URL.encode randomBytes
  return $ T.takeWhile (/= '=') stateValue

-- | Data structure to store in Redis for state mapping
data DigiLockerStateData = DigiLockerStateData
  { driverId :: Id Person.Person,
    codeVerifier :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Data structure to store in Redis for session mapping
data DigiLockerSessionData = DigiLockerSessionData
  { stateId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Cache the state and code verifier in Redis with TTL
cacheDigiLockerState :: Text -> Text -> Id Person.Person -> Flow ()
cacheDigiLockerState state codeVerifier personId = do
  let stateKey = makeDigiLockerStateKey state
  let stateData = DigiLockerStateData {driverId = personId, codeVerifier = codeVerifier}
  let ttl = 3600 :: Integer -- 1 hour
  Redis.setExp stateKey stateData ttl

-- | Cache the session data in Redis with TTL
cacheDigiLockerSession :: Text -> Id Person.Person -> Flow ()
cacheDigiLockerSession state personId = do
  let sessionKey = makeDigiLockerSessionKey personId
  let sessionData = DigiLockerSessionData {stateId = state}
  let ttl = 3600 :: Integer -- 1 hour
  Redis.setExp sessionKey sessionData ttl

-- | Check if an active session exists for the driver
checkActiveSession :: Id Person.Person -> Flow (Maybe DigiLockerSessionData)
checkActiveSession personId = do
  let sessionKey = makeDigiLockerSessionKey personId
  Redis.get sessionKey

-- | Delete the session data from Redis
deleteDigiLockerSession :: Id Person.Person -> Flow ()
deleteDigiLockerSession personId = do
  let sessionKey = makeDigiLockerSessionKey personId
  Redis.del sessionKey

-- | Redis key for state -> driver data mapping: digilocker:state:{stateID}
makeDigiLockerStateKey :: Text -> Text
makeDigiLockerStateKey state = "digilocker:state:" <> state

-- | Redis key for session -> state mapping: digilocker:session:{driverID}
makeDigiLockerSessionKey :: Id Person.Person -> Text
makeDigiLockerSessionKey personId = "digilocker:session:" <> personId.getId

-- | Main handler for DigiLocker initiation
initiateDigiLocker ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Flow DigiLockerInitiateResp
initiateDigiLocker (personId, _merchantId, _merchantOpCityId) = do
  -- Verify person exists
  _person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Check if an active session already exists for this driver
  existingSession <- checkActiveSession personId
  whenJust existingSession $ \_ ->
    throwError $ InvalidRequest "Active DigiLocker session already exists. Please complete or wait for the current session to expire."

  -- Generate PKCE parameters
  codeVerifier <- generateCodeVerifier
  let codeChallenge = generateCodeChallenge codeVerifier
  state <- generateState

  -- Cache state and code_verifier in Redis (digilocker:state:{stateID} -> {driverId, codeVerifier})
  cacheDigiLockerState state codeVerifier personId

  -- Cache session data in Redis (digilocker:session:{driverID} -> {stateId})
  cacheDigiLockerSession state personId

  -- TODO: Get these from TransporterConfig or environment
  let clientId = "YOUR_CLIENT_ID" -- Replace with actual client ID from config
      redirectUri = "https://api.moving.tech/dobpp/verify/callback/digiLocker" -- Your callback URL

  -- Construct complete DigiLocker authorization URL with all required parameters
  let authUrl =
        "https://digilocker.meripehchaan.gov.in/public/oauth2/1/authorize"
          <> "?response_type=code"
          <> "&client_id="
          <> clientId
          <> "&state="
          <> state
          <> "&redirect_uri="
          <> redirectUri
          <> "&code_challenge="
          <> codeChallenge
          <> "&code_challenge_method=S256"
          <> "&pla=Y" -- Pre-Login Authentication
          <> "&plsignup=Y" -- Pre-Login Signup
          <> "&ulsignup=Y" -- User Level Signup
          <> "&purpose=verification" -- Purpose of DigiLocker access
  return $ DigiLockerInitiateResp {authorizationUrl = authUrl}
