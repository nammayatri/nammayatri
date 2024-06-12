{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (AEAD (..), AEADMode (..), AuthTag (..), aeadInit, aeadSimpleDecrypt, cipherInit)
import Crypto.Error (CryptoFailable (..), maybeCryptoError)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.PartnerOrganization as PO
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.App
import Kernel.Types.Base64
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Common as Utils
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (Context, throwError)
import qualified Storage.CachedQueries.PartnerOrganization as CQPO
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Error

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = (Id Person.Person, Id Merchant.Merchant)
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPerson ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "authTokenCacheExpiry" r Seconds
  ) =>
  RegToken ->
  m (Id Person.Person, Id Merchant.Merchant)
verifyPerson token = do
  let key = authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  result <- Redis.safeGet key
  case result of
    Just (personId, merchantId) -> return (personId, merchantIdFallback merchantId)
    Nothing -> do
      sr <- verifyToken token
      let expiryTime = min sr.tokenExpiry authTokenCacheExpiry
      let personId = Id sr.entityId
      let merchantId = merchantIdFallback (Id sr.merchantId)
      Redis.setExp key (personId, merchantId) expiryTime
      return (personId, merchantId)

merchantIdFallback :: Id Merchant.Merchant -> Id Merchant.Merchant
merchantIdFallback "da4e23a5-3ce6-4c37-8b9b-41377c3c1a51" = "4b17bd06-ae7e-48e9-85bf-282fb310209c"
merchantIdFallback "c9811842-d572-11ed-afa1-0242ac120002" = "4b17bd06-ae7e-48e9-85bf-282fb310209c"
merchantIdFallback v = v

authTokenCacheKey :: RegToken -> Text
authTokenCacheKey regToken =
  "rider-platform:authTokenCacheKey:" <> regToken

verifyPersonAction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "authTokenCacheExpiry" r Seconds
  ) =>
  VerificationAction VerifyToken m
verifyPersonAction = VerificationAction verifyPerson

verifyToken :: (CacheFlow m r, EsqDBFlow m r) => RegToken -> m SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token
    >>= Utils.fromMaybeM (InvalidToken token)
    >>= validateToken

validateToken :: EsqDBFlow m r => SR.RegistrationToken -> m SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal updatedAt
  unless verified $ Utils.throwError TokenIsNotVerified
  when expired $ Utils.throwError TokenExpired
  return sr

-- TODO Next logic is the same for rider-app, beckn-transport and driver-offer-bpp. Move it to Lib

type DashboardTokenAuth = HeaderAuth "token" DashboardVerifyToken

data DashboardVerifyToken = DashboardVerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (DashboardTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

data Dashboard = Dashboard

instance VerificationMethod DashboardVerifyToken where
  type VerificationResult DashboardVerifyToken = Dashboard
  verificationDescription =
    "Checks whether dashboard token is registered."

verifyDashboardAction :: HasFlowEnv m r '["dashboardToken" ::: Text] => VerificationAction DashboardVerifyToken m
verifyDashboardAction = VerificationAction verifyDashboard

-- Do we need some expiry time for dashboard token?
verifyDashboard :: HasFlowEnv m r '["dashboardToken" ::: Text] => RegToken -> m Dashboard
verifyDashboard incomingToken = do
  dashboardToken <- asks (.dashboardToken)
  if incomingToken == dashboardToken
    then pure Dashboard
    else throwError (InvalidToken "dashboard token") -- we shouldn't show to dashboard user incoming token

decryptAES128 :: (MonadThrow m, Log m) => Maybe Base64 -> Text -> m Text
decryptAES128 Nothing encryptedText = return encryptedText
decryptAES128 (Just (Base64 cipherText)) encryptedText = do
  aes <- fromMaybeM (InternalError "Failed to decode CipherText") $ maybeCryptoError (cipherInit $ Base64.decodeLenient cipherText :: CryptoFailable AES128)
  let (nonce, remaining) = BS.splitAt 12 $ Base64.decodeLenient $ TE.encodeUtf8 encryptedText
      (encrypted, authTag) = BS.splitAt (BS.length remaining - 16) remaining
  aeadState <- fromMaybeM (InternalError "Failed to initialize AEAD cipher") $ maybeCryptoError (aeadInit AEAD_GCM aes (nonce :: ByteString) :: CryptoFailable (AEAD AES128))
  decrypted <- fromMaybeM (InternalError "Decryption failed") $ aeadSimpleDecrypt aeadState (BA.empty :: BS.ByteString) encrypted (mkAuthTag authTag)
  return $ TE.decodeUtf8 decrypted
  where
    mkAuthTag authTag = AuthTag $ BA.convert authTag

-- | Performs authentication based on PartnerOrganization APIKey.
type APIKey = Text

type PartnerOrganizationAPIKey = HeaderAuth "x-api-key" PartnerOrganizationVerifyAPIKey

data PartnerOrganizationVerifyAPIKey = PartnerOrganizationVerifyAPIKey

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (PartnerOrganizationAPIKey :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod PartnerOrganizationVerifyAPIKey where
  type VerificationResult PartnerOrganizationVerifyAPIKey = PartnerOrganization
  verificationDescription =
    "Checks whether partner organization api key is registered.\
    \If you don't have a partner organization api key, please contact our support team."

verifyPartnerOrganizationAction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  VerificationAction PartnerOrganizationVerifyAPIKey m
verifyPartnerOrganizationAction = VerificationAction verifyPartnerOrganization

verifyPartnerOrganization ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  APIKey ->
  m PartnerOrganization
verifyPartnerOrganization apiKey = do
  apiKeyHash <- getDbHash apiKey
  mbPartnerOrganization <- B.runInReplica $ CQPO.findByApiKeyHash apiKeyHash
  mbPartnerOrganization & fromMaybeM (InvalidAPIKey apiKey)
