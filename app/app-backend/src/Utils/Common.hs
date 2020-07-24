module Utils.Common where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Utils.Extra as Utils
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.Auth
import qualified Data.ByteString.Lazy as BSL
import Data.Text as DT
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Servant.Client.Core.ClientError
import Servant.Client.Core.Response
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Test.RandomStrings as RS

-- | Performs simple token verification.
type TokenAuth = TokenAuth' "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: *) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPerson :: RegToken -> Flow Person.Person
verifyPerson token = do
  sr <- Utils.Common.verifyToken token
  Person.findById (PersonId $ SR._EntityId sr)
    >>= fromMaybeM500 "Could not find user"

verifyPersonAction :: VerificationAction VerifyToken AppEnv
verifyPersonAction = VerificationAction Utils.Common.verifyPerson

verifyToken :: RegToken -> Flow SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token
    >>= fromMaybeM400 "INVALID_TOKEN"
    >>= validateToken

validateToken :: SR.RegistrationToken -> Flow SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ _tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal _updatedAt
  when expired (L.throwException $ err400 {errBody = "TOKEN_EXPIRED"})
  return sr

fromMaybeM :: ServerError -> Maybe a -> Flow a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a) = return a

fromMaybeM400, fromMaybeM500, fromMaybeM503 :: BSL.ByteString -> Maybe a -> Flow a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

generateShortId :: Flow Text
generateShortId = T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

-- TODO: figure out a way to extract the url directly from EulerClient
callAPI baseUrl req serviceName = do
  endTracking <- L.runUntracedIO $ Metrics.startTracking serviceName
  res <- L.callAPI baseUrl req
  let status = case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"
  _ <- L.runUntracedIO $ endTracking status
  return res
