{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.PlayIntegrity.Client
  ( DecodeFailure (..),
    PlayIntegrityFlow,
    decodeIntegrityToken,
  )
where

import AppAttestation.Domain.Types.Common (PlayIntegrityConfig (..))
import AppAttestation.PlayIntegrity.Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Kernel.Utils.JWT as JWT
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Network.HTTP.Types (status400)
import Servant hiding (throwError)
import Servant.Client (ClientError (..), ResponseF (..))

type PlayIntegrityFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  )

data DecodeFailure
  = TokenRejected
  | PlatformFailure Text
  deriving (Show, Eq)

type DecodeIntegrityTokenAPI =
  Header "Authorization" Text
    :> ReqBody '[JSON] DecodeIntegrityTokenReq
    :> Post '[JSON] DecodeIntegrityTokenRes

decodeIntegrityTokenAPI :: Proxy DecodeIntegrityTokenAPI
decodeIntegrityTokenAPI = Proxy

decodeIntegrityTokenClient :: Maybe Text -> DecodeIntegrityTokenReq -> ET.EulerClient DecodeIntegrityTokenRes
decodeIntegrityTokenClient = ET.client decodeIntegrityTokenAPI

decodeIntegrityToken :: PlayIntegrityFlow m r => PlayIntegrityConfig -> Text -> Text -> m (Either DecodeFailure TokenPayload)
decodeIntegrityToken config packageName integrityToken =
  getAccessToken config >>= \case
    Left err -> pure . Left $ PlatformFailure err
    Right accessToken -> do
      let decodeUrl = config.url {baseUrlPath = baseUrlPath config.url <> "/v1/" <> T.unpack packageName <> ":decodeIntegrityToken"}
          request = DecodeIntegrityTokenReq {integrityToken}
      callAPI decodeUrl (decodeIntegrityTokenClient (Just $ "Bearer " <> accessToken) request) "decodeIntegrityToken" decodeIntegrityTokenAPI
        <&> \case
          Right res -> Right res.tokenPayloadExternal
          Left (FailureResponse _ response) | responseStatusCode response == status400 -> Left TokenRejected
          Left err -> Left . PlatformFailure . T.take 200 $ show err

playIntegrityScope :: Text
playIntegrityScope = "https://www.googleapis.com/auth/playintegrity"

accessTokenKey :: JWT.ServiceAccount -> Text
accessTokenKey serviceAccount = "AppAttestation:PlayIntegrity:AccessToken:" <> JWT.saClientEmail serviceAccount

getAccessToken :: PlayIntegrityFlow m r => PlayIntegrityConfig -> m (Either Text Text)
getAccessToken config = do
  serviceAccountJson <- decrypt config.serviceAccountJson
  case A.eitherDecodeStrict (TE.encodeUtf8 serviceAccountJson) of
    Left err -> pure . Left $ "Invalid Play Integrity service account: " <> T.pack err
    Right serviceAccount ->
      Hedis.safeGet (accessTokenKey serviceAccount) >>= \case
        Just accessToken -> pure $ Right accessToken
        Nothing ->
          liftIO (fetchAccessToken serviceAccount) >>= \case
            Left err -> pure $ Left err
            Right (accessToken, ttlSeconds) -> do
              Hedis.setExp (accessTokenKey serviceAccount) accessToken ttlSeconds
              pure $ Right accessToken

fetchAccessToken :: JWT.ServiceAccount -> IO (Either Text (Text, Int))
fetchAccessToken serviceAccount =
  JWT.createJWT serviceAccount [("scope", A.String playIntegrityScope)] >>= \case
    Left err -> pure . Left $ "Unable to sign Play Integrity assertion: " <> T.pack err
    Right (_, assertion) ->
      handle (\(e :: SomeException) -> pure . Left $ "Play Integrity token request failed: " <> T.take 200 (show e)) $ do
        manager <- Http.getGlobalManager
        let body = JWT.JWTBody {jwtAssertion = assertion, jwtGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"}
        request <- JWT.jwtRequest (JWT.saTokenUri serviceAccount) (A.encode body)
        response <- Http.httpLbs request manager
        pure $ case A.eitherDecode (Http.responseBody response) of
          Left err -> Left $ "Play Integrity token response not understood: " <> T.pack err
          Right (token :: JWT.JWToken) ->
            Right (JWT.jwtAccessToken token, max 60 (fromIntegral (JWT.jwtExpiresIn token) - accessTokenSafetyMarginSeconds))

accessTokenSafetyMarginSeconds :: Int
accessTokenSafetyMarginSeconds = 120
