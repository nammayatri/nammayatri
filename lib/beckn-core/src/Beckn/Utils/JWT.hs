{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.JWT where

import Control.Applicative
import qualified Data.Aeson as J
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import EulerHS.Prelude hiding (exp, fromRight)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.Environment
import Web.JWT

data ServiceAccount = ServiceAccount
  { _saType :: !T.Text,
    _saProjectId :: !T.Text,
    _saPrivateKeyId :: !T.Text,
    _saPrivateKey :: !String,
    _saClientEmail :: !T.Text,
    _saClientId :: !T.Text,
    _saAuthUri :: !T.Text,
    _saTokenUri :: !T.Text,
    _saAuthProviderX509CertUrl :: !T.Text,
    _saClientX509CertUrl :: !T.Text
  }
  deriving (Show, Eq, Generic)

$(deriveFromJSON (aesonPrefix snakeCase) ''ServiceAccount)

data JWTBody = JWTBody
  { _jwtAssertion :: !T.Text,
    _jwtGrantType :: !T.Text
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''JWTBody)

data JWToken = JWToken
  { _jwtAccessToken :: !T.Text,
    _jwtExpiresIn :: Integer,
    _jwtTokenType :: !Text
  }
  deriving (Show, Eq, Generic)

$(deriveFromJSON (aesonPrefix snakeCase) ''JWToken)

getJSON :: IO (Either String BL.ByteString)
getJSON = do
  saFileName <- lookupEnv "FCM_JSON_PATH"
  case saFileName of
    Nothing -> pure $ Left "FCM service account json not found"
    Just f -> do
      bs <- BL.readFile f
      pure $ Right bs

getServiceAccount :: IO (Either String ServiceAccount)
getServiceAccount = do
  res <- getJSON
  pure $ case res of
    Left err -> Left err
    Right json -> J.eitherDecode json

createJWT :: ServiceAccount -> [(Text, Value)] -> IO (Either String (JWTClaimsSet, Text))
createJWT sa additionalClaims = do
  let iss = stringOrURI . _saClientEmail $ sa
  let aud = Left <$> (stringOrURI . _saTokenUri $ sa)
  let unregisteredClaims = ClaimsMap $ Map.fromList additionalClaims
  let jwtHeader =
        JOSEHeader
          { typ = Just "JWT",
            cty = Nothing,
            alg = Just RS256,
            kid = Just $ _saPrivateKeyId sa
          }
  let mkey = readRsaSecret . C8.pack $ _saPrivateKey sa
  case mkey of
    Nothing -> pure $ Left "Bad RSA key!"
    Just pkey -> do
      let key = RSAPrivateKey pkey
      iat <- numericDate <$> getPOSIXTime
      exp <- numericDate . (+ 3600) <$> getPOSIXTime
      let cs =
            mempty
              { exp = exp,
                iat = iat,
                iss = iss,
                aud = aud,
                unregisteredClaims = unregisteredClaims
              }
      pure $ Right (cs, (encodeSigned key jwtHeader cs))

jwtRequest :: T.Text -> BL.ByteString -> IO Request
jwtRequest tokenUri body = do
  req <- parseRequest $ T.unpack tokenUri
  pure $
    req
      { method = "POST",
        requestHeaders = [(hContentType, "application/json")],
        requestBody = RequestBodyLBS body
      }

refreshToken :: IO (Either String Text)
refreshToken = do
  sAccount <- getServiceAccount
  case sAccount of
    Left err -> pure $ Left err
    Right sa -> do
      jwtPair <- createJWT sa [("scope", String "https://www.googleapis.com/auth/firebase.messaging")]
      case jwtPair of
        Left err -> pure $ Left err
        Right (claimPairs, assertion) -> do
          let issuedAt = iat claimPairs
          manager <- newManager tlsManagerSettings
          let body =
                JWTBody
                  { _jwtAssertion = assertion,
                    _jwtGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"
                  }
          req <- jwtRequest (_saTokenUri sa) (J.encode body)
          res <- httpLbs req manager
          let rBody = J.eitherDecode $ responseBody res
          case rBody of
            Left err -> pure $ Left err
            Right respBody -> do
              let token = _jwtTokenType respBody <> T.pack " " <> _jwtAccessToken respBody
              setEnv "FCM_AUTH_TOKEN" $ T.unpack token
              setEnv "FCM_AUTH_TOKEN_EXPIRY" $ show $ getExpiry issuedAt (_jwtExpiresIn respBody)
              pure $ Right token

getExpiry :: Maybe NumericDate -> Integer -> Integer
getExpiry Nothing expiresIn = expiresIn
getExpiry (Just d) expiresIn =
  expiresIn + (round $ nominalDiffTimeToSeconds (secondsSinceEpoch d))

getToken :: IO (Either String Text)
getToken = do
  token <- lookupEnv "FCM_AUTH_TOKEN"
  expiry <- lookupEnv "FCM_AUTH_TOKEN_EXPIRY"
  case token of
    Nothing -> refreshToken
    Just t ->
      case expiry of
        Nothing -> refreshToken
        Just e -> do
          let expInt = read e :: Integer
          curInt <- round <$> getPOSIXTime
          if curInt > expInt - 60
            then refreshToken
            else pure . Right $ T.pack t
