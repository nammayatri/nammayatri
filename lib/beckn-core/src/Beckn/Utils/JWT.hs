{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Beckn.Utils.JWT where

import Data.Maybe
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.String
import Data.Bool
import qualified Data.Map as Map
import Data.Time.Clock.POSIX

import Control.Applicative
import Control.Monad
import Web.JWT as JWT

import Control.Lens.TH
import qualified Data.Aeson as J
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Default.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import EulerHS.Prelude hiding (exp, fromRight)

import           Network.HTTP.Client.TLS
import           Network.HTTP.Client
import           Network.HTTP.Types


data ServiceAccount = ServiceAccount
  { _saType :: !T.Text
  , _saProjectId :: !T.Text
  , _saPrivateKeyId :: !T.Text
  , _saPrivateKey :: !String
  , _saClientEmail :: !T.Text
  , _saClientId :: !T.Text
  , _saAuthUri :: !T.Text
  , _saTokenUri :: !T.Text
  , _saAuthProviderX509CertUrl :: !T.Text
  , _saClientX509CertUrl :: !T.Text
  } deriving (Show, Eq, Generic)

$(deriveFromJSON (aesonPrefix snakeCase) ''ServiceAccount)


data JWTBody = JWTBody
  { _jwtAssertion :: !T.Text
  , _jwtGrantType :: !T.Text
  } deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''JWTBody)

data JWTResponseBody = JWTResponseBody
  { _jwtAccessToken :: !T.Text
  , _jwtExpiresIn :: Integer
  , _jwtTokenType :: !Text
  } deriving (Show, Eq, Generic)

$(deriveFromJSON (aesonPrefix snakeCase) ''JWTResponseBody)


getJSON :: IO BL.ByteString
getJSON = BL.readFile "/home/jerry/Projects/juspay/token/jp-beckn-dev-4fbd238801a3.json"

getServiceAccount :: IO (Either String ServiceAccount)
getServiceAccount = J.eitherDecode <$> getJSON

createJWT :: ServiceAccount -> IO (Either String Text)
createJWT sa = do
  let iss = stringOrURI . _saClientEmail $ sa
  let aud = Left <$> (stringOrURI . _saTokenUri $ sa)
  let unregisteredClaims = ClaimsMap $ Map.fromList [("scope", String "https://www.googleapis.com/auth/firebase.messaging")]
  let jwtHeader = JOSEHeader {
          typ = Just "JWT"
        , cty = Nothing
        , alg = Just RS256
        , kid = Just $ _saPrivateKeyId sa
        }
  let mkey = readRsaSecret . C8.pack $ _saPrivateKey sa
  case mkey of
    Nothing -> pure $ Left "Bad RSA key!"
    Just pkey -> do
      let key = RSAPrivateKey pkey
      iat <- numericDate <$> getPOSIXTime
      exp <- numericDate . (+ 3600) <$> getPOSIXTime
      let cs = mempty {
              exp = exp
            , iat = iat
            , iss = iss
            , aud = aud
            , unregisteredClaims = unregisteredClaims
            }
      pure $ Right (encodeSigned key jwtHeader cs)

jwtRequest :: T.Text -> BL.ByteString -> IO Request
jwtRequest tokenUri body = do
  print tokenUri
  req <- parseRequest $ T.unpack tokenUri
  pure $ req { method = "POST"
             , requestHeaders = [ (hContentType, "application/json") ]
             , requestBody = RequestBodyLBS body
             }

refreshToken :: IO (Either String Text)
refreshToken = do
  sAccount <- getServiceAccount
  case sAccount of
    Left err -> pure $ Left err
    Right sa -> do
      jwt <- createJWT sa
      case jwt of
        Left err -> pure $ Left err
        Right assertion -> do
          manager <- newManager tlsManagerSettings
          let body = JWTBody {
                  _jwtAssertion = assertion
                , _jwtGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"
                }
          req <- jwtRequest (_saTokenUri sa) (J.encode body)
          print req
          print body
          res <- httpLbs req manager
          let rBody = J.eitherDecode $ responseBody res
          case rBody of
            Left err -> pure $ Left err
            Right respBody -> pure $ Right $ (_jwtTokenType respBody) <> (T.pack " ") <>  (_jwtAccessToken respBody)
          --pure $ Right ""
