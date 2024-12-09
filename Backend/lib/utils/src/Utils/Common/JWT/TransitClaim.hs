{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Common.JWT.TransitClaim where

import Data.Aeson
import Data.Aeson as J
import Data.Aeson.Casing
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Data.String
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Kernel.Prelude hiding (exp)
import Kernel.Utils.JSON
import Kernel.Utils.JWT hiding (ServiceAccount (..))
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Web.JWT

data TransitTicketClaims = TransitTicketClaims
  { iss :: Text,
    aud :: Text,
    typ :: Text,
    origins :: [Text],
    payload :: Payload
  }
  deriving (Show, Generic)

instance ToJSON TransitTicketClaims

newtype Payload = Payload
  { transitObjects :: [TransitObject]
  }
  deriving (Show, Generic)

instance ToJSON Payload

data TransitObject = TransitObject
  { id :: Text,
    classId :: Text,
    state :: Text,
    tripType :: Text,
    passengerNames :: Text,
    ticketLeg :: TicketLeg,
    barcode :: Barcode
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data TicketLeg = TicketLeg
  { originStationCode :: Text,
    originName :: Name,
    destinationStationCode :: Text,
    destinationName :: Name,
    carriage :: Text,
    ticketSeat :: Maybe TicketSeat,
    departureDateTime :: Maybe Text,
    arrivalDateTime :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype Name = Name
  { defaultValue :: LanguageValue
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data LanguageValue = LanguageValue
  { language :: Text,
    _value :: Text
  }
  deriving (Show, Generic)

instance FromJSON LanguageValue where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LanguageValue where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data TicketSeat = TicketSeat
  { coach :: Text,
    seat :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Barcode = Barcode
  { _type :: Text,
    value :: Text,
    alternateText :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Barcode where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Barcode where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ServiceAccount = ServiceAccount
  { saType :: !T.Text,
    saProjectId :: !T.Text,
    saPrivateKeyId :: !T.Text,
    saPrivateKey :: !String,
    saClientEmail :: !T.Text,
    saClientId :: !T.Text,
    saAuthUri :: !T.Text,
    saTokenUri :: !T.Text,
    saAuthProviderX509CertUrl :: !T.Text,
    saClientX509CertUrl :: !T.Text,
    saUniverseDomain :: !T.Text,
    saIssuerId :: !T.Text
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''ServiceAccount)

newtype TransitObjectPatch = TransitObjectPatch
  { state :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

createJWT' :: ServiceAccount -> [(T.Text, Value)] -> IO (Either String (JWTClaimsSet, T.Text))
createJWT' sa additionalClaims = do
  let iss = stringOrURI . saClientEmail $ sa
  let aud = Left <$> (stringOrURI . saTokenUri $ sa)
  let unregisteredClaims = ClaimsMap $ Map.fromList additionalClaims
  let jwtHeader =
        JOSEHeader
          { typ = Just "JWT",
            cty = Nothing,
            alg = Just RS256,
            kid = Just $ saPrivateKeyId sa
          }
  let mkey = readRsaSecret . C8.pack $ saPrivateKey sa
  case mkey of
    Nothing -> pure $ Left "Bad RSA key!"
    Just pkey -> do
      let key = EncodeRSAPrivateKey pkey
      iat <- numericDate <$> getPOSIXTime
      exp <- numericDate . (+ 3600) <$> getPOSIXTime
      let searchRequest =
            mempty
              { exp = exp,
                iat = iat,
                iss = iss,
                aud = aud,
                unregisteredClaims = unregisteredClaims
              }
      pure $ Right (searchRequest, encodeSigned key jwtHeader searchRequest)

getJwtToken :: ServiceAccount -> [(T.Text, Value)] -> IO (Either String JWToken)
getJwtToken sa additionalClaims = do
  jwtPair <- createJWT' sa additionalClaims
  case jwtPair of
    Left err -> pure $ Left err
    Right (claimPairs, assertion) -> do
      let issuedAt = iat claimPairs
      manager <- newManager tlsManagerSettings
      let body =
            JWTBody
              { jwtAssertion = assertion,
                jwtGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"
              }
      req <- jwtRequest (saTokenUri sa) (encode body)
      res <- httpLbs req manager
      let rBody = J.eitherDecode $ responseBody res
      case rBody of
        Left err -> pure $ Left err
        Right respBody@JWToken {..} -> do
          let expiry = getExpiry issuedAt jwtExpiresIn
          pure $
            Right
              respBody
                { jwtExpiresIn = expiry
                }
