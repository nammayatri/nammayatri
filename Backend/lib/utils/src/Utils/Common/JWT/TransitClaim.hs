{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Common.JWT.TransitClaim where

import Data.Aeson as J
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Data.String
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Utils.JSON
import Kernel.Utils.JWT hiding (ServiceAccount (..))
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Web.JWT hiding (claims)

newtype Payload = Payload
  { transitObjects :: [TransitObject]
  }
  deriving (Show, Generic)

instance ToJSON Payload

data TransitObject = TransitObject
  { id :: Text,
    classId :: Text,
    tripId :: Text,
    state :: Text,
    tripType :: Text,
    customCardTitle :: Name,
    passengerType :: Text,
    passengerNames :: Text,
    validTimeInterval :: TimeInterval,
    ticketLeg :: TicketLeg,
    barcode :: Maybe Barcode,
    textModulesData :: [TextModule],
    groupingInfo :: GroupingInfo,
    linksModuleData :: Maybe LinksModuleData,
    rotatingBarcode :: Maybe RotatingBarcode
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data TicketLeg = TicketLeg
  { originName :: Name,
    destinationName :: Name,
    originStationGmmLocationId :: Text,
    destinationStationGmmLocationId :: Text
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
    value :: Text
  }
  deriving (Show, Generic)

instance FromJSON Barcode where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Barcode where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data GroupingInfo = GroupingInfo
  { groupingId :: Text,
    sortIndex :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ServiceAccount = ServiceAccount
  { saPrivateKeyId :: !T.Text,
    saClientEmail :: !T.Text,
    saTokenUri :: !T.Text,
    saIssuerId :: !T.Text
  }
  deriving (Show, Eq, Generic)

newtype TransitObjectPatch = TransitObjectPatch
  { state :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data TextModule = TextModule
  { _header :: Text,
    body :: Text,
    id :: Text
  }
  deriving (Show, Generic)

data URI = URI
  { uri :: Maybe Text,
    description :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype LinksModuleData = LinksModuleData
  { uris :: [URI]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance FromJSON TextModule where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TextModule where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data TimeInterval = TimeInterval
  { start :: DateTime,
    end :: DateTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype DateTime = DateTime
  { date :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data RotatingBarcode = RotatingBarcode
  { _type :: Text,
    renderEncoding :: Text,
    valuePattern :: Text,
    totpDetails :: TOTPDetails,
    alternateText :: Text
  }
  deriving (Show, Generic)

instance FromJSON RotatingBarcode where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON RotatingBarcode where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data TOTPDetails = TOTPDetails
  { algorithm :: Text,
    periodMillis :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

createAdditionalClaims :: [(T.Text, Value)] -> ClaimsMap
createAdditionalClaims additionalClaims = ClaimsMap $ Map.fromList additionalClaims

createJWT' :: JOSEHeader -> JWTClaimsSet -> String -> IO (Either String (JWTClaimsSet, T.Text))
createJWT' jwtHeader claims privateKey = do
  let mkey = readRsaSecret . C8.pack $ privateKey
  case mkey of
    Nothing -> pure $ Left "Bad RSA key!"
    Just pkey -> do
      let key = EncodeRSAPrivateKey pkey
      pure $ Right (claims, encodeSigned key jwtHeader claims)

getJwtToken :: JOSEHeader -> JWTClaimsSet -> ServiceAccount -> String -> IO (Either String JWToken)
getJwtToken jwtHeader claims sa privateKey = do
  jwtPair <- createJWT' jwtHeader claims privateKey
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
